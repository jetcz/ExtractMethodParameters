using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.FindSymbols;
using Microsoft.CodeAnalysis.Formatting;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace ExtractMethodParameters
{
    /// <summary>
    /// Extract method parameters into new type
    /// </summary>
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(ExtractMethodParametersCodeRefactoringProvider)), Shared]
    internal partial class ExtractMethodParametersCodeRefactoringProvider : CodeRefactoringProvider
    {
        public sealed override async Task ComputeRefactoringsAsync(CodeRefactoringContext context)
        {
            //exit when user selected less than 3 chars
            if (context.Span.Length < 3)
                return;

            SyntaxNode root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            // Find the node at the selection.
            SyntaxNode node = root.FindNode(context.Span);

            // Only offer a refactoring if the selected node is a type ParameterListSyntax
            if (!(node is ParameterListSyntax parameterListSyntax))
                return;

            MethodDeclarationSyntax methodDeclaration = node.AncestorsAndSelf()
                .OfType<MethodDeclarationSyntax>()
                .FirstOrDefault();

            // Find the parameter nodes that intersect with the selected text
            ImmutableHashSet<ParameterSyntax> parameterNodes = methodDeclaration.ParameterList.Parameters
                .Where(p => p.Span.IntersectsWith(context.Span))
                .Where(p => !p.Modifiers.Any(m => m.IsKind(SyntaxKind.OutKeyword) || m.IsKind(SyntaxKind.ParamsKeyword))) //skip out and params parameters
                .ToImmutableHashSet();

            //exit when user selected less than 2 params
            if (parameterNodes.Count < 2)
                return;

            CustomCodeAction action = CustomCodeAction.Create("Extract method parameters", (c, isPreview) =>
                Process(context.Document, methodDeclaration.Identifier, parameterNodes, c, isPreview)
            );

            context.RegisterRefactoring(action);
        }

        bool _isPreview;
        DocumentId _documentId;
        SyntaxToken _methodIdentifier;
        ClassDeclarationSyntax _classDeclaration;
        IEnumerable<ReferencedSymbol> _allReferences;

        /// <summary>
        /// Do the magic
        /// </summary>
        /// <param name="document"></param>
        /// <param name="methodIdentifier"></param>
        /// <param name="parameterNodes">selected parameters</param>
        /// <param name="cancellationToken"></param>
        /// <param name="isPreview">true if we are in preview (just the current document), false for the actual code modification in whole solution</param>
        /// <returns></returns>
        private async Task<Solution> Process(Document document, SyntaxToken methodIdentifier, ImmutableHashSet<ParameterSyntax> parameterNodes, CancellationToken cancellationToken, bool isPreview)
        {
            _isPreview = isPreview;
            _documentId = document.Id;
            _methodIdentifier = methodIdentifier;

            var newSolution = document.Project.Solution;

            _classDeclaration = await CreateClassDeclarationAsync(newSolution, parameterNodes, cancellationToken);

            newSolution = await ModifyMethodReferencesAsync(newSolution, cancellationToken);

            newSolution = await ModifyMethodDefinitionAsync(newSolution, parameterNodes, cancellationToken);

            //is this needed?
            _allReferences = null;
            _classDeclaration = null;
            _documentId = null;

            return newSolution;
        }


        /// <summary>
        /// Create class declaration with properties
        /// </summary>
        /// <param name="solution"></param>
        /// <param name="parameterNodes"></param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        private async Task<ClassDeclarationSyntax> CreateClassDeclarationAsync(Solution solution, ImmutableHashSet<ParameterSyntax> parameterNodes, CancellationToken cancellationToken)
        {
            Document document = solution.GetDocument(_documentId);

            SyntaxNode root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

            Location methodIdentifierLocation = _methodIdentifier.GetLocation();
            MethodDeclarationSyntax methodSyntax = root.FindNode(methodIdentifierLocation.SourceSpan) as MethodDeclarationSyntax;

            // Get the semantic model for the document
            SemanticModel semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            // Get the trivia for the parameter syntax
            SyntaxTriviaList methodTrivia = methodSyntax.GetLeadingTrivia();

            // Find the XML comment trivia for the myId parameter
            SyntaxTrivia xmlCommentTrivia = methodTrivia.FirstOrDefault(t =>
                t.IsKind(SyntaxKind.SingleLineDocumentationCommentTrivia));

            // Get the current namespace
            NamespaceDeclarationSyntax namespaceSyntax = root.DescendantNodesAndSelf().OfType<NamespaceDeclarationSyntax>().FirstOrDefault();
            INamespaceSymbol currentNamespaceSymbol = semanticModel.GetDeclaredSymbol(namespaceSyntax);

            int i = 0;
            string className = $"{methodSyntax.Identifier}Args";
            while (currentNamespaceSymbol.GetTypeMembers(className).Any())
            {
                i++;
                className = $"{methodSyntax.Identifier}Args{i}";
            }

            string xmlComment = $"/// <summary>\n/// {methodSyntax.Identifier} arguments\n/// </summary>\n";

            Dictionary<string, ExpressionSyntax> defaultValues = _isPreview ? null : await GetDefaultValuesForPropertiesAsync(solution, parameterNodes, cancellationToken);

            IEnumerable<MemberDeclarationSyntax> properties = CreateProperties(parameterNodes, defaultValues, xmlCommentTrivia.ToString());

            return SyntaxFactory.ClassDeclaration(className)
                .WithModifiers(SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
                .WithMembers(SyntaxFactory.List(properties))
                .WithLeadingTrivia(SyntaxFactory.ParseLeadingTrivia(xmlComment));
        }

        /// <summary>
        /// Create class properties with xml comments
        /// </summary>
        /// <param name="parameterNodes"></param>
        /// <param name="defaultValues"></param>
        /// <param name="methodXmlComment"></param>
        /// <returns></returns>
        private IEnumerable<MemberDeclarationSyntax> CreateProperties(ImmutableHashSet<ParameterSyntax> parameterNodes, Dictionary<string, ExpressionSyntax> defaultValues, string methodXmlComment)
        {
            foreach (ParameterSyntax par in parameterNodes)
            {
                //declare the property
                PropertyDeclarationSyntax propertyDeclaration = SyntaxFactory.PropertyDeclaration(par.Type, SyntaxFactory.Identifier(Up1stLetter(par.Identifier.ValueText)))
                    .WithModifiers(SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
                    .WithAccessorList(SyntaxFactory.AccessorList(
                        SyntaxFactory.List(new[]
                        {
                        SyntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration)
                            .WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken)),
                        SyntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration)
                            .WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken))
                        }
                        )));

                //steal the xml comment from method
                int parameterIndex = methodXmlComment.IndexOf($"<param name=\"{par.Identifier.ValueText}\"");
                if (parameterIndex != -1)
                {
                    int commentStartIndex = methodXmlComment.IndexOf(">", parameterIndex) + 1;
                    int commentEndIndex = methodXmlComment.IndexOf($"</param>", parameterIndex);

                    string comment = methodXmlComment.Substring(commentStartIndex, commentEndIndex - commentStartIndex);
                    string xmlComment = $"/// <summary>\n/// {comment}\n/// </summary>\n";

                    propertyDeclaration = propertyDeclaration.WithLeadingTrivia(SyntaxFactory.ParseLeadingTrivia(xmlComment));
                }

                //handle default value
                EqualsValueClauseSyntax init = par.Default; //get the method parameter default
                if (init is null
                    && defaultValues != null
                    && defaultValues.TryGetValue(par.Identifier.ValueText, out ExpressionSyntax defaultValue))
                {
                    init = SyntaxFactory.EqualsValueClause(defaultValue);
                }
                if (init != null)
                {
                    propertyDeclaration = propertyDeclaration
                        .WithInitializer(init)
                        .WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken));
                }

                yield return propertyDeclaration;
            }
        }

        /// <summary>
        /// Analyze solution for the most common parameter values
        /// </summary>
        /// <param name="solution"></param>
        /// <param name="parameterNodes"></param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        private async Task<Dictionary<string, ExpressionSyntax>> GetDefaultValuesForPropertiesAsync(Solution solution, ImmutableHashSet<ParameterSyntax> parameterNodes, CancellationToken cancellationToken)
        {
            //get the method symbol to find references for this method     
            Document document = solution.GetDocument(_documentId);

            SyntaxNode root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

            Location methodIdentifierLocation = _methodIdentifier.GetLocation();
            MethodDeclarationSyntax methodSyntax = root.FindNode(methodIdentifierLocation.SourceSpan) as MethodDeclarationSyntax;

            // Get the semantic model for the syntax node
            SemanticModel semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            // Get the method symbol from the syntax node
            IMethodSymbol methodSymbol = semanticModel.GetDeclaredSymbol(methodSyntax);

            _allReferences = await SymbolFinder.FindReferencesAsync(methodSymbol, solution, cancellationToken).ConfigureAwait(false);

            Dictionary<string, Dictionary<ExpressionSyntax, int>> paramsData = new Dictionary<string, Dictionary<ExpressionSyntax, int>>(parameterNodes.Count);

            foreach (ReferencedSymbol reference in _allReferences)
            {
                IEnumerable<IGrouping<DocumentId, ReferenceLocation>> groupsByDoc = reference.Locations.GroupBy(x => x.Document.Id);

                foreach (IGrouping<DocumentId, ReferenceLocation> groupByDoc in groupsByDoc)
                {
                    if (groupByDoc.Key != document.Id)
                    {
                        document = solution.GetDocument(groupByDoc.Key);

                        root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
                    }

                    foreach (ReferenceLocation location in groupByDoc)
                    {
                        SyntaxNode syntaxNode = root.FindNode(location.Location.SourceSpan);
                        InvocationExpressionSyntax invocation = syntaxNode.AncestorsAndSelf().OfType<InvocationExpressionSyntax>().First();

                        for (int i = 0; i < invocation.ArgumentList.Arguments.Count; i++)
                        {
                            IParameterSymbol parameter = methodSymbol.Parameters[i];
                            ArgumentSyntax argument = invocation.ArgumentList.Arguments[i];

                            if (!parameterNodes.Any(x => x.Identifier.ValueText == parameter.Name))
                            {
                                continue;
                            }

                            if (!paramsData.TryGetValue(parameter.Name, out Dictionary<ExpressionSyntax, int> values))
                            {
                                values = new Dictionary<ExpressionSyntax, int>(new MyExpressionSyntaxComparer());
                                paramsData.Add(parameter.Name, values);
                            }

                            if (values.TryGetValue(argument.Expression, out _))
                            {
                                values[argument.Expression]++;
                            }
                            else
                            {
                                values.Add(argument.Expression, 1);
                            }
                        }
                    }
                }
            }

            Dictionary<string, ExpressionSyntax> retval = new Dictionary<string, ExpressionSyntax>(parameterNodes.Count);

            foreach (KeyValuePair<string, Dictionary<ExpressionSyntax, int>> paramData in paramsData)
            {
                Dictionary<ExpressionSyntax, int> expressionOccurences = paramData.Value;

                ExpressionSyntax expression = expressionOccurences.Where(x => x.Value > 1).OrderByDescending(x => x.Value).FirstOrDefault().Key;

                if (expression != default && !expression.IsKind(SyntaxKind.NullLiteralExpression))
                {
                    retval.Add(paramData.Key, expression);
                }
            }

            return retval;
        }

        /// <summary>
        /// Changes method calls to reflect new method signature
        /// </summary>
        /// <param name="solution"></param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        private async Task<Solution> ModifyMethodReferencesAsync(Solution solution, CancellationToken cancellationToken)
        {
            //get the method symbol to find references for this method     
            Document document = solution.GetDocument(_documentId);

            SyntaxNode root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

            Location methodIdentifierLocation = _methodIdentifier.GetLocation();
            MethodDeclarationSyntax methodSyntax = root.FindNode(methodIdentifierLocation.SourceSpan) as MethodDeclarationSyntax;

            // Get the semantic model for the syntax node
            SemanticModel semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            // Get the method symbol from the syntax node
            IMethodSymbol methodSymbol = semanticModel.GetDeclaredSymbol(methodSyntax);

            Dictionary<string, PropertyDeclarationSyntax> classProps = _classDeclaration.Members.OfType<PropertyDeclarationSyntax>().ToDictionary(prop => prop.Identifier.ValueText, prop => prop);

            IEnumerable<ReferencedSymbol> references;
            if (_isPreview)
            {
                ImmutableHashSet<Document> documents = _isPreview ? new[] { document }.ToImmutableHashSet() : null;
                references = await SymbolFinder.FindReferencesAsync(methodSymbol, solution, documents, cancellationToken).ConfigureAwait(false);
            }
            else if (_allReferences != null)
            {
                references = _allReferences;
            }
            else
            {
                throw new System.Exception(nameof(_allReferences) + " not initialized");
            }

            foreach (ReferencedSymbol reference in references)
            {
                IEnumerable<IGrouping<DocumentId, ReferenceLocation>> groupsByDoc = reference.Locations.GroupBy(x => x.Document.Id);

                foreach (IGrouping<DocumentId, ReferenceLocation> groupByDoc in groupsByDoc)
                {
                    if (groupByDoc.Key != document.Id)
                    {
                        document = solution.GetDocument(groupByDoc.Key);

                        root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
                    }

                    SyntaxEditor editor = new SyntaxEditor(root, solution.Workspace.Services);

                    int variableCount = 0;
                    foreach (ReferenceLocation location in groupByDoc)
                    {
                        SyntaxNode syntaxNode = root.FindNode(location.Location.SourceSpan);
                        InvocationExpressionSyntax invocation = syntaxNode.AncestorsAndSelf().OfType<InvocationExpressionSyntax>().First();

                        // Build initializer expression for the new args object
                        InitializerExpressionSyntax initializerExpression = SyntaxFactory.InitializerExpression(
                            SyntaxKind.ObjectInitializerExpression,
                            SyntaxFactory.SeparatedList<ExpressionSyntax>());

                        List<ArgumentSyntax> newArgsForMethodInvocation = new List<ArgumentSyntax>();
                        for (int i = 0; i < invocation.ArgumentList.Arguments.Count; i++)
                        {
                            IParameterSymbol parameter = methodSymbol.Parameters[i];
                            ArgumentSyntax argument = invocation.ArgumentList.Arguments[i];

                            if (!classProps.TryGetValue(Up1stLetter(parameter.Name), out PropertyDeclarationSyntax prop))
                            {
                                newArgsForMethodInvocation.Add(argument); //keep the method param as is, we do not have it in our new class
                                continue;
                            }

                            //do not initialize property if the property has the same default value as the current method parameter
                            if (prop.Initializer is null && argument.Expression.IsKind(SyntaxKind.NullLiteralExpression)
                                || prop.Initializer?.Value?.ToString() == argument.Expression.ToString())
                            {
                                continue; // skip this property assignment
                            }

                            // Create a syntax node for the property assignment
                            AssignmentExpressionSyntax propertyAssignment = SyntaxFactory.AssignmentExpression(
                                SyntaxKind.SimpleAssignmentExpression,
                                SyntaxFactory.IdentifierName(prop.Identifier),
                                argument.Expression)
                                .WithLeadingTrivia(SyntaxFactory.LineFeed)
                                .WithoutTrailingTrivia();

                            // Add the property assignment to the initializer expression
                            initializerExpression = initializerExpression.AddExpressions(propertyAssignment);
                        }

                        // Create a variable to hold the arguments class instance
                        // scan the code block if there is the same variable already, create unique name
                        string variableName;
                        BlockSyntax block = syntaxNode.AncestorsAndSelf().OfType<BlockSyntax>().First();
                        IEnumerable<VariableDeclaratorSyntax> variables = block.DescendantNodes().OfType<VariableDeclarationSyntax>().SelectMany(x => x.Variables);
                        do
                        {
                            variableName = "args" + (variableCount == 0 ? "" : variableCount.ToString());
                            variableCount++;

                        } while (variables.Any(x => x.Identifier.ValueText == variableName));

                        SyntaxToken argsVar = SyntaxFactory.Identifier(variableName);

                        IdentifierNameSyntax identifierName = SyntaxFactory.IdentifierName(argsVar);
                        ArgumentSyntax identifierArg = SyntaxFactory.Argument(identifierName);

                        newArgsForMethodInvocation.Insert(0, identifierArg);

                        // Replace the method invocation with the new syntax
                        InvocationExpressionSyntax newInvocation = invocation.WithArgumentList(SyntaxFactory.ArgumentList(
                            SyntaxFactory.SeparatedList(newArgsForMethodInvocation)
                            ));

                        //declaration of the variable
                        LocalDeclarationStatementSyntax argsVarDecl = SyntaxFactory.LocalDeclarationStatement(
                            SyntaxFactory.VariableDeclaration(SyntaxFactory.IdentifierName(_classDeclaration.Identifier.Text))
                            .WithVariables(SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.VariableDeclarator(argsVar)
                                .WithInitializer(SyntaxFactory.EqualsValueClause(SyntaxFactory.ObjectCreationExpression(
                                    SyntaxFactory.ParseTypeName(_classDeclaration.Identifier.Text))
                                .WithInitializer(initializerExpression)
                                    .WithNewKeyword(SyntaxFactory.Token(SyntaxKind.NewKeyword)
                                    .WithTrailingTrivia(SyntaxFactory.Space))
                                    .WithArgumentList(SyntaxFactory.ArgumentList()))))));

                        editor.InsertBefore(invocation.Parent, argsVarDecl);

                        editor.ReplaceNode(invocation, newInvocation);
                    }

                    SyntaxNode newRoot = editor.GetChangedRoot();

                    newRoot = Formatter.Format(newRoot, solution.Workspace);

                    solution = solution.WithDocumentSyntaxRoot(document.Id, newRoot);
                }
            }

            return solution;
        }

        /// <summary>
        /// Modify method definition - signature and references to the parameters in the body of the method
        /// </summary>
        /// <param name="parameterNodes"></param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        private async Task<Solution> ModifyMethodDefinitionAsync(Solution solution, ImmutableHashSet<ParameterSyntax> parameterNodes, CancellationToken cancellationToken)
        {
            Document document = solution.GetDocument(_documentId);

            SyntaxNode root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

            Location methodIdentifierLocation = _methodIdentifier.GetLocation();
            MethodDeclarationSyntax methodSyntax = root.FindNode(methodIdentifierLocation.SourceSpan) as MethodDeclarationSyntax;

            //fix method signature

            // Get the list of parameters from the method's syntax node
            ParameterListSyntax oldParameterList = methodSyntax.DescendantNodes().OfType<ParameterListSyntax>().First();

            List<ParameterSyntax> newParameters = oldParameterList.Parameters
                .Where(x => !parameterNodes.Any(y => y.Identifier.ValueText == x.Identifier.ValueText))
                .ToList();

            string parameterName = "args";
            int i = 0;
            while (newParameters.Exists(x => x.Identifier.ValueText == parameterName))
            {
                i++;
                parameterName = $"args{i}";
            }

            // Get the type of the class
            IdentifierNameSyntax className = SyntaxFactory.IdentifierName(_classDeclaration.Identifier);
            TypeSyntax classType = SyntaxFactory.ParseTypeName(className.ToString());
            ParameterSyntax argsParameter = SyntaxFactory.Parameter(SyntaxFactory.Identifier(parameterName))
                .WithType(classType);

            newParameters.Insert(0, argsParameter);

            // Create a new parameter list syntax that contains only the selected parameters
            ParameterListSyntax newParameterList = SyntaxFactory.ParameterList(SyntaxFactory.SeparatedList(newParameters));

            // Replace the old parameter list with the new one in the method's syntax node
            MethodDeclarationSyntax newMethodSyntax = methodSyntax.ReplaceNode(oldParameterList, newParameterList);

            newMethodSyntax = newMethodSyntax.WithParameterList(newParameterList);

            //fix method body

            // Replace all references to the old parameters with references to the new args parameter
            IEnumerable<IdentifierNameSyntax> nodesToReplace = newMethodSyntax.DescendantNodes()
                .OfType<IdentifierNameSyntax>()
                .Where(x => parameterNodes.Any(p => p.Identifier.ValueText == x.Identifier.ValueText));

            BlockSyntax newBody = newMethodSyntax.Body.ReplaceNodes(nodesToReplace,
                (node, _) => SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression
                , SyntaxFactory.IdentifierName(parameterName)
                , SyntaxFactory.IdentifierName(Up1stLetter(node.Identifier.ValueText)))
                    .WithOperatorToken(SyntaxFactory.Token(SyntaxKind.DotToken))
                    .WithLeadingTrivia(node.GetLeadingTrivia())
                    .WithTrailingTrivia(node.GetTrailingTrivia())
                    );

            // Create the new method syntax with the updated body
            newMethodSyntax = newMethodSyntax.WithBody(newBody);

            //add class definition
            NamespaceDeclarationSyntax namespaceSyntax = methodSyntax.FirstAncestorOrSelf<NamespaceDeclarationSyntax>();
            string namespaceName = namespaceSyntax.Name.ToString();

            // Create a new namespace declaration
            NamespaceDeclarationSyntax newNamespace = SyntaxFactory.NamespaceDeclaration(SyntaxFactory.ParseName(namespaceName));

            // Add the new class declaration to the namespace
            newNamespace = newNamespace.AddMembers(_classDeclaration);

            SyntaxEditor editor = new SyntaxEditor(root, solution.Workspace.Services);

            editor.ReplaceNode(methodSyntax, newMethodSyntax);
            editor.InsertAfter(root.ChildNodes().Last(), newNamespace);

            SyntaxNode newRoot = editor.GetChangedRoot();

            newRoot = Formatter.Format(newRoot, solution.Workspace);

            return solution.WithDocumentSyntaxRoot(document.Id, newRoot);
        }

        /// <summary>
        /// Capitalize first letter of the string
        /// </summary>
        /// <param name="text"></param>
        /// <returns></returns>
        private static string Up1stLetter(string text) => text.Substring(0, 1).ToUpper() + text.Substring(1);

    }
}
