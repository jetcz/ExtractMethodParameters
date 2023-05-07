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

//todo check named paramters
//todo paramters with default values

namespace ExtractMethodParameters
{
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(ExtractMethodParametersCodeRefactoringProvider)), Shared]
    internal class ExtractMethodParametersCodeRefactoringProvider : CodeRefactoringProvider
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
            List<ParameterSyntax> parameterNodes = methodDeclaration.ParameterList.Parameters
                .Where(p => p.Span.IntersectsWith(context.Span)
                        && !p.Modifiers.Any(m => m.IsKind(SyntaxKind.OutKeyword))) //skip out parameters
                .ToList();

            if (parameterNodes.Count == 0)
                return;

            _methodIdentifier = methodDeclaration.Identifier;
            _documentId = context.Document.Id;

            CustomCodeAction action = CustomCodeAction.Create("Extract method parameters", (c, isPreview) => ExtractParameters(context.Document.Project.Solution, parameterNodes, c, isPreview));

            context.RegisterRefactoring(action);
        }

        bool _isSang;
        bool _isPreview;
        DocumentId _documentId;
        SyntaxToken _methodIdentifier;
        Solution _solution;

        private async Task<Solution> ExtractParameters(Solution solution, List<ParameterSyntax> parameterNodes, CancellationToken cancellationToken, bool isPreview)
        {
            _solution = solution;
            _isPreview = isPreview;
            _isSang = true;// string.Equals(Path.GetFileNameWithoutExtension(_solution.FilePath), "SangSolution", StringComparison.OrdinalIgnoreCase);

            ClassDeclarationSyntax classDeclaration = await CreateClassDeclarationAsync(parameterNodes, cancellationToken);

            await FixMethodReferencesAsync(classDeclaration, cancellationToken);

            await FixMethodDeclarationAsync(classDeclaration, parameterNodes, cancellationToken);

            return _solution;
        }

        private async Task<ClassDeclarationSyntax> CreateClassDeclarationAsync(List<ParameterSyntax> parameterNodes, CancellationToken cancellationToken)
        {
            Document document = _solution.GetDocument(_documentId);

            SyntaxNode root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

            Location methodIdentifierLocation = _methodIdentifier.GetLocation();
            MethodDeclarationSyntax methodSyntax = root.FindNode(methodIdentifierLocation.SourceSpan) as MethodDeclarationSyntax;

            // Get the semantic model for the document
            SemanticModel semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

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

            return SyntaxFactory.ClassDeclaration(className)
                .WithModifiers(SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
                .WithMembers(SyntaxFactory.List(CreateProperties(parameterNodes)));
        }

        private IEnumerable<MemberDeclarationSyntax> CreateProperties(List<ParameterSyntax> parameterNodes)
        {
            EqualsValueClauseSyntax emptyString = SyntaxFactory.EqualsValueClause(SyntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression, SyntaxFactory.Literal("")));
            EqualsValueClauseSyntax dbIgnoredID = SyntaxFactory.EqualsValueClause(SyntaxFactory.IdentifierName("DB.IgnoredID"));
            EqualsValueClauseSyntax dbIgnoredDate = SyntaxFactory.EqualsValueClause(SyntaxFactory.IdentifierName("DB.IgnoredDate"));

            foreach (ParameterSyntax par in parameterNodes)
            {
                PropertyDeclarationSyntax propertyDeclaration = SyntaxFactory.PropertyDeclaration(
                    par.Type,
                    SyntaxFactory.Identifier(par.Identifier.ValueText))
                    .WithModifiers(SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
                    .WithAccessorList(SyntaxFactory.AccessorList(
                        SyntaxFactory.List(new[]
                        {
                    SyntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration)
                        .WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken)),
                    SyntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration)
                        .WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken))
                        })));

                if (_isSang)
                {
                    EqualsValueClauseSyntax init = null;

                    switch (par.Type.Kind())
                    {
                        case SyntaxKind.PredefinedType:
                            switch (((PredefinedTypeSyntax)par.Type).Keyword.Kind())
                            {
                                case SyntaxKind.StringKeyword:
                                    init = emptyString;
                                    break;
                                case SyntaxKind.IntKeyword:
                                    init = dbIgnoredID;
                                    break;
                            }
                            break;
                        case SyntaxKind.IdentifierName:
                            switch (((IdentifierNameSyntax)par.Type).Identifier.ValueText.ToLower())
                            {
                                case "datetime":
                                    init = dbIgnoredDate;
                                    break;
                            }
                            break;
                    }

                    if (init != null)
                    {
                        propertyDeclaration = propertyDeclaration.WithInitializer(init).WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken));
                    }
                }

                yield return propertyDeclaration;
            }
        }

        /// <summary>
        /// Changes method calls to reflect new method signature
        /// This has to be called before modifying the method signature otherwise we would be able to find the references
        /// </summary>
        /// <param name="methodSymbol"></param>
        /// <param name="classDeclaration">args</param>
        private async Task FixMethodReferencesAsync(ClassDeclarationSyntax classDeclaration, CancellationToken cancellationToken)
        {
            //get the method symbol to find references for this method     
            Document document = _solution.GetDocument(_documentId);

            SyntaxNode root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

            Location methodIdentifierLocation = _methodIdentifier.GetLocation();
            MethodDeclarationSyntax methodSyntax = root.FindNode(methodIdentifierLocation.SourceSpan) as MethodDeclarationSyntax;

            // Get the semantic model for the syntax node
            SemanticModel semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            // Get the method symbol from the syntax node
            IMethodSymbol methodSymbol = semanticModel.GetDeclaredSymbol(methodSyntax);

            Dictionary<string, PropertyDeclarationSyntax> classProps = classDeclaration.Members.OfType<PropertyDeclarationSyntax>().ToDictionary(prop => prop.Identifier.ValueText, prop => prop);

            ImmutableHashSet<Document> documentsToBeSearched = _isPreview ? new[] { document }.ToImmutableHashSet() : null; //in preview mode, just work with the current document, otherwise the whole solution

            IEnumerable<ReferencedSymbol> references = await SymbolFinder.FindReferencesAsync(methodSymbol, _solution, documentsToBeSearched, cancellationToken).ConfigureAwait(false);
            foreach (ReferencedSymbol reference in references)
            {
                IEnumerable<IGrouping<DocumentId, ReferenceLocation>> groupsByDoc = reference.Locations.GroupBy(x => x.Document.Id);

                foreach (IGrouping<DocumentId, ReferenceLocation> groupByDoc in groupsByDoc)
                {
                    if (groupByDoc.Key != document.Id)
                    {
                        document = _solution.GetDocument(groupByDoc.Key);

                        root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
                    }

                    SyntaxEditor editor = new SyntaxEditor(root, _solution.Workspace.Services);

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

                            if (!classProps.TryGetValue(parameter.Name, out PropertyDeclarationSyntax prop))
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
                                SyntaxFactory.IdentifierName(parameter.Name),
                                argument.Expression)
                                .WithLeadingTrivia(SyntaxFactory.LineFeed)
                                .WithoutTrailingTrivia();

                            // Add the property assignment to the initializer expression
                            initializerExpression = initializerExpression.AddExpressions(propertyAssignment);
                        }

                        // Create a variable to hold the arguments class instance
                        string variableName = "args" + (variableCount == 0 ? "" : variableCount.ToString()); //it would be better to search the current block for occurences of this variable name but hey

                        SyntaxToken argsVar = SyntaxFactory.Identifier(variableName);

                        LocalDeclarationStatementSyntax argsVarDecl = SyntaxFactory.LocalDeclarationStatement(
                            SyntaxFactory.VariableDeclaration(SyntaxFactory.IdentifierName(classDeclaration.Identifier.Text))
                            .WithVariables(SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.VariableDeclarator(argsVar)
                                .WithInitializer(SyntaxFactory.EqualsValueClause(SyntaxFactory.ObjectCreationExpression(
                                    SyntaxFactory.ParseTypeName(classDeclaration.Identifier.Text))
                                .WithInitializer(initializerExpression)
                                    .WithNewKeyword(SyntaxFactory.Token(SyntaxKind.NewKeyword)
                                    .WithTrailingTrivia(SyntaxFactory.Space))
                                    .WithArgumentList(SyntaxFactory.ArgumentList()))))));

                        IdentifierNameSyntax identifierName = SyntaxFactory.IdentifierName(argsVar);
                        ArgumentSyntax identifierArg = SyntaxFactory.Argument(identifierName);

                        newArgsForMethodInvocation.Insert(0, identifierArg);

                        // Replace the method invocation with the new syntax
                        InvocationExpressionSyntax newInvocation = invocation.WithArgumentList(SyntaxFactory.ArgumentList(
                            SyntaxFactory.SeparatedList(newArgsForMethodInvocation)
                            ));

                        //finally change the root
                        editor.InsertBefore(invocation.Parent, argsVarDecl);
                        editor.ReplaceNode(invocation, newInvocation);

                        variableCount++;
                    }

                    SyntaxNode newRoot = editor.GetChangedRoot();

                    newRoot = Formatter.Format(newRoot, _solution.Workspace);

                    _solution = _solution.WithDocumentSyntaxRoot(document.Id, newRoot);
                }
            }
        }

        private async Task FixMethodDeclarationAsync(ClassDeclarationSyntax classDeclaration, List<ParameterSyntax> parameterNodes, CancellationToken cancellationToken)
        {
            Document document = _solution.GetDocument(_documentId);

            SyntaxNode root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

            Location methodIdentifierLocation = _methodIdentifier.GetLocation();
            MethodDeclarationSyntax methodSyntax = root.FindNode(methodIdentifierLocation.SourceSpan) as MethodDeclarationSyntax;

            //fix signature

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
            IdentifierNameSyntax className = SyntaxFactory.IdentifierName(classDeclaration.Identifier);
            TypeSyntax classType = SyntaxFactory.ParseTypeName(className.ToString());
            ParameterSyntax argsParameter = SyntaxFactory.Parameter(SyntaxFactory.Identifier(parameterName))
                .WithType(classType);

            newParameters.Insert(0, argsParameter);

            // Create a new parameter list syntax that contains only the selected parameters
            ParameterListSyntax newParameterList = SyntaxFactory.ParameterList(SyntaxFactory.SeparatedList(newParameters));

            // Replace the old parameter list with the new one in the method's syntax node
            MethodDeclarationSyntax newMethodSyntax = methodSyntax.ReplaceNode(oldParameterList, newParameterList);

            newMethodSyntax = newMethodSyntax.WithParameterList(newParameterList);

            //fix body

            // Replace all references to the old parameters with references to the new args parameter
            IEnumerable<IdentifierNameSyntax> nodesToReplace = newMethodSyntax.DescendantNodes()
                .OfType<IdentifierNameSyntax>()
                .Where(identifier => parameterNodes.Any(parameterIdentifier => parameterIdentifier.Identifier.ValueText == identifier.Identifier.ValueText));

            BlockSyntax newBody = newMethodSyntax.Body.ReplaceNodes(nodesToReplace,
                (node, _) => SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, SyntaxFactory.IdentifierName(parameterName), SyntaxFactory.IdentifierName(node.Identifier.ValueText))
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
            newNamespace = newNamespace.AddMembers(classDeclaration);

            SyntaxEditor editor = new SyntaxEditor(root, _solution.Workspace.Services);

            editor.ReplaceNode(methodSyntax, newMethodSyntax);
            editor.InsertAfter(root.ChildNodes().Last(), newNamespace);

            SyntaxNode newRoot = editor.GetChangedRoot();

            newRoot = Formatter.Format(newRoot, _solution.Workspace);

            _solution = _solution.WithDocumentSyntaxRoot(document.Id, newRoot);
        }

    }
}
