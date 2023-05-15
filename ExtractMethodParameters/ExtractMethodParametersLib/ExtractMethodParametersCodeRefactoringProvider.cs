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
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;

namespace ExtractMethodParametersLib
{
    /// <summary>
    /// Extract method parameters into new type
    /// </summary>
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(ExtractMethodParametersCodeRefactoringProvider)), Shared]
    internal partial class ExtractMethodParametersCodeRefactoringProvider : CodeRefactoringProvider
    {
        private bool _isPreview;
        private DocumentId _documentId;
        private SyntaxToken _methodIdentifier;
        private IEnumerable<ReferencedSymbol> _allReferences;
        private ExpressionSyntaxComparer _expressionSyntaxComparer;

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

            if (methodDeclaration is null)
                return;

            var parameterNodes = (from p in methodDeclaration.ParameterList.Parameters
                              
                                  let hasGenericParam = (p.Type as GenericNameSyntax)?.TypeArgumentList?.Arguments //skip generics such as List<T> etc
                                                        .OfType<IdentifierNameSyntax>()
                                                        .Any(typeArg => methodDeclaration.TypeParameterList?.Parameters
                                                            .Any(typeParam => typeParam.Identifier.ValueText == typeArg.Identifier.ValueText) == true) == true

                                  where p.Span.IntersectsWith(context.Span) //get just selected text
                                  where !p.Modifiers.Any(m => m.IsKind(SyntaxKind.OutKeyword) || m.IsKind(SyntaxKind.ParamsKeyword)) //skip out and params
                                  where !hasGenericParam

                                  select p)
                                  .ToList();

            //exit when user selected less than 2 params
            if (parameterNodes.Count < 2)
                return;

            CustomCodeAction action = CustomCodeAction.Create("Extract method parameters", (cancellationToken, isPreview) =>
                Process(isPreview, context.Document, methodDeclaration.Identifier, parameterNodes, cancellationToken)
            );

            context.RegisterRefactoring(action);
        }

        /// <summary>
        /// Do the magic
        /// </summary>
        /// <param name="isPreview">true if we are in preview (just the current document), false for the actual code modification in whole solution</param>
        /// <param name="document"></param>
        /// <param name="methodIdentifier"></param>
        /// <param name="parameterNodes">selected parameters</param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        private async Task<Solution> Process(bool isPreview, Document document, SyntaxToken methodIdentifier, List<ParameterSyntax> parameterNodes
            , CancellationToken cancellationToken)
        {
            Stopwatch sw = Stopwatch.StartNew();

            _isPreview = isPreview;
            _documentId = document.Id;
            _methodIdentifier = methodIdentifier;
            _expressionSyntaxComparer = new ExpressionSyntaxComparer();

            Solution newSolution = document.Project.Solution;

            List<ParameterSyntax> parameters = CreatePropertyNames(newSolution, parameterNodes).ToList();

            ClassDeclarationSyntax classDeclaration = await CreateClassDeclarationAsync(newSolution, parameters, cancellationToken);

            newSolution = await ModifyMethodReferencesAsync(newSolution, classDeclaration, cancellationToken);

            newSolution = await ModifyMethodDefinitionAsync(newSolution, parameters, classDeclaration, cancellationToken);

            //is this needed?
            _allReferences = null;

            Debug.WriteLine($"{nameof(ExtractMethodParametersCodeRefactoringProvider)} isPreview={_isPreview} elapsed={sw.Elapsed}");

            return newSolution;
        }

        /// <summary>
        /// Process collection of parameters and create the name for our new properties and store it to custom annotation of the PArameterSyntax
        /// It makes sure the names have a naming convention and are unique
        /// </summary>
        /// <param name="solution"></param>
        /// <param name="parameterNodes"></param>
        /// <returns></returns>
        private IEnumerable<ParameterSyntax> CreatePropertyNames(Solution solution, List<ParameterSyntax> parameterNodes)
        {
            string solutionName = Path.GetFileNameWithoutExtension(solution.FilePath);

            IPropertyNameNormalizer propertyNameNormalizer = PropertyNormalizerFactory.CreateNormalizer(solutionName);

            IEnumerable<IGrouping<string, ParameterSyntax>> groups = parameterNodes.GroupBy(x => propertyNameNormalizer.Normalize(x.Identifier.ValueText));

            foreach (IGrouping<string, ParameterSyntax> group in groups)
            {
                ParameterSyntax[] parameters = group.ToArray();
                string propertyName = group.Key;

                for (int i = 0; i < parameters.Length; i++)
                {
                    string ordinal = i == 0 ? "" : (i + 1).ToString();

                    yield return parameters[i].WithAdditionalAnnotations(new SyntaxAnnotation(AnnotationKind.PropName.ToString(), $"{propertyName}{ordinal}"));
                }
            }
        }


        /// <summary>
        /// Create class declaration with properties
        /// </summary>
        /// <param name="solution"></param>
        /// <param name="parameterNodes"></param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        private async Task<ClassDeclarationSyntax> CreateClassDeclarationAsync(Solution solution, List<ParameterSyntax> parameterNodes
            , CancellationToken cancellationToken)
        {
            Document document = solution.GetDocument(_documentId);

            SyntaxNode root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

            Location methodIdentifierLocation = _methodIdentifier.GetLocation();
            MethodDeclarationSyntax methodSyntax = root.FindNode(methodIdentifierLocation.SourceSpan) as MethodDeclarationSyntax;

            // Get the semantic model for the document
            SemanticModel semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            ClassDeclarationSyntax enclosingClass = methodSyntax.Ancestors().OfType<ClassDeclarationSyntax>().First();
            INamedTypeSymbol enclosingClassSymbol = semanticModel.GetDeclaredSymbol(enclosingClass);

            // create class name       
            string uniqueMyClassName;
            {
                string name = $"{methodSyntax.Identifier}Args";
                int i = 0;
                uniqueMyClassName = name;
                while (enclosingClassSymbol.GetTypeMembers(uniqueMyClassName).Any())
                {
                    i++;
                    uniqueMyClassName = $"{name}{i}";
                }
            }

            //do not try to figure out default property initializers in preview mode
            Dictionary<string, ExpressionSyntax> defaultValues = _isPreview ? null : await GetDefaultValuesForPropertiesAsync(solution, parameterNodes, cancellationToken);

            // Find the XML comment trivia for the method
            SyntaxTrivia xmlCommentTrivia = methodSyntax.GetLeadingTrivia().FirstOrDefault(t => t.IsKind(SyntaxKind.SingleLineDocumentationCommentTrivia));

            IEnumerable<MemberDeclarationSyntax> properties = CreateProperties(parameterNodes, defaultValues, xmlCommentTrivia.ToString());

            string xmlComment = $"/// <summary>\n/// {methodSyntax.Identifier} arguments\n/// </summary>\n";

            return SyntaxFactory.ClassDeclaration(uniqueMyClassName)
                .WithModifiers(SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
                .WithMembers(SyntaxFactory.List(properties))
                .WithLeadingTrivia(SyntaxFactory.ParseLeadingTrivia(xmlComment))
                .WithAdditionalAnnotations(new SyntaxAnnotation(AnnotationKind.EnclosingType.ToString(), enclosingClassSymbol.Name));
        }

        /// <summary>
        /// Create class properties with xml comments
        /// </summary>
        /// <param name="parameterNodes"></param>
        /// <param name="defaultValues"></param>
        /// <param name="methodXmlComment"></param>
        /// <returns></returns>
        private IEnumerable<MemberDeclarationSyntax> CreateProperties(List<ParameterSyntax> parameterNodes, Dictionary<string, ExpressionSyntax> defaultValues, string methodXmlComment)
        {
            foreach (ParameterSyntax par in parameterNodes)
            {
                //declare the property
                SyntaxToken identifier = SyntaxFactory.Identifier(par.GetAnnotations(AnnotationKind.PropName.ToString()).First().Data);

                PropertyDeclarationSyntax propertyDeclaration = SyntaxFactory.PropertyDeclaration(par.Type, identifier)
                    .WithModifiers(SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
                    .WithAccessorList(SyntaxFactory.AccessorList(
                        SyntaxFactory.List(new[]
                        {
                        SyntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration)
                            .WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken)),
                        SyntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration)
                            .WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken))
                        }
                        )))
                    .WithAdditionalAnnotations(new SyntaxAnnotation(AnnotationKind.ParamName.ToString(), par.Identifier.ValueText));

                #region xml comments for properties (steal them from the method xml comment)

                int parameterIndex = methodXmlComment.IndexOf($"<param name=\"{par.Identifier.ValueText}\"");
                if (parameterIndex != -1)
                {
                    int commentStartIndex = methodXmlComment.IndexOf(">", parameterIndex) + 1;
                    int commentEndIndex = methodXmlComment.IndexOf($"</param>", parameterIndex);

                    string comment = methodXmlComment.Substring(commentStartIndex, commentEndIndex - commentStartIndex);
                    string xmlComment = $"/// <summary>\n/// {comment}\n/// </summary>\n";

                    propertyDeclaration = propertyDeclaration.WithLeadingTrivia(SyntaxFactory.ParseLeadingTrivia(xmlComment));
                }

                #endregion

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
        private async Task<Dictionary<string, ExpressionSyntax>> GetDefaultValuesForPropertiesAsync(Solution solution, List<ParameterSyntax> parameterNodes
            , CancellationToken cancellationToken)
        {
            //get the method symbol to find references for this method     
            Document document = solution.GetDocument(_documentId);

            SyntaxNode root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

            Location srcMethodIdentifierLocation = _methodIdentifier.GetLocation();
            MethodDeclarationSyntax srcMethodSyntax = root.FindNode(srcMethodIdentifierLocation.SourceSpan) as MethodDeclarationSyntax;

            // Get the semantic model for the syntax node
            SemanticModel semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            // Get the method symbol from the syntax node
            IMethodSymbol srcMethodSymbol = semanticModel.GetDeclaredSymbol(srcMethodSyntax);

            _allReferences = await SymbolFinder.FindReferencesAsync(srcMethodSymbol, solution, cancellationToken).ConfigureAwait(false);

            Dictionary<string, Dictionary<ExpressionSyntax, int>> paramsData = new Dictionary<string, Dictionary<ExpressionSyntax, int>>(parameterNodes.Count);

            Dictionary<ExpressionSyntax, bool> usableAsDefaultExpressions = new Dictionary<ExpressionSyntax, bool>(_expressionSyntaxComparer);

            //go through all references
            foreach (ReferencedSymbol reference in _allReferences)
            {
                IEnumerable<IGrouping<DocumentId, ReferenceLocation>> groupsByDoc = reference.Locations.GroupBy(x => x.Document.Id);

                //group by document so that we do not load syntax tree for the same document repeatedly
                foreach (IGrouping<DocumentId, ReferenceLocation> groupByDoc in groupsByDoc)
                {
                    semanticModel = null;

                    if (groupByDoc.Key != document.Id)
                    {
                        document = solution.GetDocument(groupByDoc.Key);

                        root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
                    }

                    //go through each refernce in scope of a single document
                    foreach (ReferenceLocation location in groupByDoc)
                    {
                        SyntaxNode syntaxNode = root.FindNode(location.Location.SourceSpan);
                        InvocationExpressionSyntax invocation = syntaxNode.AncestorsAndSelf().OfType<InvocationExpressionSyntax>().FirstOrDefault();

                        //invocation might be null if the method is referenced e.g. in xml comment as <see cref...
                        if (invocation is null)
                        {
                            continue;
                        }

                        //go throught all arguments
                        for (int i = 0; i < invocation.ArgumentList.Arguments.Count; i++)
                        {
                            ArgumentSyntax argument = invocation.ArgumentList.Arguments[i];

                            //get parameters either by index or name (if the method call has named paramters)
                            IParameterSymbol parameter = argument.NameColon is null ?
                                srcMethodSymbol.Parameters[i]
                                : srcMethodSymbol.Parameters.First(p => p.Name == argument.NameColon.Name.Identifier.ValueText);

                            if (!parameterNodes.Any(x => x.Identifier.ValueText == parameter.Name))
                            {
                                continue;
                            }

                            //determine if the argument value can be used as default property initializer, ie. it is some literal, or static member accessor
                            bool canBeUsedAsDefaultInitializer = false;

                            #region can this argument be used as default property initializer?

                            if (argument.Expression is LiteralExpressionSyntax)
                            {
                                canBeUsedAsDefaultInitializer = true;
                            }
                            else if (argument.Expression is ObjectCreationExpressionSyntax objectCreation)
                            {
                                //this is brave
                                //check if it is simple object creation without constructor parameters or initializer, this is so that we can initialize new List<int>() for example
                                canBeUsedAsDefaultInitializer = (objectCreation.ArgumentList is null || objectCreation.ArgumentList?.Arguments.Count == 0)
                                                                        && objectCreation.Initializer is null;
                            }
                            else if (!usableAsDefaultExpressions.TryGetValue(argument.Expression, out canBeUsedAsDefaultInitializer)) //check cached expressions first for performance
                            {                            
                                //following checks require srcSemanticModel
                                if (semanticModel is null)
                                {
                                    semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
                                }

                                //check if the argument has constant value (enums, string, number...)
                                if (!canBeUsedAsDefaultInitializer)
                                {
                                    Optional<object> constantValue = semanticModel.GetConstantValue(argument.Expression, cancellationToken);

                                    canBeUsedAsDefaultInitializer = constantValue.HasValue;
                                }

                                //check if static                                
                                if (!canBeUsedAsDefaultInitializer)
                                {
                                    ISymbol symbol = semanticModel.GetSymbolInfo(argument.Expression, cancellationToken).Symbol
                                        ?? semanticModel.GetSymbolInfo(argument, cancellationToken).Symbol; //?

                                    if (symbol?.IsStatic ?? false)
                                    {
                                        switch (symbol)
                                        {
                                            case IMethodSymbol staticMethodSymbol:
                                                canBeUsedAsDefaultInitializer = staticMethodSymbol.Parameters.Length == 0;
                                                //actually the method params could be also static fields or props but we dont need to go that deep... this is good enough
                                                break;
                                            case IFieldSymbol staticFieldSymbol:
                                            case IPropertySymbol staticPropertySymbol:
                                                //there is potential problem when the field or prop is not fully qualified in the method argument, but we would need the full qualification in the new class prop initializer
                                                //but it is easily manually fixable aftewards so we shall not worry
                                                canBeUsedAsDefaultInitializer = true;
                                                break;
                                            default:
                                                canBeUsedAsDefaultInitializer = false; //?
                                                break;
                                        }
                                    }

                                    usableAsDefaultExpressions.Add(argument.Expression, canBeUsedAsDefaultInitializer);
                                }
                            }

                            #endregion

                            if (!canBeUsedAsDefaultInitializer)
                            {
                                continue;
                            }

                            //build knowledgebase about frequency of each parameter value
                            if (!paramsData.TryGetValue(parameter.Name, out Dictionary<ExpressionSyntax, int> values))
                            {
                                values = new Dictionary<ExpressionSyntax, int>(_expressionSyntaxComparer);
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

            //pick the most common parameter values
            Dictionary<string, ExpressionSyntax> retval = new Dictionary<string, ExpressionSyntax>(parameterNodes.Count);

            foreach (KeyValuePair<string, Dictionary<ExpressionSyntax, int>> paramData in paramsData)
            {
                Dictionary<ExpressionSyntax, int> expressionOccurences = paramData.Value;

                ExpressionSyntax expression = expressionOccurences.OrderByDescending(x => x.Value).FirstOrDefault().Key;

                if (expression != null && !expression.IsKind(SyntaxKind.NullLiteralExpression))
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
        /// <param name="classDeclaration"></param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        /// <exception cref="System.Exception"></exception>
        private async Task<Solution> ModifyMethodReferencesAsync(Solution solution, ClassDeclarationSyntax classDeclaration
            , CancellationToken cancellationToken)
        {
            //get the method symbol to find references for this method     
            Document document = solution.GetDocument(_documentId);

            SyntaxNode root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

            Location srcMethodIdentifierLocation = _methodIdentifier.GetLocation();
            MethodDeclarationSyntax srcMethodSyntax = root.FindNode(srcMethodIdentifierLocation.SourceSpan) as MethodDeclarationSyntax;

            // Get the semantic model for the syntax node
            SemanticModel srcSemanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            // Get the method symbol from the syntax node
            IMethodSymbol srcMethodSymbol = srcSemanticModel.GetDeclaredSymbol(srcMethodSyntax);

            Dictionary<string, PropertyDeclarationSyntax> classProps = classDeclaration.Members.OfType<PropertyDeclarationSyntax>()
                .ToDictionary(prop => prop.GetAnnotations(AnnotationKind.ParamName.ToString()).First().Data, prop => prop);

            IEnumerable<ReferencedSymbol> references;
            if (_isPreview)
            {
                ImmutableHashSet<Document> documents = _isPreview ? new[] { document }.ToImmutableHashSet() : null;
                references = await SymbolFinder.FindReferencesAsync(srcMethodSymbol, solution, documents, cancellationToken).ConfigureAwait(false);
            }
            else if (_allReferences != null)
            {
                references = _allReferences;
            }
            else
            {
                throw new System.Exception($"{nameof(_allReferences)} not initialized");
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
                        InvocationExpressionSyntax invocation = syntaxNode.AncestorsAndSelf().OfType<InvocationExpressionSyntax>().FirstOrDefault();

                        //invocation might be null if the method is referenced e.g. in xml comment as <see cref...
                        if (invocation is null)
                        {
                            continue;
                        }

                        #region build class body initializer

                        // Build initializer expression for the new args object
                        InitializerExpressionSyntax initializerExpression = SyntaxFactory.InitializerExpression(
                            SyntaxKind.ObjectInitializerExpression,
                            SyntaxFactory.SeparatedList<ExpressionSyntax>());

                        List<ArgumentSyntax> newArgsForMethodInvocation = new List<ArgumentSyntax>();
                        for (int i = 0; i < invocation.ArgumentList.Arguments.Count; i++)
                        {
                            ArgumentSyntax argument = invocation.ArgumentList.Arguments[i];

                            //get parameters either by index or name (if the method call has named paramters)
                            IParameterSymbol parameter = argument.NameColon is null ?
                                srcMethodSymbol.Parameters[i]
                                : srcMethodSymbol.Parameters.First(p => p.Name == argument.NameColon.Name.Identifier.ValueText);

                            if (!classProps.TryGetValue(parameter.Name, out PropertyDeclarationSyntax prop))
                            {
                                newArgsForMethodInvocation.Add(argument); //keep the method param as is, we do not have it in our new class
                                continue;
                            }

                            //do not initialize property if the property has the same default value as the current method parameter
                            if (_expressionSyntaxComparer.EqualsForInit(prop.Initializer?.Value, argument.Expression))
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

                        #endregion

                        //find a block syntax where we put out new variable, we insert int before the target which should be expression syntax                
                        SyntaxNode target = invocation.Parent;
                        while (!(target.Parent is BlockSyntax))
                        {
                            target = target.Parent;
                        }
                        BlockSyntax block = target.Parent as BlockSyntax;

                        // scan the code block if there is the same variable already, create unique name
                        IEnumerable<VariableDeclaratorSyntax> variables = block.DescendantNodes().OfType<VariableDeclarationSyntax>().SelectMany(x => x.Variables);

                        // create class name       
                        string uniqueVariableName;
                        do
                        {
                            string ordinal = variableCount == 0 ? "" : variableCount.ToString();
                            uniqueVariableName = $"args{ordinal}";
                            variableCount++;

                        } while (variables.Any(x => x.Identifier.ValueText == uniqueVariableName));

                        SyntaxToken argsVar = SyntaxFactory.Identifier(uniqueVariableName);

                        newArgsForMethodInvocation.Insert(0, SyntaxFactory.Argument(SyntaxFactory.IdentifierName(argsVar)));

                        // Replace the method invocation with the new syntax
                        InvocationExpressionSyntax newInvocation = invocation.WithArgumentList(SyntaxFactory.ArgumentList(
                            SyntaxFactory.SeparatedList(newArgsForMethodInvocation)
                        ));

                        // Create a variable to hold the arguments class instance
                        string myClassType = $"{classDeclaration.GetAnnotations(AnnotationKind.EnclosingType.ToString()).First().Data}.{classDeclaration.Identifier.Text}";

                        //declaration of the variable
                        LocalDeclarationStatementSyntax argsVarDeclStatement = SyntaxFactory.LocalDeclarationStatement(
                            SyntaxFactory.VariableDeclaration(SyntaxFactory.IdentifierName("var"))
                            .WithVariables(SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.VariableDeclarator(argsVar)
                                .WithInitializer(SyntaxFactory.EqualsValueClause(SyntaxFactory.ObjectCreationExpression(
                                    SyntaxFactory.ParseTypeName(myClassType))
                                .WithInitializer(initializerExpression)
                                    .WithNewKeyword(SyntaxFactory.Token(SyntaxKind.NewKeyword)
                                    .WithTrailingTrivia(SyntaxFactory.Space))
                                    .WithArgumentList(SyntaxFactory.ArgumentList()))))));

                        editor.InsertBefore(target, argsVarDeclStatement);

                        editor.ReplaceNode(invocation, newInvocation.NormalizeWhitespace());
                    }

                    SyntaxNode newRoot = editor.GetChangedRoot();

                    newRoot = Formatter.Format(newRoot, solution.Workspace, null, cancellationToken);

                    solution = solution.WithDocumentSyntaxRoot(document.Id, newRoot);

                    //_roots.Remove(document.Id);
                }
            }

            return solution;
        }

        /// <summary>
        /// Modify method definition - signature and references to the parameters in the body of the method
        /// </summary>
        /// <param name="solution"></param>
        /// <param name="parameterNodes"></param>
        /// <param name="classDeclaration"></param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        private async Task<Solution> ModifyMethodDefinitionAsync(Solution solution, List<ParameterSyntax> parameterNodes, ClassDeclarationSyntax classDeclaration
            , CancellationToken cancellationToken)
        {
            Document document = solution.GetDocument(_documentId);

            SyntaxNode root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

            Location methodIdentifierLocation = _methodIdentifier.GetLocation();
            MethodDeclarationSyntax methodSyntax = root.FindNode(methodIdentifierLocation.SourceSpan) as MethodDeclarationSyntax;

            #region fix method signature

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

            #endregion

            #region fix xml comment

            SyntaxTrivia xmlCommentTrivia = methodSyntax.GetLeadingTrivia().FirstOrDefault(t => t.IsKind(SyntaxKind.SingleLineDocumentationCommentTrivia));

            if (xmlCommentTrivia != null)
            {
                //this is absolutely horrific but I dont give a shit at this point
                //we really should be using SyntaxFactory and XmlElements and Attributes for this, but fuck it
                //we assume that there is always </summary> in the comment, if its not, then it wont do anything
                string input = xmlCommentTrivia.ToString();
                int index = input.IndexOf("</summary>");
                if (index >= 0)
                {
                    //new param tags
                    string pipeSeparatedParamNames = string.Join("|", newParameters.Select(x => x.Identifier.ValueText));

                    string myString = Regex.Replace(input, $@"^\s*///\s*<param name=""(?!{pipeSeparatedParamNames})\w+"">.*", "", RegexOptions.Multiline); //replace the param comments which we no longer have
                    myString = Regex.Replace(myString, @"^\s*$[\r\n]*", "", RegexOptions.Multiline); //replace empty lines beacause chatgpt

                    //create entries for newly added params
                    IEnumerable<string> newParametersStr = newParameters
                        .Where(x => !myString.Contains($"<param name=\"{x.Identifier.ValueText}\">"))
                        .Select(x => $"/// <param name=\"{x.Identifier.ValueText}\"></param>");

                    string newParametersStrFragment = "\n" + string.Join("\n", newParametersStr);

                    //insert the new params right after </summary> tag and also create leading /// which we lost for some reason lol
                    myString = $"/// {myString.Insert(index + "</summary>".Length, newParametersStrFragment).TrimStart()}";

                    //add the trivia               
                    newMethodSyntax = newMethodSyntax.WithLeadingTrivia(SyntaxFactory.ParseLeadingTrivia(myString));
                }
            }

            #endregion

            #region fix method body

            // Replace all references to the old parameters with references to the new args parameter
            IEnumerable<IdentifierNameSyntax> nodesToReplace = newMethodSyntax.DescendantNodes()
                .OfType<IdentifierNameSyntax>()
                .Where(x => parameterNodes.Any(p => p.Identifier.ValueText == x.Identifier.ValueText));

            BlockSyntax newBody = newMethodSyntax.Body.ReplaceNodes(nodesToReplace,
                (node, _) => SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression
                            , SyntaxFactory.IdentifierName(parameterName)
                            , SyntaxFactory.IdentifierName(parameterNodes.First(x => x.Identifier.ValueText == node.Identifier.ValueText).GetAnnotations(AnnotationKind.PropName.ToString()).First().Data))
                    .WithOperatorToken(SyntaxFactory.Token(SyntaxKind.DotToken))
                    .WithLeadingTrivia(node.GetLeadingTrivia())
                    .WithTrailingTrivia(node.GetTrailingTrivia())
                    );

            // Create the new method syntax with the updated body
            newMethodSyntax = newMethodSyntax.WithBody(newBody);

            #endregion

            SyntaxEditor editor = new SyntaxEditor(root, solution.Workspace.Services);

            editor.ReplaceNode(methodSyntax, newMethodSyntax);
            editor.InsertBefore(newMethodSyntax, classDeclaration);

            SyntaxNode newRoot = editor.GetChangedRoot();

            //newRoot = Formatter.Format(newRoot, solution.Workspace, null, cancellationToken); //this produces huge preview

            return solution.WithDocumentSyntaxRoot(document.Id, newRoot);
        }
    }
}
