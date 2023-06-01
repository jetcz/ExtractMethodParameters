using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Editing;
using Microsoft.CodeAnalysis.FindSymbols;
using Microsoft.CodeAnalysis.Formatting;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;

namespace ExtractMethodParametersLib
{
    internal class MethodProcessor
    {
        /*

         _UseIndividualAssigmentStatements = true

            var args = new Class1Declaration.MyMethodArgs();
            args.MyId = (int)myEnum.option;
            args.Name = nameof(Class1Declaration);


         _UseIndividualAssigmentStatements = false

            var args = new Class1Declaration.MyMethodArgs()
            {
                MyId = (int)myEnum.option,
                Name = nameof(Class1Declaration)
            };

         */
        private readonly bool _UseIndividualAssigmentStatements = true; //this would be nice to have in tools->options but I don't know how

        private readonly bool _isPreview;
        private readonly DocumentId _documentId;
        private readonly MethodDeclarationSyntax _methodSyntax;
        private Solution _solution;
        private List<ParameterSyntax> _parameters;
        private ExpressionSyntaxComparer _expressionSyntaxComparer;
        private IEnumerable<ReferencedSymbol> _allReferences;

        /// <summary>
        /// Processes selected method/parameters
        /// </summary>
        /// <param name="isPreview">true if we are in preview (just the current document), false for the actual code modification in whole solution</param>
        /// <param name="document"></param>
        /// <param name="methodSyntax"></param>
        /// <param name="parameters">user selected parameters</param>
        /// <exception cref="ArgumentNullException"></exception>
        public MethodProcessor(bool isPreview, Document document, MethodDeclarationSyntax methodSyntax, List<ParameterSyntax> parameters)
        {
            _isPreview = isPreview;
            _documentId = document.Id ?? throw new ArgumentNullException(nameof(document));
            _solution = document.Project.Solution ?? throw new ArgumentNullException(nameof(document));
            _methodSyntax = methodSyntax ?? throw new ArgumentNullException(nameof(methodSyntax));
            _parameters = parameters ?? throw new ArgumentNullException(nameof(parameters));
            _expressionSyntaxComparer = new ExpressionSyntaxComparer();
        }


        /// <summary>
        /// Do the magic
        /// </summary>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        internal async Task<Solution> ProcessAsync(CancellationToken cancellationToken)
        {
#if DEBUG
            Stopwatch sw = Stopwatch.StartNew();
#endif

            _expressionSyntaxComparer = new ExpressionSyntaxComparer();

            _parameters = ProcessParameters().ToList();

            ClassDeclarationSyntax classDeclaration = await CreateClassDeclarationAsync(cancellationToken);

            _solution = await ModifyMethodReferencesAsync(classDeclaration, cancellationToken);

#if DEBUG
            sw.Stop();
            Debug.WriteLine($"{nameof(ExtractMethodParametersCodeRefactoringProvider)} isPreview={_isPreview} elapsed={sw.Elapsed}");
#endif

            //is this needed?
            _allReferences = null;
            _parameters = null;
            _expressionSyntaxComparer = null;

            return _solution;
        }

        /// <summary>
        /// Process collection of parameters and create the name for our new properties and store it to custom annotation of the ParameterSyntax
        /// It makes sure the names have a naming convention and are unique
        /// </summary>
        /// <returns></returns>
        private IEnumerable<ParameterSyntax> ProcessParameters()
        {
            string solutionName = Path.GetFileNameWithoutExtension(_solution.FilePath);

            IPropertyNameNormalizer propertyNameNormalizer = PropertyNormalizerFactory.CreateNormalizer(solutionName);

            var groups = _parameters.GroupBy(x => propertyNameNormalizer.Normalize(x.Identifier.ValueText));

            foreach (var group in groups)
            {
                var parameters = group.ToArray();
                string propertyName = group.Key;

                for (int i = 0; i < parameters.Length; i++)
                {
                    string ordinal = i == 0 ? "" : (i + 1).ToString();

                    SyntaxAnnotation propName = new SyntaxAnnotation(AnnotationKind.PropName.ToString(), $"{propertyName}{ordinal}");
                    yield return parameters[i].WithAdditionalAnnotations(propName);
                }
            }
        }


        /// <summary>
        /// Create class declaration with properties
        /// </summary>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        private async Task<ClassDeclarationSyntax> CreateClassDeclarationAsync(CancellationToken cancellationToken)
        {
            Document document = _solution.GetDocument(_documentId);

            SyntaxNode root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

            MethodDeclarationSyntax methodSyntax = root.DescendantNodes().OfType<MethodDeclarationSyntax>()
                .First(x => SyntaxFactory.AreEquivalent(x, _methodSyntax));

            // Get the semantic model for the document
            SemanticModel semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            ClassDeclarationSyntax enclosingClass = methodSyntax.Ancestors().OfType<ClassDeclarationSyntax>().FirstOrDefault();

            // create class name       
            string uniqueMyClassName = $"{methodSyntax.Identifier}Args";
            string enclosingTypeName = "";
            if (enclosingClass != null)
            {
                INamedTypeSymbol enclosingTypeSymbol = semanticModel.GetDeclaredSymbol(enclosingClass, cancellationToken);
                enclosingTypeName = enclosingTypeSymbol.Name;

                string name = uniqueMyClassName;
                int i = 0;
                while (enclosingTypeSymbol.GetTypeMembers(uniqueMyClassName).Any())
                {
                    i++;
                    uniqueMyClassName = $"{name}{i}";
                }
            }
            else
            {
                //checking the whole solution would be too expensive
                uniqueMyClassName += new Random().Next(0, 100).ToString(); //this is good enough
            }

            //do not try to figure out default property initializers in preview mode
            var defaultValues = _isPreview ? null : await GetDefaultValuesForPropertiesAsync(cancellationToken);

            // Find the XML comment trivia for the method
            SyntaxTrivia xmlCommentTrivia = methodSyntax.GetLeadingTrivia().FirstOrDefault(t => t.IsKind(SyntaxKind.SingleLineDocumentationCommentTrivia));

            string xmlMethodComment = xmlCommentTrivia.ToString();

            var properties = CreateProperties(defaultValues, xmlMethodComment);

            string xmlClassComment = string.IsNullOrEmpty(xmlMethodComment) ? ""
                : $"/// <summary>{Environment.NewLine}/// {methodSyntax.Identifier} arguments{Environment.NewLine}/// </summary>{Environment.NewLine}";

            return SyntaxFactory.ClassDeclaration(uniqueMyClassName)
                .WithModifiers(SyntaxFactory.TokenList(SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
                .WithMembers(SyntaxFactory.List(properties))
                .WithLeadingTrivia(SyntaxFactory.ParseLeadingTrivia(xmlClassComment))
                .WithAdditionalAnnotations(new SyntaxAnnotation(AnnotationKind.EnclosingType.ToString(), enclosingTypeName));
        }

        /// <summary>
        /// Create class properties with xml comments
        /// </summary>
        /// <param name="defaultValues"></param>
        /// <param name="methodXmlComment"></param>
        /// <returns></returns>
        private IEnumerable<MemberDeclarationSyntax> CreateProperties(Dictionary<string, ExpressionSyntax> defaultValues, string methodXmlComment)
        {
            foreach (ParameterSyntax par in _parameters)
            {
                //declare the property
                SyntaxToken identifier = SyntaxFactory.Identifier(par.GetAnnotations(AnnotationKind.PropName.ToString()).First().Data);

                SyntaxAnnotation paramName = new SyntaxAnnotation(AnnotationKind.ParamName.ToString(), par.Identifier.ValueText);

                PropertyDeclarationSyntax propertyDeclaration = SyntaxFactory.PropertyDeclaration(par.Type.WithoutTrivia(), identifier)
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
                    .WithAdditionalAnnotations(paramName);

                #region xml comments for properties (steal them from the method xml comment)

                int parameterIndex = methodXmlComment.IndexOf($"<param name=\"{par.Identifier.ValueText}\"");
                if (parameterIndex != -1)
                {
                    int commentStartIndex = methodXmlComment.IndexOf(">", parameterIndex) + 1;
                    int commentEndIndex = methodXmlComment.IndexOf($"</param>", parameterIndex);

                    string comment = methodXmlComment.Substring(commentStartIndex, commentEndIndex - commentStartIndex);
                    string xmlComment = $"/// <summary>{Environment.NewLine}/// {comment}{Environment.NewLine}/// </summary>{Environment.NewLine}";

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
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        private async Task<Dictionary<string, ExpressionSyntax>> GetDefaultValuesForPropertiesAsync(CancellationToken cancellationToken)
        {
            //get the method symbol to find references for this method     
            Document document = _solution.GetDocument(_documentId);

            SyntaxNode root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

            MethodDeclarationSyntax methodSyntax = root.DescendantNodes().OfType<MethodDeclarationSyntax>()
                .First(x => SyntaxFactory.AreEquivalent(x, _methodSyntax));

            // Get the semantic model for the syntax node
            SemanticModel semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            // Get the method symbol from the syntax node
            IMethodSymbol srcMethodSymbol = semanticModel.GetDeclaredSymbol(methodSyntax, cancellationToken);

            _allReferences = await SymbolFinder.FindReferencesAsync(srcMethodSymbol, _solution, cancellationToken).ConfigureAwait(false);

            var paramsData = new Dictionary<string, Dictionary<ExpressionSyntax, int>>(_parameters.Count);

            var usableAsDefaultExpressions = new Dictionary<ExpressionSyntax, bool>(_expressionSyntaxComparer);

            //go through all references
            foreach (ReferencedSymbol reference in _allReferences)
            {
                var groupsByDoc = reference.Locations.GroupBy(x => x.Document.Id);

                //group by document so that we do not load syntax tree for the same document repeatedly
                foreach (IGrouping<DocumentId, ReferenceLocation> groupByDoc in groupsByDoc)
                {
                    semanticModel = null;

                    if (groupByDoc.Key != document.Id)
                    {
                        document = _solution.GetDocument(groupByDoc.Key);

                        root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
                    }

                    //go through each reference in scope of a single document
                    foreach (ReferenceLocation location in groupByDoc)
                    {
                        SyntaxNode syntaxNode = root.FindNode(location.Location.SourceSpan);
                        InvocationExpressionSyntax invocation = syntaxNode.AncestorsAndSelf().OfType<InvocationExpressionSyntax>().FirstOrDefault();

                        //invocation might be null if the method is referenced e.g. in xml comment as <see cref...
                        if (invocation is null)
                        {
                            continue;
                        }

                        //do not consider unit tests as source for default values
                        bool isTestMethod() =>
                            syntaxNode.Ancestors()
                            .OfType<MethodDeclarationSyntax>()
                            .SelectMany(x => x.AttributeLists)
                            .SelectMany(x => x.Attributes)
                            .Any(x => x.Name.ToString() == "TestMethod");

                        bool isTestClass() =>
                            syntaxNode.Ancestors()
                            .OfType<ClassDeclarationSyntax>()
                            .SelectMany(x => x.AttributeLists)
                            .SelectMany(x => x.Attributes)
                            .Any(x => x.Name.ToString() == "TestClass");

                        if (isTestMethod() || isTestClass())
                        {
                            continue;
                        }

                        //go through all arguments
                        for (int i = 0; i < invocation.ArgumentList.Arguments.Count; i++)
                        {
                            ArgumentSyntax argument = invocation.ArgumentList.Arguments[i];

                            //get parameters either by index or name (if the method call has named paramters)
                            IParameterSymbol parameter = argument.NameColon is null ?
                                srcMethodSymbol.Parameters[i]
                                : srcMethodSymbol.Parameters.First(p => p.Name == argument.NameColon.Name.Identifier.ValueText);

                            if (!_parameters.Exists(x => x.Identifier.ValueText == parameter.Name))
                            {
                                continue;
                            }    

                            //determine if the argument value can be used as default property initializer, ie. it is some literal, or static member accessor
                            bool canBeUsedAsDefaultInitializer = false;

                            #region can this argument be used as default property initializer?

                            if (argument.Expression is LiteralExpressionSyntax) //including NullLiteralExpression
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
                                }

                                usableAsDefaultExpressions.Add(argument.Expression, canBeUsedAsDefaultInitializer);
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
            var retval = new Dictionary<string, ExpressionSyntax>(_parameters.Count);

            foreach (var paramData in paramsData)
            {
                var expressionOccurences = paramData.Value;

                ExpressionSyntax expression = expressionOccurences.OrderByDescending(x => x.Value).FirstOrDefault(x => x.Value > 1).Key; //take the value which is there at least twice

                if (expression != null)
                {
                    retval.Add(paramData.Key, expression);
                }
            }

            return retval;
        }

        /// <summary>
        /// Changes method definitions and calls to reflect new method signature
        /// </summary>
        /// <param name="classDeclaration"></param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        /// <exception cref="Exception"></exception>
        private async Task<Solution> ModifyMethodReferencesAsync(ClassDeclarationSyntax classDeclaration, CancellationToken cancellationToken)
        {
            #region preparation

            Document document = _solution.GetDocument(_documentId);

            SyntaxNode root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

            SyntaxEditor editor = new SyntaxEditor(root, _solution.Workspace.Services);

            MethodDeclarationSyntax methodSyntax = root.DescendantNodes().OfType<MethodDeclarationSyntax>()
                .First(x => SyntaxFactory.AreEquivalent(x, _methodSyntax));

            SemanticModel srcSemanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
            IMethodSymbol srcMethodSymbol = srcSemanticModel.GetDeclaredSymbol(methodSyntax, cancellationToken);

            var classProps = classDeclaration.Members.OfType<PropertyDeclarationSyntax>()
                .ToDictionary(prop => prop.GetAnnotations(AnnotationKind.ParamName.ToString()).First().Data, prop => prop);

            //get referecnces
            IEnumerable<ReferencedSymbol> references;
            if (_isPreview)
            {
                references = await SymbolFinder.FindReferencesAsync(srcMethodSymbol, _solution, new[] { document }.ToImmutableHashSet(), cancellationToken).ConfigureAwait(false);
            }
            else if (_allReferences != null)
            {
                references = _allReferences;
            }
            else
            {
                throw new Exception($"{nameof(_allReferences)} not initialized");
            }

            var referencesWithIdsGroups = await AssignDocumentIdsToReferencesAsync(references, cancellationToken).ConfigureAwait(false);

            //method which populates document, root and editor by the document id if needed
            async void RefreshValuesByDocumentContext(DocumentId documentId)
            {
                if (documentId != document.Id)
                {
                    document = _solution.GetDocument(documentId);

                    root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

                    editor = new SyntaxEditor(root, _solution.Workspace.Services);
                }
            }

            //method which saves changed editor to solution
            void SaveChanges()
            {
                SyntaxNode newRoot = editor.GetChangedRoot();

                if (!_isPreview) //do not format the current document in preview mode to keep the preview small
                {
                    newRoot = Formatter.Format(newRoot, _solution.Workspace, null, cancellationToken);
                }

                _solution = _solution.WithDocumentSyntaxRoot(document.Id, newRoot);
            }

            #endregion

            #region do the magic

            //we are trying to minimize calls to RefreshValuesByDocumentContext (getsyntaxroot) -> we modify the given document only once

            //first process documents where we have method declarations (references)
            foreach (var referencesWithIdsGroup in referencesWithIdsGroups)
            {
                RefreshValuesByDocumentContext(referencesWithIdsGroup.Key);

                //get all locations in the same document which we are working with now
                var documentLocationsGroup = from rf in references
                                             from location in rf.Locations
                                             where location.Document.Id == referencesWithIdsGroup.Key
                                             select location;

                ModifyLocations(classDeclaration, root, editor, srcMethodSymbol, classProps, documentLocationsGroup);

                #region modify method declarations

                //firs process documents where we have method declarations
                foreach (var (reference, oldMethodSyntax) in referencesWithIdsGroup)
                {
                    MethodDeclarationSyntax newMethodSyntax = GetNewMethodDefinition(oldMethodSyntax, classDeclaration);

                    editor.ReplaceNode(oldMethodSyntax, newMethodSyntax); //replace method declaration, typically we'll do this just once, but if there is an interface in the game, we might be working with multiple methods

                    //put the class declaration near the place where user selected the parameters
                    if (SymbolEqualityComparer.Default.Equals(reference.Definition, srcMethodSymbol))
                    {
                        TypeDeclarationSyntax enclosingTypeDeclarationSyntax = oldMethodSyntax?.Ancestors()?.OfType<TypeDeclarationSyntax>()?.FirstOrDefault();
                        if (enclosingTypeDeclarationSyntax != null)
                        {
                            switch (enclosingTypeDeclarationSyntax)
                            {
                                case ClassDeclarationSyntax _:
                                    editor.InsertBefore(newMethodSyntax, classDeclaration);
                                    break;
                                case InterfaceDeclarationSyntax _:
                                    editor.InsertBefore(enclosingTypeDeclarationSyntax, classDeclaration);
                                    break;
                                default:
                                    throw new Exception($"Unknown enclosing type {enclosingTypeDeclarationSyntax.GetType()}");
                            }
                        }
                    }
                }

                #endregion

                SaveChanges();
            }

            //then process documents where we have just references (locations) and not method declarations
            var otherLocationsGroups = from rf in references
                                       from location in rf.Locations
                                       where !referencesWithIdsGroups.Any(x => x.Key == location.Document.Id)
                                       group location by location.Document.Id;

            foreach (var documentLocationsGroup in otherLocationsGroups)
            {
                RefreshValuesByDocumentContext(documentLocationsGroup.Key);

                ModifyLocations(classDeclaration, root, editor, srcMethodSymbol, classProps, documentLocationsGroup);

                SaveChanges();
            }

            #endregion

            return _solution;
        }

        /// <summary>
        /// Modify method definition - signature and references to the parameters in the body of the method
        /// </summary>
        /// <param name="methodSyntax"></param>
        /// <param name="classDeclaration"></param>
        /// <returns></returns>
        private MethodDeclarationSyntax GetNewMethodDefinition(MethodDeclarationSyntax methodSyntax, ClassDeclarationSyntax classDeclaration)
        {
            #region modify method signature            

            // Get the list of parameters from the method's syntax node
            ParameterListSyntax oldParameterList = methodSyntax.DescendantNodes().OfType<ParameterListSyntax>().First();

            List<ParameterSyntax> newParameters = oldParameterList.Parameters
                .Where(x => !_parameters.Exists(y => y.Identifier.ValueText == x.Identifier.ValueText))
                .ToList();

            string parameterName = "args";
            int i = 0;
            while (newParameters.Exists(x => x.Identifier.ValueText == parameterName))
            {
                i++;
                parameterName = $"args{i}";
            }

            // Get the type of the class
            //IdentifierNameSyntax className = SyntaxFactory.IdentifierName(classDeclaration.Identifier);
            string type = GetTypeForArgsVariable(classDeclaration, methodSyntax);

            TypeSyntax classType = SyntaxFactory.ParseTypeName(type);

            ParameterSyntax argsParameter = SyntaxFactory.Parameter(SyntaxFactory.Identifier(parameterName))
                .WithType(classType);

            newParameters.Insert(0, argsParameter);

            // Create a new parameter list syntax that contains only the selected parameters
            ParameterListSyntax newParameterList = SyntaxFactory.ParameterList(SyntaxFactory.SeparatedList(newParameters));

            // Replace the old parameter list with the new one in the method's syntax node
            MethodDeclarationSyntax newMethodSyntax = methodSyntax.ReplaceNode(oldParameterList, newParameterList).WithParameterList(newParameterList);

            #endregion

            #region modify xml comment

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
                    var newParametersStr = newParameters
                        .Where(x => !myString.Contains($"<param name=\"{x.Identifier.ValueText}\">"))
                        .Select(x => $"/// <param name=\"{x.Identifier.ValueText}\"></param>");

                    string newParametersStrFragment = Environment.NewLine + string.Join(Environment.NewLine, newParametersStr);

                    //insert the new params right after </summary> tag and also create leading /// which we lost for some reason lol
                    myString = $"/// {myString.Insert(index + "</summary>".Length, newParametersStrFragment).TrimStart()}";

                    //add the trivia               
                    newMethodSyntax = newMethodSyntax.WithLeadingTrivia(SyntaxFactory.ParseLeadingTrivia(myString));
                }
            }

            #endregion

            #region modify method body

            //interface method doesnt have body
            // Replace all references to the old parameters with references to the new args parameter
            var nodesToReplace = newMethodSyntax.DescendantNodes()
                .OfType<IdentifierNameSyntax>()
                .Where(x => _parameters.Any(p => p.Identifier.ValueText == x.Identifier.ValueText));

            //method which does the replacing
            T ReplaceParameterReferences<T>(T body) where T : SyntaxNode
            {
                return body.ReplaceNodes(nodesToReplace,
                    (node, _) => SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression
                                , SyntaxFactory.IdentifierName(parameterName)
                                , SyntaxFactory.IdentifierName(_parameters.First(x => x.Identifier.ValueText == node.Identifier.ValueText).GetAnnotations(AnnotationKind.PropName.ToString()).First().Data))
                        .WithOperatorToken(SyntaxFactory.Token(SyntaxKind.DotToken))
                        .WithLeadingTrivia(node.GetLeadingTrivia())
                        .WithTrailingTrivia(node.GetTrailingTrivia())
                        );
            }

            //method has body { ... }
            if (newMethodSyntax.Body != null)
            {
                BlockSyntax newBody = ReplaceParameterReferences(newMethodSyntax.Body);

                // Create the new method syntax with the updated body
                newMethodSyntax = newMethodSyntax.WithBody(newBody);
            }
            else
            {
                //method is expressions syntax => ...
                ArrowExpressionClauseSyntax lambda = newMethodSyntax.DescendantNodes().OfType<ArrowExpressionClauseSyntax>().FirstOrDefault();
                if (lambda != null)
                {
                    ArrowExpressionClauseSyntax newLambda = ReplaceParameterReferences(lambda);
                    newMethodSyntax = newMethodSyntax.WithExpressionBody(newLambda);
                }
            }

            #endregion

            return newMethodSyntax;
        }

        /// <summary>
        /// Modify method references
        /// </summary>
        /// <param name="classDeclaration"></param>
        /// <param name="root"></param>
        /// <param name="editor"></param>
        /// <param name="srcMethodSymbol"></param>
        /// <param name="classProps"></param>
        /// <param name="thisDocumentLocations"></param>
        private void ModifyLocations(ClassDeclarationSyntax classDeclaration, SyntaxNode root, SyntaxEditor editor, IMethodSymbol srcMethodSymbol
            , Dictionary<string, PropertyDeclarationSyntax> classProps, IEnumerable<ReferenceLocation> thisDocumentLocations)
        {
            int variableCount = 0; //it would be better to check just current block, but this is easier...

            foreach (ReferenceLocation location in thisDocumentLocations)
            {
                SyntaxNode syntaxNode = root.FindNode(location.Location.SourceSpan);
                InvocationExpressionSyntax invocation = syntaxNode?.AncestorsAndSelf()?.OfType<InvocationExpressionSyntax>()?.FirstOrDefault();

                //invocation might be null if the method is referenced e.g. in xml comment as <see cref...
                if (invocation is null)
                {
                    continue;
                }

                //find a block syntax where we put our new variable, we insert it before the target which should be expression syntax                
                SyntaxNode target = invocation.Parent;
                while (!(target.Parent is BlockSyntax))
                {
                    target = target.Parent;
                }
                BlockSyntax block = target.Parent as BlockSyntax;            

                // scan the code block if there is the same variable already, create unique name
                var variables = block.DescendantNodes().OfType<VariableDeclarationSyntax>().SelectMany(x => x.Variables);

                // create object name       
                string uniqueVariableName;
                do
                {
                    string ordinal = variableCount == 0 ? "" : variableCount.ToString();
                    uniqueVariableName = $"args{ordinal}";
                    variableCount++;

                } while (variables.Any(x => x.Identifier.ValueText == uniqueVariableName));

                SyntaxToken argsVar = SyntaxFactory.Identifier(uniqueVariableName);

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

                    ExpressionSyntax leftSide;
                    if (_UseIndividualAssigmentStatements)
                    {
                        leftSide = SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            SyntaxFactory.IdentifierName(argsVar),
                            SyntaxFactory.IdentifierName(prop.Identifier));
                    }
                    else
                    {
                        leftSide = SyntaxFactory.IdentifierName(prop.Identifier);
                    }

                    ExpressionSyntax rightSide = argument.Expression;

                    // Create a syntax node for the property assignment
                    AssignmentExpressionSyntax propertyAssignment = SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, leftSide, rightSide)
                        .WithLeadingTrivia(SyntaxFactory.LineFeed)
                        .WithoutTrailingTrivia();

                    // Add the property assignment to the initializer expression
                    initializerExpression = initializerExpression.AddExpressions(propertyAssignment);
                }

                #endregion

                newArgsForMethodInvocation.Insert(0, SyntaxFactory.Argument(SyntaxFactory.IdentifierName(argsVar)));

                // Replace the method invocation with the new syntax
                InvocationExpressionSyntax newInvocation = invocation.WithArgumentList(SyntaxFactory.ArgumentList(
                    SyntaxFactory.SeparatedList(newArgsForMethodInvocation)
                ));

                // Create a variable to hold the arguments class instance
                string myClassType = GetTypeForArgsVariable(classDeclaration, syntaxNode);

                ObjectCreationExpressionSyntax objectCreationExpression = SyntaxFactory.ObjectCreationExpression(
                            SyntaxFactory.ParseTypeName(myClassType))
                            .WithNewKeyword(SyntaxFactory.Token(SyntaxKind.NewKeyword)
                            .WithTrailingTrivia(SyntaxFactory.Space))
                            .WithArgumentList(SyntaxFactory.ArgumentList());

                if (!_UseIndividualAssigmentStatements)
                {
                    objectCreationExpression = objectCreationExpression.WithInitializer(initializerExpression);
                }

                //declaration of the variable
                LocalDeclarationStatementSyntax argsVarDeclStatement = SyntaxFactory.LocalDeclarationStatement(
                    SyntaxFactory.VariableDeclaration(SyntaxFactory.IdentifierName("var"))
                    .WithVariables(SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.VariableDeclarator(argsVar)
                        .WithInitializer(SyntaxFactory.EqualsValueClause(objectCreationExpression))))
                    );

                //modify the syntax tree
                editor.InsertBefore(target, argsVarDeclStatement);

                if (_UseIndividualAssigmentStatements)
                {
                    editor.InsertBefore(target, initializerExpression.Expressions.Select(x => SyntaxFactory.ExpressionStatement(x)));
                }

                editor.ReplaceNode(invocation, newInvocation.NormalizeWhitespace());
            }
        }

        /// <summary>
        /// Find document id for each reference/methodSyntax
        /// </summary>
        /// <param name="references"></param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        private async Task<IEnumerable<IGrouping<DocumentId, (ReferencedSymbol, MethodDeclarationSyntax)>>> AssignDocumentIdsToReferencesAsync(IEnumerable<ReferencedSymbol> references, CancellationToken cancellationToken)
        {
            //this is not great for perfromance but we assume that there will be typically just one reference
            //there might be more references when we work with interface

            var referenecesGroupedByDocumentId = new List<Grouping<DocumentId, (ReferencedSymbol, MethodDeclarationSyntax)>>();

            foreach (ReferencedSymbol reference in references)
            {
                foreach (SyntaxReference syntaxReference in reference.Definition.DeclaringSyntaxReferences) //should be almost always 1
                {
                    MethodDeclarationSyntax methodSyntax = await syntaxReference.GetSyntaxAsync(cancellationToken) as MethodDeclarationSyntax;

                    DocumentId documentId = _solution.GetDocumentId(syntaxReference.SyntaxTree);

                    var item = referenecesGroupedByDocumentId.Find(x => x.Key == documentId);
                    if (item is null)
                    {
                        var list = new List<(ReferencedSymbol, MethodDeclarationSyntax)> { (reference, methodSyntax) };
                        var grouping = new Grouping<DocumentId, (ReferencedSymbol, MethodDeclarationSyntax)>(documentId, list);
                        referenecesGroupedByDocumentId.Add(grouping);
                    }
                    else
                    {
                        item.Add((reference, methodSyntax));
                    }
                }
            }

            return referenecesGroupedByDocumentId;
        }

        /// <summary>
        /// Get the fully qualified type name of our new class if needed
        /// </summary>
        /// <param name="classDeclaration"></param>
        /// <param name="location"></param>
        /// <returns></returns>
        private string GetTypeForArgsVariable(ClassDeclarationSyntax classDeclaration, SyntaxNode location)
        {
            ClassDeclarationSyntax declaringClassSyntax = location.Ancestors().OfType<ClassDeclarationSyntax>().FirstOrDefault();

            string enclosingType = classDeclaration.GetAnnotations(AnnotationKind.EnclosingType.ToString()).First().Data;

            if (declaringClassSyntax is null
                || string.IsNullOrEmpty(enclosingType)
                || declaringClassSyntax.Identifier.Text == enclosingType)
            {
                return classDeclaration.Identifier.Text;
            }

            return $"{enclosingType}.{classDeclaration.Identifier.Text}";
        }
    }
}