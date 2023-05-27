using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;
using System.Composition;
using System.Linq;
using System.Threading.Tasks;

namespace ExtractMethodParametersLib
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

            MethodDeclarationSyntax methodSyntax = node.AncestorsAndSelf()
                .OfType<MethodDeclarationSyntax>()
                .FirstOrDefault();

            List<ParameterSyntax> parameterNodes = (from p in methodSyntax.ParameterList.Parameters

                                                    let hasGenericParam = (p.Type as GenericNameSyntax)?.TypeArgumentList?.Arguments //skip generics such as List<T> etc
                                                                          .OfType<IdentifierNameSyntax>()
                                                                          .Any(typeArg => methodSyntax.TypeParameterList?.Parameters
                                                                              .Any(typeParam => typeParam.Identifier.ValueText == typeArg.Identifier.ValueText) == true) == true

                                                    where !hasGenericParam
                                                    where !p.Modifiers.Any(m => m.IsKind(SyntaxKind.OutKeyword) || m.IsKind(SyntaxKind.ParamsKeyword)) //skip out and params
                                                    where p.Span.IntersectsWith(context.Span) //get just selected text

                                                    select p)
                                  .ToList();

            //exit when user selected less than 2 params
            if (parameterNodes.Count < 2)
                return;

            CustomCodeAction action = CustomCodeAction.Create("Extract method parameters", (cancellationToken, isPreview) =>
            {
                return new MethodProcessor(isPreview, context.Document, methodSyntax, parameterNodes).ProcessAsync(cancellationToken);
            });

            context.RegisterRefactoring(action);
        }

    }
}
