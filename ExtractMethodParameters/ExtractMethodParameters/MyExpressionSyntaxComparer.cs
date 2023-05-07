using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;

namespace ExtractMethodParameters
{
    internal partial class ExtractMethodParametersCodeRefactoringProvider
    {
        public class MyExpressionSyntaxComparer : IEqualityComparer<ExpressionSyntax>
        {
            public bool Equals(ExpressionSyntax x, ExpressionSyntax y)
            {
               return SyntaxFactory.AreEquivalent(x, y);
            }

            public int GetHashCode(ExpressionSyntax obj)
            {
                // Implement your custom hash code calculation logic here
                return obj.ToString().GetHashCode();
            }
        }

    }
}
