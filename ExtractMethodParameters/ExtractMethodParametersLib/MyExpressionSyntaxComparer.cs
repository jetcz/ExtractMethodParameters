using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;

namespace ExtractMethodParametersLib
{
    public class MyExpressionSyntaxComparer : IEqualityComparer<ExpressionSyntax>
    {
        public bool Equals(ExpressionSyntax x, ExpressionSyntax y)
        {
            return SyntaxFactory.AreEquivalent(x, y);
        }

        public int GetHashCode(ExpressionSyntax obj)
        {
            return obj.ToString().GetHashCode();
        }
    }
}
