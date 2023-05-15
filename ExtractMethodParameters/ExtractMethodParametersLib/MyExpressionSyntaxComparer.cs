using Microsoft.CodeAnalysis;
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

        /// <summary>
        /// Custom comparing for initializaion of arguments, we are more tolerant here about what is equal
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        public bool EqualsForInit(ExpressionSyntax x, ExpressionSyntax y)
        {
            if ((x is null || x.IsKind(SyntaxKind.NullLiteralExpression))
                && (y is null || y.IsKind(SyntaxKind.NullLiteralExpression)))
            {
                return true;
            }

            if (Equals(x, y))
            {
                return true;
            }

            if (x != null && y != null)
            {
                string xx = x.ToString();
                string yy = y.ToString();

                return (xx == "\"\"" || string.Equals(xx, "string.empty", System.StringComparison.OrdinalIgnoreCase))
                    && (yy == "\"\"" || string.Equals(yy, "string.empty", System.StringComparison.OrdinalIgnoreCase));
                
            }

            return false;
        }

        public int GetHashCode(ExpressionSyntax obj)
        {
            return obj.ToString().GetHashCode();
        }
    }
}
