using System.Text.RegularExpressions;

namespace ExtractMethodParametersLib
{
    internal interface IPropertyNameNormalizer
    {
        string Normalize(string value);
    }

    internal static class PropertyNormalizerFactory
    {
        public static IPropertyNameNormalizer CreateNormalizer(string solutionName)
        {
            if (solutionName.StartsWith("Sang"))
            {
                return new SangPropertyNameNormalizer();
            }
            else
            {
                return new GenericPropertyNameNormalizer();
            }
        }

    }

    /// <summary>
    /// Uppercase first letter
    /// </summary>
    internal class GenericPropertyNameNormalizer : IPropertyNameNormalizer
    {
        public string Normalize(string value)
        {
            return Normalize(value, 0);
        }

        public string Normalize(string value, int offset)
        {
            return value.Substring(0 + offset, 1).ToUpper() + value.Substring(1 + offset);  
        }
    }

    /// <summary>
    /// Removes leading type hint letter and caps first letter
    /// </summary>
    internal class SangPropertyNameNormalizer : IPropertyNameNormalizer
    {
        private GenericPropertyNameNormalizer genericPropertyNameNormalizer = new GenericPropertyNameNormalizer ();
        private const string pattern = @"^[a-z][A-Z].{2,}$";

        public string Normalize(string value)
        {
            bool skipFirstLetter = Regex.IsMatch(value, pattern);

            return genericPropertyNameNormalizer.Normalize(value, skipFirstLetter ? 1 : 0);
        }
    }


}
