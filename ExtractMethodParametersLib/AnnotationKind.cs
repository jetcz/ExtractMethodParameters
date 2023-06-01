
namespace ExtractMethodParametersLib
{
    /// <summary>
    /// Kind of annotations used in this refactoring extension
    /// </summary>
    internal enum AnnotationKind
    {
        /// <summary>
        /// Enclosing type of our new class
        /// </summary>
        EnclosingType,

        /// <summary>
        /// Orig parameter name
        /// </summary>
        ParamName,

        /// <summary>
        /// New property name
        /// </summary>
        PropName
    }
}

