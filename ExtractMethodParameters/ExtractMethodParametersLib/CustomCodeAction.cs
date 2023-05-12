using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;

namespace ExtractMethodParametersLib
{
    /// <summary>
    /// This modified CodeAction is needed just so we know if we are in preview mode or not, otherwise this class would not be needed
    /// </summary>
    public class CustomCodeAction : CodeAction
    {
        private readonly Func<CancellationToken, bool, Task<Solution>> _createChangedSolution;

        public override string EquivalenceKey { get; }
        public override string Title { get; }

        protected CustomCodeAction(string title, Func<CancellationToken, bool, Task<Solution>> createChangedSolution, string equivalenceKey = null)
        {
            _createChangedSolution = createChangedSolution;

            Title = title;
            EquivalenceKey = equivalenceKey;
        }

        /// <summary>
        ///     Creates a <see cref="CustomCodeAction" /> for a change to more than one <see cref="Document" /> within a <see cref="Solution" />.
        ///     Use this factory when the change is expensive to compute and should be deferred until requested.
        /// </summary>
        /// <param name="title">Title of the <see cref="CustomCodeAction" />.</param>
        /// <param name="createChangedSolution">Function to create the <see cref="Solution" />.</param>
        /// <param name="equivalenceKey">Optional value used to determine the equivalence of the <see cref="CustomCodeAction" /> with other <see cref="CustomCodeAction" />s. See <see cref="CustomCodeAction.EquivalenceKey" />.</param>
        public static CustomCodeAction Create(string title, Func<CancellationToken, bool, Task<Solution>> createChangedSolution, string equivalenceKey = null)
        {
            if (title == null)
                throw new ArgumentNullException(nameof(title));

            if (createChangedSolution == null)
                throw new ArgumentNullException(nameof(createChangedSolution));

            return new CustomCodeAction(title, createChangedSolution, equivalenceKey);
        }

        protected override async Task<IEnumerable<CodeActionOperation>> ComputePreviewOperationsAsync(CancellationToken cancellationToken)
        {
            const bool isPreview = true;
            // Content copied from http://source.roslyn.io/#Microsoft.CodeAnalysis.Workspaces/CodeActions/CodeAction.cs,81b0a0866b894b0e,references
            var changedSolution = await GetChangedSolutionWithPreviewAsync(cancellationToken, isPreview).ConfigureAwait(false);
            if (changedSolution == null)
                return null;

            return new CodeActionOperation[] { new ApplyChangesOperation(changedSolution) };
        }

        protected override Task<Solution> GetChangedSolutionAsync(CancellationToken cancellationToken)
        {
            const bool isPreview = false;
            return GetChangedSolutionWithPreviewAsync(cancellationToken, isPreview);
        }

        protected virtual Task<Solution> GetChangedSolutionWithPreviewAsync(CancellationToken cancellationToken, bool isPreview)
        {
            return _createChangedSolution(cancellationToken, isPreview);
        }
    }
}