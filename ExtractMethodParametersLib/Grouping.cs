using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ExtractMethodParametersLib
{
    /// <summary>
    /// Custom implementation of IGrouping so that we can build in manually
    /// </summary>
    /// <typeparam name="TKey"></typeparam>
    /// <typeparam name="TElement"></typeparam>
    public class Grouping<TKey, TElement> : List<TElement>, IGrouping<TKey, TElement>
    {
        public Grouping(TKey key) : base() => Key = key;
        public Grouping(TKey key, int capacity) : base(capacity) => Key = key;
        public Grouping(TKey key, IEnumerable<TElement> collection)
            : base(collection) => Key = key;
        public TKey Key { get; }
    }
}
