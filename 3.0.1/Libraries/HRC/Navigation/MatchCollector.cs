using System;
using System.Collections;
using System.Collections.Generic;

namespace HRC.Navigation
{
    public interface IMatchCollector
    {
        /**
     * collect an indication that the query object a (or some part of it)
     * matches object b in some way, presumably by intersection.
     * 
     * @param a
     * @param b
     */
        void collect(Object a, Object b);

        /** @return an iterator over the previously collected elements * */
        IEnumerator GetEnumerator();
    }

    /**
     * A MatchCollector that collects a list of pairs of the matching objects
     */

    public class PairArrayMatchCollector : IMatchCollector
    {
        protected List<Pair> result = new List<Pair>();

        public void collect(Object a, Object b)
        {
            result.Add(new Pair(a, b));
        }

        public IEnumerator GetEnumerator()
        {
            return result.GetEnumerator();
        }
    }

    public class Pair
    {
        private readonly Object _a;
        private readonly Object _b;

        public Pair(Object a, Object b)
        {
            _a = a;
            _b = b;
        }

        public Object getA()
        {
            return _a;
        }

        public Object getB()
        {
            return _b;
        }
    }

    public class SetMatchCollector<T> : IMatchCollector
    {
        protected HashSet<T> result = new HashSet<T>();

        public void collect(Object a, Object b)
        {
            result.Add((T)b);
        }

        public IEnumerator GetEnumerator()
        {
            return result.GetEnumerator();
        }
    }

    public class MatchCollector<T, U> : IMatchCollector where T: ICollection<U>, new()
    {
        protected T result = new T();

        public void collect(Object a, Object b)
        {
            result.Add((U)b);
        }

        public IEnumerator GetEnumerator()
        {
            return result.GetEnumerator();
        }
    }

    public class CollectionMatchCollector<T> : IMatchCollector
    {
        protected List<T> c;

        public CollectionMatchCollector(List<T> c)
        {
            this.c = c;
        }

        public void collect(Object a, Object b)
        {
            c.Add((T)b);
        }

        public IEnumerator GetEnumerator()
        {
            return c.GetEnumerator();
        }
    }
}