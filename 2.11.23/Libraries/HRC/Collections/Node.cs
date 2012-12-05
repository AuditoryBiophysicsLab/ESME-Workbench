﻿namespace HRC.Collections
{
    /// <summary>
    /// The Node&lt;T&gt; class represents the base concept of a Node for a tree or graph.  It contains
    /// a data item of type T, and a list of neighbors.
    /// </summary>
    /// <typeparam name="T">The type of data contained in the Node.</typeparam>
    /// <remarks>None of the classes in the SkmDataStructures2 namespace use the Node class directly;
    /// they all derive from this class, adding necessary functionality specific to each data structure.</remarks>
    public class Node<T>
    {
        public Node() {}
        public Node(T data) : this(data, null) {}
        public Node(T data, NodeList<T> neighbors)
        {
            Value = data;
            Neighbors = neighbors;
        }

        public T Value { get; set; }

        protected NodeList<T> Neighbors { get; set; }
    }
}
