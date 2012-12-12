using System;

namespace HRC.Collections
{
    ///<summary>
    /// The RedBlackNode class encapsulates a node in the tree
    ///</summary>

    public class RedBlackNode<T>
	{
		// key provided by the calling class
        // the data or value associated with the key
        // color - used to balance the tree
        // left node 
        // right node 
        // parent node 

        ///<summary>
        ///Key
        ///</summary>
        public IComparable Key { get; set; }

        ///<summary>
        ///Data
        ///</summary>
        public T Data { get; set; }

        ///<summary>
        ///Color
        ///</summary>
        public RedBlackColors Color { get; set; }

        ///<summary>
        ///Left
        ///</summary>
        public RedBlackNode<T> Left { get; set; }

        ///<summary>
        /// Right
        ///</summary>
        public RedBlackNode<T> Right { get; set; }

        public RedBlackNode<T> Parent { get; set; }

        public RedBlackNode()
		{
            Color = RedBlackColors.Red;
		}
	}

    public enum RedBlackColors
    {
        Red = 0,
        Black = 1,
    }
}
