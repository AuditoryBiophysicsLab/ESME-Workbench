namespace HRC.Collections
{
    /// <summary>
    /// Represents a binary tree.  This class provides access to the Root of the tree.  The developer
    /// must manually create the binary tree by adding descendents to the root.
    /// </summary>
    /// <typeparam name="T">The type of data stored in the binary tree's nodes.</typeparam>
    public class BinaryTree<T>
    {
        /// <summary>
        /// Clears out the contents of the binary tree.
        /// </summary>
        public void Clear() { Root = null; }

        public BinaryTreeNode<T> Root { get; set; }
    }
}
