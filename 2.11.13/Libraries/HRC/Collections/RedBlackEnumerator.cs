using System;
using System.Collections.Generic;

namespace HRC.Collections
{
    ///<summary>
    /// The RedBlackEnumerator class returns the keys or data objects of the treap in
    /// sorted order. 
    ///</summary>
    public class RedBlackEnumerator<T> where T : class
	{
		// the treap uses the stack to order the nodes
		private readonly Stack<RedBlackNode<T>> _stack;
		// return in ascending order (true) or descending (false)
		private readonly bool _ascending;
		
		// key
        // the data or value associated with the key

        public  string  Color;             // testing only, don't use in live system
        public  IComparable ParentKey;     // testing only, don't use in live system

        ///<summary>
        ///Key
        ///</summary>
        public IComparable Key { get; set; }

        ///<summary>
        ///Data
        ///</summary>
        public T Value { get; set; }

		///<summary>
		/// Determine order, walk the tree and push the nodes onto the stack
		///</summary>
		public RedBlackEnumerator(RedBlackNode<T> tnode, bool ascending) 
        {

            _stack = new Stack<RedBlackNode<T>>();
			_ascending  = ascending;
			
            // use depth-first traversal to push nodes into stack
            // the lowest node will be at the top of the stack
            if(ascending)
			{   // find the lowest node
				while(tnode != RedBlack<T>.SentinelNode)
				{
					_stack.Push(tnode);
					tnode = tnode.Left;
				}
			}
			else
			{
                // the highest node will be at top of stack
				while(tnode != RedBlack<T>.SentinelNode)
				{
					_stack.Push(tnode);
					tnode = tnode.Right;
				}
			}
			
		}
		///<summary>
		/// HasMoreElements
		///</summary>
		public bool HasMoreElements()
		{
			return (_stack.Count > 0);
		}
		///<summary>
		/// NextElement
		///</summary>
		public RedBlackNode<T> NextElement()
		{
			if(_stack.Count == 0)
				throw(new RedBlackException("Element not found"));
			
			// the top of stack will always have the next item
			// get top of stack but don't remove it as the next nodes in sequence
			// may be pushed onto the top
			// the stack will be popped after all the nodes have been returned
			var node = _stack.Peek();	//next node in sequence
			
            if(_ascending)
            {
                if(node.Right == RedBlack<T>.SentinelNode)
                {	
                    // yes, top node is lowest node in subtree - pop node off stack 
                    var tn = _stack.Pop();
                    // peek at right node's parent 
                    // get rid of it if it has already been used
                    while(HasMoreElements()&& _stack.Peek().Right == tn)
                        tn = _stack.Pop();
                }
                else
                {
                    // find the next items in the sequence
                    // traverse to left; find lowest and push onto stack
                    var tn = node.Right;
                    while(tn != RedBlack<T>.SentinelNode)
                    {
                        _stack.Push(tn);
                        tn = tn.Left;
                    }
                }
            }
            else            // descending, same comments as above apply
            {
                if(node.Left == RedBlack<T>.SentinelNode)
                {
                    // walk the tree
                    var tn = _stack.Pop();
                    while(HasMoreElements() && (_stack.Peek()).Left == tn)
                        tn = _stack.Pop();
                }
                else
                {
                    // determine next node in sequence
                    // traverse to left subtree and find greatest node - push onto stack
                    var tn = node.Left;
                    while(tn != RedBlack<T>.SentinelNode)
                    {
                        _stack.Push(tn);
                        tn = tn.Right;
                    }
                }
            }
			
			// the following is for .NET compatibility (see MoveNext())
			Key     = node.Key;
			Value   = node.Data;
            // ******** testing only ********
            try
            {
            ParentKey = node.Parent.Key;            // testing only
            
            }
            catch(Exception)
            {
                ParentKey = 0;
            }
			Color = node.Color == 0 ? "Red" : "Black";
            // ******** testing only ********

		    return node;
		}
		///<summary>
		/// MoveNext
		/// For .NET compatibility
		///</summary>
		public bool MoveNext()
		{
			if(HasMoreElements())
			{
				NextElement();
				return true;
			}
			return false;
		}
	}
}
