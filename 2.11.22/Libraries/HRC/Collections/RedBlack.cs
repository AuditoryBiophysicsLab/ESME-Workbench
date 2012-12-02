using System;

namespace HRC.Collections
{
    ///<summary>
    ///A red-black tree must satisfy these properties:
    ///
    ///1. The root is black. 
    ///2. All leaves are black. 
    ///3. Red nodes can only have black children. 
    ///4. All paths from a node to its leaves contain the same number of black nodes.
    ///</summary>
    public class RedBlack<T> where T : class
	{

        // the number of nodes contained in the tree
		private int             _intCount;
        // a simple randomized hash code. The hash code could be used as a key
        // if it is "unique" enough. Note: The IComparable interface would need to 
        // be replaced with int.
        private readonly int             _intHashCode;
		// identifies the owner of the tree
		private readonly string          _strIdentifier;
		// the tree
		private RedBlackNode<T>	_rbTree;
        //  sentinelNode is convenient way of indicating a leaf node.
        public static RedBlackNode<T> SentinelNode;          
        // the node that was last found; used to optimize searches
		private RedBlackNode<T>	_lastNodeFound;			
		private readonly Random          _rand	= new Random();

		public RedBlack() 
        {
            _strIdentifier       = base.ToString() + _rand.Next();
            _intHashCode         = _rand.Next();

            // set up the sentinel node. the sentinel node is the key to a successfull
            // implementation and for understanding the red-black tree properties.
            SentinelNode = new RedBlackNode<T> { Left = null, Right = null, Parent = null, Color = RedBlackColors.Black };
		    _rbTree              = SentinelNode;
            _lastNodeFound       = SentinelNode;
        }
		
		public RedBlack(string strIdentifier) 
        {
			_intHashCode         = _rand.Next();
			_strIdentifier  = strIdentifier;
		}
		
		///<summary>
		/// Add
		/// args: ByVal key As IComparable, ByVal data As Object
		/// key is object that implements IComparable interface
		/// performance tip: change to use use int type (such as the hashcode)
		///</summary>
		public void Add(IComparable key, T data)
		{
			if(key == null || data == null)
				throw(new RedBlackException("RedBlackNode key and data must not be null"));
			
			// traverse tree - find where node belongs
			int result;
			// create new node
			var node	=	new RedBlackNode<T>();
			var temp	=	_rbTree;				// grab the rbTree node of the tree

			while(temp != SentinelNode)
			{	// find Parent
				node.Parent	= temp;
				result		=  key.CompareTo(temp.Key);
				if(result == 0)
					throw(new RedBlackException("A Node with the same key already exists"));
				temp = result > 0 ? temp.Right : temp.Left;
			}
			
            // setup node
			node.Key			=	key;
			node.Data			=	data;
            node.Left           =   SentinelNode;
			node.Right          =   SentinelNode;

			// insert node into tree starting at parent's location
			if(node.Parent != null)	
			{
				result	=  node.Key.CompareTo(node.Parent.Key);
				if(result > 0)
					node.Parent.Right = node;
				else
					node.Parent.Left = node;
			}
			else
				_rbTree = node;					// first node added

            RestoreAfterInsert(node);           // restore red-black properities

			_lastNodeFound = node;
			
			_intCount = _intCount + 1;
		}
        ///<summary>
        /// RestoreAfterInsert
        /// Additions to red-black trees usually destroy the red-black 
        /// properties. Examine the tree and restore. Rotations are normally 
        /// required to restore it
        ///</summary>
		private void RestoreAfterInsert(RedBlackNode<T> x)
		{   
            // x and y are used as variable names for brevity, in a more formal
            // implementation, you should probably change the names

			RedBlackNode<T> y;

			// maintain red-black tree properties after adding x
            while (x != _rbTree && x.Parent.Color == RedBlackColors.Red)
			{
				// Parent node is .Colored red; 
				if(x.Parent == x.Parent.Parent.Left)	// determine traversal path			
				{										// is it on the Left or Right subtree?
					y = x.Parent.Parent.Right;			// get uncle
                    if (y != null && y.Color == RedBlackColors.Red)
					{	// uncle is red; change x's Parent and uncle to black
                        x.Parent.Color = RedBlackColors.Black;
                        y.Color = RedBlackColors.Black;
						// grandparent must be red. Why? Every red node that is not 
						// a leaf has only black children 
                        x.Parent.Parent.Color = RedBlackColors.Red;	
						x						= x.Parent.Parent;	// continue loop with grandparent
					}	
					else
					{
						// uncle is black; determine if x is greater than Parent
						if(x == x.Parent.Right) 
						{	// yes, x is greater than Parent; rotate Left
							// make x a Left child
							x = x.Parent;
							RotateLeft(x);
						}
						// no, x is less than Parent
                        x.Parent.Color = RedBlackColors.Black;	// make Parent black
                        x.Parent.Parent.Color = RedBlackColors.Red;		// make grandparent black
						RotateRight(x.Parent.Parent);					// rotate right
					}
				}
				else
				{	// x's Parent is on the Right subtree
					// this code is the same as above with "Left" and "Right" swapped
					y = x.Parent.Parent.Left;
                    if (y != null && y.Color == RedBlackColors.Red)
					{
                        x.Parent.Color = RedBlackColors.Black;
                        y.Color = RedBlackColors.Black;
                        x.Parent.Parent.Color = RedBlackColors.Red;
						x						= x.Parent.Parent;
					}
					else
					{
						if(x == x.Parent.Left)
						{
							x = x.Parent;
							RotateRight(x);
						}
                        x.Parent.Color = RedBlackColors.Black;
                        x.Parent.Parent.Color = RedBlackColors.Red;
						RotateLeft(x.Parent.Parent);
					}
				}																													
			}
            if (_rbTree != null) _rbTree.Color = RedBlackColors.Black;		// rbTree should always be black
		}
		
		///<summary>
		/// RotateLeft
		/// Rebalance the tree by rotating the nodes to the left
		///</summary>
		public void RotateLeft(RedBlackNode<T> x)
		{
			// pushing node x down and to the Left to balance the tree. x's Right child (y)
			// replaces x (since y > x), and y's Left child becomes x's Right child 
			// (since it's < y but > x).
            
			var y = x.Right;			// get x's Right node, this becomes y

			// set x's Right link
			x.Right = y.Left;					// y's Left child's becomes x's Right child

			// modify parents
			if(y.Left != SentinelNode) 
				y.Left.Parent = x;				// sets y's Left Parent to x

            if(y != SentinelNode)
			    y.Parent = x.Parent;			// set y's Parent to x's Parent

			if(x.Parent != null)		
			{	// determine which side of it's Parent x was on
				if(x == x.Parent.Left)			
					x.Parent.Left = y;			// set Left Parent to y
				else
					x.Parent.Right = y;			// set Right Parent to y
			} 
			else 
				_rbTree = y;						// at rbTree, set it to y

			// link x and y 
			y.Left = x;							// put x on y's Left 
			if(x != SentinelNode)						// set y as x's Parent
				x.Parent = y;		
		}
		///<summary>
		/// RotateRight
		/// Rebalance the tree by rotating the nodes to the right
		///</summary>
		public void RotateRight(RedBlackNode<T> x)
		{
			// pushing node x down and to the Right to balance the tree. x's Left child (y)
			// replaces x (since x < y), and y's Right child becomes x's Left child 
			// (since it's < x but > y).
            
			var y = x.Left;			// get x's Left node, this becomes y

			// set x's Right link
			x.Left = y.Right;					// y's Right child becomes x's Left child

			// modify parents
			if(y.Right != SentinelNode) 
				y.Right.Parent = x;				// sets y's Right Parent to x

            if(y != SentinelNode)
                y.Parent = x.Parent;			// set y's Parent to x's Parent

			if(x.Parent != null)				// null=rbTree, could also have used rbTree
			{	// determine which side of it's Parent x was on
				if(x == x.Parent.Right)			
					x.Parent.Right = y;			// set Right Parent to y
				else
					x.Parent.Left = y;			// set Left Parent to y
			} 
			else 
				_rbTree = y;						// at rbTree, set it to y

			// link x and y 
			y.Right = x;						// put x on y's Right
			if(x != SentinelNode)				// set y as x's Parent
				x.Parent = y;		
		}		
		///<summary>
		/// GetData
		/// Gets the data object associated with the specified key
		///</summary>
		public object GetData(IComparable key)
		{
		    var treeNode = _rbTree;     // begin at root
            
            // traverse tree until node is found
            while(treeNode != SentinelNode)
			{
				var result = key.CompareTo(treeNode.Key);
				if(result == 0)
				{
					_lastNodeFound = treeNode;
					return treeNode.Data;
				}
				treeNode = result < 0 ? treeNode.Left : treeNode.Right;
			}
			
			throw(new RedBlackException("RedBlackNode key was not found"));
		}
		///<summary>
		/// GetMinKey
		/// Returns the minimum key value
		///</summary>
		public IComparable GetMinKey()
		{
			var treeNode = _rbTree;
			
            if(treeNode == null || treeNode == SentinelNode)
				throw(new RedBlackException("RedBlack tree is empty"));
			
            // traverse to the extreme left to find the smallest key
			while(treeNode.Left != SentinelNode)
				treeNode = treeNode.Left;
			
			_lastNodeFound = treeNode;
			
			return treeNode.Key;
			
		}
		///<summary>
		/// GetMaxKey
		/// Returns the maximum key value
		///</summary>
		public IComparable GetMaxKey()
		{
			var treeNode = _rbTree;
			
            if(treeNode == null || treeNode == SentinelNode)
                throw(new RedBlackException("RedBlack tree is empty"));

            // traverse to the extreme right to find the largest key
			while(treeNode.Right != SentinelNode)
				treeNode = treeNode.Right;

			_lastNodeFound = treeNode;

			return treeNode.Key;
			
		}
		///<summary>
		/// GetMinValue
		/// Returns the object having the minimum key value
		///</summary>
		public object GetMinValue()
		{
			return GetData(GetMinKey());
		}
		///<summary>
		/// GetMaxValue
		/// Returns the object having the maximum key
		///</summary>
		public object GetMaxValue()
		{
			return GetData(GetMaxKey());
		}
		///<summary>
		/// GetEnumerator
		/// return an enumerator that returns the tree nodes in order
		///</summary>
		public RedBlackEnumerator<T> GetEnumerator()
		{
            // elements is simply a generic name to refer to the 
            // data objects the nodes contain
            return new RedBlackEnumerator<T>(_rbTree, true);
		}

        ///<summary>
		/// IsEmpty
		/// Is the tree empty?
		///</summary>
		public bool IsEmpty()
		{
			return (_rbTree == null);
		}
		///<summary>
		/// Remove
		/// removes the key and data object (delete)
		///</summary>
		public void Remove(IComparable key)
		{
            if(key == null)
                throw(new RedBlackException("RedBlackNode key is null"));
		
			// find node
		    RedBlackNode<T> node;

			// see if node to be deleted was the last one found
			var result = key.CompareTo(_lastNodeFound.Key);
			if(result == 0)
				node = _lastNodeFound;
			else
			{	// not found, must search		
				node = _rbTree;
				while(node != SentinelNode)
				{
					result = key.CompareTo(node.Key);
					if(result == 0)
						break;
					node = result < 0 ? node.Left : node.Right;
				}

				if(node == SentinelNode)
					return;				// key not found
			}

			Delete(node);
			
			_intCount = _intCount - 1;
		}
		///<summary>
		/// Delete
		/// Delete a node from the tree and restore red black properties
		///</summary>
		private void Delete(RedBlackNode<T> z)
		{
			// A node to be deleted will be: 
			//		1. a leaf with no children
			//		2. have one child
			//		3. have two children
			// If the deleted node is red, the red black properties still hold.
			// If the deleted node is black, the tree needs rebalancing

		    RedBlackNode<T> y;					// work node 

			// find the replacement node (the successor to x) - the node one with 
			// at *most* one child. 
			if(z.Left == SentinelNode || z.Right == SentinelNode) 
				y = z;						// node has sentinel as a child
			else 
			{
				// z has two children, find replacement node which will 
				// be the leftmost node greater than z
				y = z.Right;				        // traverse right subtree	
				while(y.Left != SentinelNode)		// to find next node in sequence
					y = y.Left;
			}

			// at this point, y contains the replacement node. it's content will be copied 
			// to the valules in the node to be deleted

			// x (y's only child) is the node that will be linked to y's old parent. 
            var x = y.Left != SentinelNode ? y.Left : y.Right;					

			// replace x's parent with y's parent and
			// link x to proper subtree in parent
			// this removes y from the chain
			x.Parent = y.Parent;
			if(y.Parent != null)
				if(y == y.Parent.Left)
					y.Parent.Left = x;
				else
					y.Parent.Right = x;
			else
				_rbTree = x;			// make x the root node

			// copy the values from y (the replacement node) to the node being deleted.
			// note: this effectively deletes the node. 
			if(y != z) 
			{
				z.Key	= y.Key;
				z.Data	= y.Data;
			}

			if(y.Color == RedBlackColors.Black)
				RestoreAfterDelete(x);

			_lastNodeFound = SentinelNode;
		}

        ///<summary>
        /// RestoreAfterDelete
        /// Deletions from red-black trees may destroy the red-black 
        /// properties. Examine the tree and restore. Rotations are normally 
        /// required to restore it
        ///</summary>
		private void RestoreAfterDelete(RedBlackNode<T> x)
		{
			// maintain Red-Black tree balance after deleting node 			

			RedBlackNode<T> y;

			while(x != _rbTree && x.Color == RedBlackColors.Black) 
			{
                if (x == x.Parent.Left)			// determine sub tree from parent
                {
                    y = x.Parent.Right;			// y is x's sibling 
                    if (y.Color == RedBlackColors.Red)
                    {	// x is black, y is red - make both black and rotate
                        y.Color = RedBlackColors.Black;
                        x.Parent.Color = RedBlackColors.Red;
                        RotateLeft(x.Parent);
                        y = x.Parent.Right;
                    }
                    if (y.Left.Color == RedBlackColors.Black &&
                        y.Right.Color == RedBlackColors.Black)
                    {	// children are both black
                        y.Color = RedBlackColors.Red;		// change parent to red
                        x = x.Parent;					// move up the tree
                    }
                    else
                    {
                        if (y.Right.Color == RedBlackColors.Black)
                        {
                            y.Left.Color = RedBlackColors.Black;
                            y.Color = RedBlackColors.Red;
                            RotateRight(y);
                            y = x.Parent.Right;
                        }
                        y.Color = x.Parent.Color;
                        x.Parent.Color = RedBlackColors.Black;
                        y.Right.Color = RedBlackColors.Black;
                        RotateLeft(x.Parent);
                        x = _rbTree;
                    }
                }
                else
                {
                    // right subtree - same as code above with right and left swapped
                    y = x.Parent.Left;
                    if (y.Color == RedBlackColors.Red)
                    {
                        y.Color = RedBlackColors.Black;
                        x.Parent.Color = RedBlackColors.Red;
                        RotateRight(x.Parent);
                        y = x.Parent.Left;
                    }
                    if (y.Right.Color == RedBlackColors.Black &&
                        y.Left.Color == RedBlackColors.Black)
                    {
                        y.Color = RedBlackColors.Red;
                        x = x.Parent;
                    }
                    else
                    {
                        if (y.Left.Color == RedBlackColors.Black)
                        {
                            y.Right.Color = RedBlackColors.Black;
                            y.Color = RedBlackColors.Red;
                            RotateLeft(y);
                            y = x.Parent.Left;
                        }
                        y.Color = x.Parent.Color;
                        x.Parent.Color = RedBlackColors.Black;
                        y.Left.Color = RedBlackColors.Black;
                        RotateRight(x.Parent);
                        x = _rbTree;
                    }
                }
			}
            if (x != null) x.Color = RedBlackColors.Black;
		}
		
		///<summary>
		/// RemoveMin
		/// removes the node with the minimum key
		///</summary>
		public void RemoveMin()
		{
            if(_rbTree == null)
                throw(new RedBlackException("RedBlackNode is null"));
            
             Remove(GetMinKey());
		}
		///<summary>
		/// RemoveMax
		/// removes the node with the maximum key
		///</summary>
		public void RemoveMax()
		{
            if(_rbTree == null)
                throw(new RedBlackException("RedBlackNode is null"));
            
            Remove(GetMaxKey());
		}
		///<summary>
		/// Clear
		/// Empties or clears the tree
		///</summary>
		public void Clear ()
		{
			_rbTree      = SentinelNode;
			_intCount    = 0;
		}
		///<summary>
		/// Size
		/// returns the size (number of nodes) in the tree
		///</summary>
		public int Size()
		{
			// number of keys
			return _intCount;
		}
		///<summary>
		/// Equals
		///</summary>
		public override bool Equals(object obj)
		{
			if(obj == null)
				return false;
			
			if(!(obj is RedBlackNode<T> ))
				return false;
			
			return this == obj || (ToString().Equals(obj.ToString()));
		}
		///<summary>
		/// HashCode
		///</summary>
		public override int GetHashCode()
		{
			return _intHashCode;
		}
		///<summary>
		/// ToString
		///</summary>
		public override string ToString()
		{
			return _strIdentifier;
		}
	}
}
