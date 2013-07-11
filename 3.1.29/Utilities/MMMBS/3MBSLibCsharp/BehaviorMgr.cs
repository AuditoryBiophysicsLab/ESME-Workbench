using System;
using System.Collections.Generic;
using System.Collections;
using System.Text;
using System.Diagnostics;


namespace MMMBSLib
{
    public class CBehaviorListMgr
    {
        ArrayList m_list = new ArrayList(); // List of behaviors

        public int Count { get { return m_list.Count; } }
        public string[] NameArray { get { return GenerateBehaviorNameArray(); } }

        // Constructor
        public CBehaviorListMgr() { }

        public CBehavior AddBehavior(Boolean UpdateModel)
        {
            CBehavior beh;

            beh = new CBehavior(); // +1 for the new behavior being added.
            m_list.Add(beh);

            if(UpdateModel == false)
                return beh;

            // Initially there is only a single depth span for the new behavior.  Set the
            // single span's shallow and deep values to default.
            Debug.Assert(beh.SpanManager.SpanCount == 1);
            beh.SpanManager.SetSpan(0, 0, -3500); // index 0 because first and only span since it is a new behavior.

            // Set the behavior's default name.
            beh.name = "Behavior " + (m_list.Count);

            //--------------------------------------------------------------------------//
            // Update Behavior Transition Matrices
            //------------------------------------//
            // For each behavior add a column to each of its normal behavior depth span 
            // as well as its depth environmental attractor and temperature environmental
            // attractor.  Each added behavior gets an added column.
            for(int i=0; i<m_list.Count; i++) //
            {
                // Increment the environmental attractor behavior transtion matrices
                beh = (CBehavior)m_list[i];

                while(beh.depthBehTrans.vector.columnCount < Count + 1)
                    beh.IncrementTransitionBehaviorVectors();

                // Increment each spans normal behavior transtion matrices
                for(int j=0; j<beh.SpanManager.SpanCount; j++)  /*Count is the number of behaviors*/
                    while(beh.SpanManager.GetSpan(j).behTrans.transitionCount < Count)
                        beh.SpanManager.GetSpan(j).IncrementSpanBehaviorTransitionVectorElement();
            }

            // Return the newly added behavior.
            return (CBehavior)m_list[Count-1];
        }

        public Boolean RemoveBehavior(int BehaviorIndex)
        {
            int i;
            CSpanMgr mgr;
            if(CUtil.CheckIndex(m_list.Count, BehaviorIndex) == false)
                return false;

            // Must update the matrices before deleting the behavior for indexing reasons.
            for(i=0; i<m_list.Count; i++)
            {
                if(false == ((CBehavior)m_list[i]).DecrementEnvironmentalTransitionBehaviorVectorElement(BehaviorIndex))
                    return false;
            }

            for(i=0; i<m_list.Count; i++)
            {
                mgr = ((CBehavior)m_list[i]).SpanManager;
                mgr.DecrementAllSpanBehaviorTransitionVectorElements(BehaviorIndex);
                //mgr = 
                //mgr = .SpanManager;
                //for(j=0; j<beh.SpanManager; j++)
               // {
                //}
            }
            // Now safe to delete the behavior
            m_list.RemoveAt(BehaviorIndex);
            return true;
        }

        public CBehavior GetBehavior(int Index)
        {
            if(CUtil.CheckIndex(m_list.Count, Index) == false)
                return null;
            return (CBehavior)m_list[Index];
        }

        public Boolean ReplaceBehavior(int Index, CBehavior Replacement)
        {
            if(false == CUtil.CheckIndex(Count, Index))
                return false;
            m_list[Index] = Replacement;
            return true;
        }

        private string[] GenerateBehaviorNameArray()
        {
            string[] sz = new string[m_list.Count];
            for(int i=0; i<m_list.Count; i++)
                sz[i] = ((CBehavior)m_list[i]).name;
            return sz;
        }
    }
}