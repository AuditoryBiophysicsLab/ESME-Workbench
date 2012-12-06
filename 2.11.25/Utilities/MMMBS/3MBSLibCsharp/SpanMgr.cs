using System;
using System.Collections.Generic;
using System.Collections;
using System.Text;
using System.Diagnostics;


namespace MMMBSLib
{
    abstract public class CSpanMgr
    {
        //-----------------------------------------------------------------------------//
        // Member Variables
        //-----------------//
        protected ArrayList m_list = new ArrayList(); // contains CDepthSpan instances.

        //-----------------------------------------------------------------------------//
        // Properties
        //-----------//
        public int SpanCount { get { return m_list.Count; } }

        //-----------------------------------------------------------------------------//
        // Methods
        //--------//
        public abstract CSpan AddSpan();
        public int AddSpan(CSpan DepthSpan) { return m_list.Add(DepthSpan); }

        // Returns the Index passed in if successful or -1 otherwise.
        public Boolean InsertSpan(int Index, CSpan DepthSpan)
        {
            if(false == CUtil.CheckIndex(m_list.Count, Index))
                return false;
            m_list.Add(null);
            m_list.Insert(Index, DepthSpan);
            return true;
        }

        // Returns index deleted or -1 of invalid index passed in.
        public Boolean DeleteSpan(int Index)
        {
            if(false == CUtil.CheckIndex(m_list.Count, Index))
                return false;
            m_list.RemoveAt(Index);
            return true;
        }

        public Boolean SwapSpans(int Index1, int Index2)
        {
            CSpan span1;
            if(!CUtil.CheckIndex(m_list.Count, Index1) || !CUtil.CheckIndex(m_list.Count, Index2))
                return false;
            span1 = (CSpan)m_list[Index1];
            m_list[Index1] = (CSpan)m_list[Index2];
            m_list[Index2] = span1;
            return true;
        }

        public CSpan GetSpan(int Index)
        {
            if(false == CUtil.CheckIndex(m_list.Count, Index))
                return null;
            return (CSpan)m_list[Index];
        }

        public Boolean SetSpan(int Index, double Shallow, double Deep)
        {
            if(false == CUtil.CheckIndex(m_list.Count, Index))
                return false;
            ((CSpan)m_list[Index]).shallow = Shallow;
            ((CSpan)m_list[Index]).deep = Deep;
            return true;
        }

        //public abstract CSpanMgr GetCopy();

/*        public CSpanMgr GetCopy()
        {
            CSpanMgr c = new CSpanMgr();
            for(int i=0; i<m_list.Count; i++)
                c.m_list.Add(((CSpan)m_list[i]).GetCopy());
            return c;
        }
*/
        public void IncrementAllSpanTransitionBehaviorVectorElements()
        {
            for(int i=0; i<m_list.Count; i++)
                ((CSpan)m_list[i]).IncrementSpanBehaviorTransitionVectorElement();
        }

        public Boolean DecrementAllSpanBehaviorTransitionVectorElements(int BehaviorIndex)
        {
            for(int i=0; i<m_list.Count; i++)
            {
                if(false == ((CSpan)m_list[i]).DecrmentSpanBehaviorTransitionVectorElement(BehaviorIndex))
                    return false;
            }
            return true;
        }
        public void DeleteAllSpans() { m_list.Clear(); }
    }

    // Behavior Transition Span Manager
    public class CBehaviorTransitionSpanMgr: CSpanMgr
    {
        //-----------------------------------------------------------------------------//
        // Constructor
        //------------//
        public CBehaviorTransitionSpanMgr() { }

        public override CSpan AddSpan()
        {
            return (CSpan)m_list[m_list.Add(new CSpanBehTrans())];
        }

        public CBehaviorTransitionSpanMgr GetCopy()
        {
            CBehaviorTransitionSpanMgr c = new CBehaviorTransitionSpanMgr();
            for(int i=0; i<m_list.Count; i++)
                c.m_list.Add(((CSpanBehTrans)m_list[i]).GetCopy());
            return c;
        }


    }

    // Initial Behavior Span Manager
    public class CInitialBehaviorSpanMgr : CSpanMgr
    {
        //-----------------------------------------------------------------------------//
        // Constructor
        //------------//
        public CInitialBehaviorSpanMgr(){}


        public override CSpan AddSpan()
        {
            return (CSpan)m_list[m_list.Add(new CSpanInitialBeh())];
        }

        public CInitialBehaviorSpanMgr GetCopy()
        {
            CInitialBehaviorSpanMgr c = new CInitialBehaviorSpanMgr();
            for(int i=0; i<m_list.Count; i++)
                c.m_list.Add(((CSpanInitialBeh)m_list[i]).GetCopy());
            return c;
        }


    }
}
