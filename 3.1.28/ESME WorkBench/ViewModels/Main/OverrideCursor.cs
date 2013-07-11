using System;
using System.Collections.Generic;
using System.Windows.Input;

namespace ESMEWorkbench.ViewModels.Main
{
    public class OverrideCursor : IDisposable
    {
        static readonly Stack<Cursor> SStack = new Stack<Cursor>();

        public OverrideCursor(Cursor changeToCursor)
        {
            SStack.Push(changeToCursor);

            if (Mouse.OverrideCursor != changeToCursor) Mouse.OverrideCursor = changeToCursor;
        }

        #region IDisposable Members

        public void Dispose()
        {
            SStack.Pop();

            Cursor cursor = SStack.Count > 0 ? SStack.Peek() : null;

            if (cursor != Mouse.OverrideCursor) Mouse.OverrideCursor = cursor;
        }

        #endregion
    }
}