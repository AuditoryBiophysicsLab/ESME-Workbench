using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ESME
{
    public static class Delegates
    {
        public delegate void Delegate<in T>(T param);
    }
}
