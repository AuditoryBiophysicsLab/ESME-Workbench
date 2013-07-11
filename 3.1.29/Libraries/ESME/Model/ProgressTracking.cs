using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ESME.Model
{
    public class ProgressChangedEventArgs : EventArgs
    {
        public string JobDescription { get; set; }
        public float CurrentProgress { get; set; }
        public float ProgressWhenComplete { get; set; }
    }
}
