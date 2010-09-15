using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ESMEWorkBench.AppSettings
{
    public class ApplicationSetting<T> where T : IComparable<T>
    {
        T _value;

        public ApplicationSetting(ref T settingRef)
        {
            var tmp = new WeakReference(settingRef, false);
        }

        T Value
        {
            get { return _value; }
            set
            {
                if (value.Equals(_value)) return;
                _value = value;
            }
        }

    }
}
