using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Xml.Serialization;
using HRC.Aspects;
using HRC.Utility;
using HRC.Validation;

namespace ESME.Settings
{
    public interface ISettingsBase
    {
        void Save(string fileName);
        string SettingsFilename { get; }
    }

    [Serializable, NotifyPropertyChanged]
    public abstract class SettingsBase : ValidatingViewModel, ISettingsBase
    {
        public abstract void Save(string fileName);

        [XmlIgnore]
        public string SettingsFilename { get; protected set; }
    }

}