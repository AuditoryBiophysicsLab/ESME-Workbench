using System;
using System.Xml.Serialization;
using HRC.Utility;
using HRC.Validation;

namespace ESME.Settings
{
    public interface ISettingsBase
    {
        void Save(string fileName = null);
        string SettingsFilename { get; }
    }

    [Serializable]
    public abstract class SettingsBase : ValidatingViewModel, ISettingsBase
    {
        protected virtual void SaveBase<T>(T settings, string fileName = null) where T: class, new()
        {
            if (fileName != null) SettingsFilename = fileName;
            if (SettingsFilename == null) throw new NullReferenceException("SettingsFilename cannot be null");
            StaticXmlSerializer.Save(SettingsFilename, settings);
        }

        public abstract void Save(string fileName = null);

        [XmlIgnore]
        public string SettingsFilename { get; protected set; }
    }

}