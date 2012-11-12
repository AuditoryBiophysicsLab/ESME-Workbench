using System;
using System.IO;
using System.Linq;
using System.Windows.Controls;
using System.Xml.Serialization;
using ESME.Environment;
using ESME.Locations;
using ESME.NEMO;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;
using HRC.Validation;

namespace ESME.Plugins
{
    public abstract class PluginBase : ValidatingViewModel, IESMEPlugin
    {
        protected PluginBase() 
        {
            PluginName = "Not set!";
            PluginDescription = "Not set!";
            ConfigurationControl = null;
            PluginType = PluginType.Unknown;
            ConfigurationDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), "ESME Workbench\\Plugins");
            if (!Directory.Exists(ConfigurationDirectory)) Directory.CreateDirectory(ConfigurationDirectory);
            PropertyChanged += (s, e) =>
            {
                switch (e.PropertyName)
                {
                    case "IsValid":
                        IsConfigured = IsValid;
                        return;
                    case "IsConfigured":
                        return;
                }
            };
        }

        [XmlIgnore] public Control SelectionControl { get; protected set; }
        [XmlIgnore] public string PluginName { get; protected set; }
        [XmlIgnore] public string PluginDescription { get; protected set; }
        [XmlIgnore] public PluginType PluginType { get; protected set; }
        [XmlIgnore] public string DLLPath { get; set; }
        [XmlIgnore] public PluginSubtype PluginSubtype { get; protected set; }
        [XmlIgnore] protected string ConfigurationDirectory { get; set; }
        [XmlIgnore] protected virtual string ConfigurationFile { get { return Path.Combine(ConfigurationDirectory, PluginName + ".xml"); } }
        [XmlIgnore]
        public virtual PluginIdentifier PluginIdentifier
        {
            get
            {
                return new PluginIdentifier
                {
                    PluginType = PluginType,
                    PluginSubtype = PluginSubtype,
                    Type = GetType().ToString(),
                };
            }
        }

        [Affects("IsConfigurable")]
        [XmlIgnore] public Control ConfigurationControl { get; protected set; }
        [XmlIgnore] public bool IsConfigurable { get { return ConfigurationControl != null; } }
        [XmlIgnore] public bool IsSelectable { get; protected set; }
        [XmlIgnore] public virtual bool IsConfigured { get; protected set; }

        protected abstract void Save();
        public abstract void LoadSettings();
    }

    [Serializable]
    public class TransmissionLossCalculatorPluginBase : PluginBase
    {
        protected override void Save() { }
        public override void LoadSettings() { }
    }

    [Serializable]
    public abstract class EnvironmentalDataSourcePluginBase : PluginBase
    {
        protected EnvironmentalDataSourcePluginBase() { SelectionControl = null; }
        /// <summary>
        /// An array of available resolutions, expressed in arc-minutes per sample
        /// </summary>
        [XmlIgnore] public float[] AvailableResolutions { get; protected set; }
        [XmlIgnore] public bool IsTimeVariantData { get; protected set; }
        [XmlIgnore] public TimePeriod[] AvailableTimePeriods { get; protected set; }
        [XmlIgnore] public EnvironmentDataType EnvironmentDataType { get; protected set; }
        [XmlIgnore] public object SelectionControlViewModel { get; protected set; }
        [XmlIgnore] public abstract EnvironmentalDataSet SelectedDataSet { get; }

        protected void CheckResolutionAndTimePeriod(float resolution, TimePeriod timePeriod)
        {
            if (!AvailableTimePeriods.Contains(timePeriod)) throw new ParameterOutOfRangeException(string.Format("Specified timePeriod is not available in the {0} data set", PluginName));
            if (!AvailableResolutions.Contains(resolution)) throw new ParameterOutOfRangeException(string.Format("Specified resolution is not available in the {0} data set", PluginName));
        }
        protected override void Save()
        {
        }

        public override void LoadSettings()
        {
        }

        protected void SetPropertiesFromAttributes(Type type)
        {
            var pluginAttribute = (EnvironmentDataSourceAttribute)type.GetCustomAttributes(typeof(ESMEPluginAttribute), false)[0];
            PluginType = PluginType.EnvironmentalDataSource;
            EnvironmentDataType = pluginAttribute.EnvironmentDataType;
            switch (EnvironmentDataType)
            {
                case EnvironmentDataType.Wind:
                    PluginSubtype = PluginSubtype.Wind;
                    break;
                case EnvironmentDataType.SoundSpeed:
                    PluginSubtype = PluginSubtype.SoundSpeed;
                    break;
                case EnvironmentDataType.Sediment:
                    PluginSubtype = PluginSubtype.Sediment;
                    break;
                case EnvironmentDataType.Bathymetry:
                    PluginSubtype = PluginSubtype.Bathymetry;
                    break;
            }
            PluginName = pluginAttribute.Name;
            PluginDescription = pluginAttribute.Description;
        }
    }

    public abstract class EnvironmentalDataSourcePluginBase<T> : EnvironmentalDataSourcePluginBase, IEnvironmentalDataSource<T>
    {
        public abstract T Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, PercentProgress progress = null);
    }
    
}
