using System;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.IO;
using System.Linq;
using System.Windows.Controls;
using Cinch;
using ESME.Environment;
using ESME.NEMO;
using HRC.Navigation;
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
            PropertyChanged += (s, e) => { if (e.PropertyName == "IsValid") IsConfigured = IsValid; };
            ConfigurationDirectory = Path.Combine(System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData), "ESME Workbench\\Plugins");
            if (!Directory.Exists(ConfigurationDirectory)) Directory.CreateDirectory(ConfigurationDirectory);
        }

        public string PluginName { get; protected set; }
        public string PluginDescription { get; protected set; }
        public PluginType PluginType { get; protected set; }
        public string DLLPath { get; set; }
        public string Subtype { get; protected set; }
        protected string ConfigurationDirectory { get; set; }
        protected virtual string ConfigurationFile { get { return Path.Combine(ConfigurationDirectory, PluginName + ".xml"); } }

        #region public Control ConfigurationControl { get; protected set; }

        public Control ConfigurationControl
        {
            get { return _configurationControl; }
            protected set
            {
                if (_configurationControl == value) return;
                _configurationControl = value;
                NotifyPropertyChanged(ConfigurationControlChangedEventArgs);
                NotifyPropertyChanged(HasConfigurationControlChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ConfigurationControlChangedEventArgs = ObservableHelper.CreateArgs<PluginBase>(x => x.ConfigurationControl);
        Control _configurationControl;

        #endregion
        #region public bool IsConfigurable { get; }

        public bool IsConfigurable
        {
            get { return ConfigurationControl != null; }
        }

        static readonly PropertyChangedEventArgs HasConfigurationControlChangedEventArgs = ObservableHelper.CreateArgs<PluginBase>(x => x.IsConfigurable);

        #endregion
        #region public bool IsSelectable { get; protected set; }

        public bool IsSelectable
        {
            get { return _isSelectable; }
            protected set
            {
                if (_isSelectable == value) return;
                _isSelectable = value;
                NotifyPropertyChanged(IsAvailableChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsAvailableChangedEventArgs = ObservableHelper.CreateArgs<PluginBase>(x => x.IsSelectable);
        bool _isSelectable;

        #endregion
        #region public bool IsConfigured { get; protected set; }

        public bool IsConfigured
        {
            get { return _isConfigured; }
            protected set
            {
                if (_isConfigured == value) return;
                _isConfigured = value;
                NotifyPropertyChanged(IsConfiguredChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsConfiguredChangedEventArgs = ObservableHelper.CreateArgs<PluginBase>(x => x.IsConfigured);
        bool _isConfigured;

        #endregion
    }

    public abstract class EnvironmentalDataSourcePluginBase<T> : PluginBase, IEnvironmentalDataSource<T>
    {
        /// <summary>
        /// An array of available resolutions, expressed in arc-minutes per sample
        /// </summary>
        public float[] AvailableResolutions { get; protected set; }
        public bool IsTimeVariantData { get; protected set; }
        public TimePeriod[] AvailableTimePeriods { get; protected set; }
        public abstract T Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, IProgress<float> progress = null);
        protected void CheckResolutionAndTimePeriod(float resolution, TimePeriod timePeriod)
        {
            if (!AvailableTimePeriods.Contains(timePeriod)) throw new ParameterOutOfRangeException(string.Format("Specified timePeriod is not available in the {0} data set", PluginName));
            if (!AvailableResolutions.Contains(resolution)) throw new ParameterOutOfRangeException(string.Format("Specified resolution is not available in the {0} data set", PluginName));
        }

        protected void SetPropertiesFromAttributes(Type type)
        {
            var pluginAttribute = (ESMEPluginAttribute)type.GetCustomAttributes(typeof(ESMEPluginAttribute), false)[0];
            PluginType = pluginAttribute.PluginType;
            Subtype = pluginAttribute.Subtype;
            PluginName = pluginAttribute.Name;
            PluginDescription = pluginAttribute.Description;
        }
    }

    [MetadataAttribute]
    [AttributeUsage(AttributeTargets.Class, AllowMultiple = false)]
    public class ESMEPluginAttribute : ExportAttribute
    {
        public ESMEPluginAttribute() : base(typeof(IESMEPlugin)) { }

        public PluginType PluginType { get; set; }
        public string Subtype { get; set; }
        public string Name { get; set; }
        public string Description { get; set; }
    }

}
