using System;
using System.ComponentModel;
using System.Windows.Controls;
using Cinch;
using ESME.Environment;
using ESME.Environment.NAVO;
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
        }

        public string PluginName { get; protected set; }
        public string PluginDescription { get; protected set; }
        public PluginType PluginType { get; protected set; }

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
        #region public bool HasConfigurationControl { get; set; }

        public bool HasConfigurationControl
        {
            get { return ConfigurationControl != null; }
        }

        static readonly PropertyChangedEventArgs HasConfigurationControlChangedEventArgs = ObservableHelper.CreateArgs<PluginBase>(x => x.HasConfigurationControl);

        #endregion
        #region public bool IsAvailable { get; protected set; }

        public bool IsAvailable
        {
            get { return _isAvailable; }
            protected set
            {
                if (_isAvailable == value) return;
                _isAvailable = value;
                NotifyPropertyChanged(IsAvailableChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsAvailableChangedEventArgs = ObservableHelper.CreateArgs<PluginBase>(x => x.IsAvailable);
        bool _isAvailable;

        #endregion

    }

    public abstract class EnvironmentalDataSourcePluginBase<T> : PluginBase, IEnvironmentalDataSource<T>
    {
        /// <summary>
        /// An array of available resolutions in samples per degree
        /// </summary>
        public float[] Resolutions { get; protected set; }
        public virtual string DataLocation { get; set; }
        public string DataLocationHelp { get; protected set; }
        #region public bool IsDataLocationValid { get; protected set; }

        public bool IsDataLocationValid
        {
            get { return _isDataLocationValid; }
            protected set
            {
                _isDataLocationValid = value;
                NotifyPropertyChanged(IsDataLocationValidChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsDataLocationValidChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentalDataSourcePluginBase<T>>(x => x.IsDataLocationValid);
        bool _isDataLocationValid;

        #endregion


        public abstract T Extract(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, IProgress<float> progress = null);
    }
}
