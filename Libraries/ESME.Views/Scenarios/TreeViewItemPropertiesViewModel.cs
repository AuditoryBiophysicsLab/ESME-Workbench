using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using ESME.Scenarios;
using ESME.Views.Controls;
using ESME.Views.Locations;
using HRC.Aspects;
using HRC.Validation;
using HRC.ViewModels;
using HRC.WPF;
using ValidationRule = HRC.Validation.ValidationRule;

namespace ESME.Views.Scenarios
{
    /// <summary>
    ///   To create and show the view as a dialog: var vm = new TreeViewItemPropertiesViewModel {...}; var result = _visualizerService.ShowDialog("TreeViewItemPropertiesView", vm); if ((!result.HasValue) || (!result.Value)) return; To create and show the view as a window: var vm = new TreeViewItemPropertiesViewModel {...}; var window = _visualizerService.ShowWindow("TreeViewItemPropertiesView", vm);
    /// </summary>
    public abstract class TreeViewItemPropertiesViewModel : ViewModelBase
    {
        [Initialize]
        public List<EditableKeyValuePair<string, string>> Properties { get; set; }
        public string WindowTitle { get; set; }
        protected abstract void GenerateProperties();

        internal static string FormattedSize(long size)
        {
            switch ((int)Math.Log10(size))
            {
                case 0:
                case 1:
                case 2:
                    return String.Format("{0}b", size);
                case 3:
                case 4:
                case 5:
                    return String.Format("{0}K", size >> 10);
                case 6:
                case 7:
                case 8:
                    return String.Format("{0}M", size >> 20);
                case 9:
                case 10:
                case 11:
                    return String.Format("{0}G", size >> 30);
                case 12:
                case 13:
                case 14:
                    return String.Format("{0}T", size >> 40);
                default:
                    return String.Format("{0}b", size);
            }
        }

        #region public Object PropertyObject { get; set; }
        object _object;
        public object PropertyObject
        {
            get { return _object; }
            set
            {
                _object = value;
                GenerateProperties();
            }
        }
        #endregion

        #region commands

        #region CancelCommand
        public SimpleCommand<object, EventToCommandArgs> CancelCommand { get { return _cancel ?? (_cancel = new SimpleCommand<object, EventToCommandArgs>(o => CloseDialog(false))); } }
        SimpleCommand<object, EventToCommandArgs> _cancel;
        #endregion

        #region OkCommand
        public SimpleCommand<object, EventToCommandArgs> OkCommand { get { return _ok ?? (_ok = new SimpleCommand<object, EventToCommandArgs>(o => CloseDialog(true))); } }
        SimpleCommand<object, EventToCommandArgs> _ok;
        #endregion

        #region ViewClosingCommand
        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand { get { return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(o => Views.Properties.Settings.Default.Save())); } }
        SimpleCommand<object, EventToCommandArgs> _viewClosing;
        #endregion

        #endregion
    }

    public class AnalysisPointPropertiesViewModel : TreeViewItemPropertiesViewModel
    {
        protected override void GenerateProperties()
        {
            var propOpj = (AnalysisPoint)PropertyObject;
            WindowTitle = "Analysis Point Properties: " + propOpj.Geo;
            Properties.Add(new EditableKeyValuePair<string, string>("Location:", propOpj.Geo.ToString()));
            Properties.Add(new EditableKeyValuePair<string, string>("Number of Platforms:", propOpj.Scenario.Platforms.Count.ToString(CultureInfo.InvariantCulture)));
            Properties.Add(new EditableKeyValuePair<string, string>("Number of Transmission Losses:", propOpj.TransmissionLosses.Count.ToString(CultureInfo.InvariantCulture)));
            Properties.Add(new EditableKeyValuePair<string, string>("Number of Computed Radials:", propOpj.TransmissionLosses.SelectMany(r => r.Radials).Count().ToString(CultureInfo.InvariantCulture)));
            Properties.Add(new EditableKeyValuePair<string, string>("Size on Disk:", AnalysisPointSize));
        }

        string AnalysisPointSize
        {
            get
            {
                var propOpj = (AnalysisPoint)PropertyObject;
                var size = propOpj.TransmissionLosses.Sum(tl => tl.Radials.Select(radial => Directory.GetFiles(Path.GetDirectoryName(radial.BasePath), Path.GetFileName(radial.BasePath + ".*"))).Select(files => files.Select(file => new FileInfo(file)).Select(fi => fi.Length).Sum()).Sum());
                return FormattedSize(size);
            }
        }
    }

    public class ScenarioPropertiesViewModel : TreeViewItemPropertiesViewModel
    {
        protected override void GenerateProperties()
        {
            var obj = (Scenario)PropertyObject;
            WindowTitle = "Scenario Properties: " + obj.Name;
            Properties.Add(new EditableKeyValuePair<string, string>("Location:", obj.Location.Name));
            Properties.Add(new EditableKeyValuePair<string, string>("Number of Platforms:", obj.Platforms.Distinct().Count().ToString(CultureInfo.InvariantCulture)));
            Properties.Add(new EditableKeyValuePair<string, string>("Number of Sources:", obj.Platforms.SelectMany(p => p.Sources).Distinct().Count().ToString(CultureInfo.InvariantCulture)));
            Properties.Add(new EditableKeyValuePair<string, string>("Number of Modes:", obj.Platforms.SelectMany(p => p.Sources.SelectMany(m => m.Modes).Distinct()).Distinct().Count().ToString(CultureInfo.InvariantCulture)));
            Properties.Add(new EditableKeyValuePair<string, string>("Acoustic Data Size on Disk:", ScenarioAcousticSize));
            Properties.Add(new EditableKeyValuePair<string, string>("Environmental Data Size on Disk: ", ScenarioEnvironmentalSize));
        }

        string ScenarioAcousticSize
        {
            get
            {
                long size = 0;
                var obj = (Scenario)PropertyObject;
                foreach (var point in obj.AnalysisPoints)
                {
                    foreach (var loss in point.TransmissionLosses)
                    {
                        foreach (var radial in loss.Radials)
                        {
                            var files = Directory.GetFiles(Path.GetDirectoryName(radial.BasePath), Path.GetFileName(radial.BasePath + ".*"));
                            foreach (var file in files)
                            {
                                var info = new FileInfo(file);
                                size += info.Length;
                            }
                        }
                    }
                }
                return FormattedSize(size);
            }
        }

        string ScenarioEnvironmentalSize
        {
            get
            {
                var obj = (Scenario)PropertyObject;
                return FormattedSize(obj.Wind.FileSize + obj.SoundSpeed.FileSize + obj.Sediment.FileSize + obj.Bathymetry.FileSize);
            }
        }
    }


    internal class PropertiesViewHelpers
    { }
#if false
    public class ModePropertiesViewModel : TreeViewItemPropertiesViewModel
    {
        Mode _mode;
        public Mode Mode
        {
            get { return _mode; }
            set
            {
                _mode = value;
                GenerateProperties();
                WindowTitle = "Mode Properties: " + _mode.ModeName;
            }
        }

        void GenerateProperties()
        {

            Properties.Add(new EditableKeyValuePair<string, object>("Mode Type", "", true));
            Properties.Add(new EditableKeyValuePair<string, object>("Depth offset (m)", "", true));
            Properties.Add(new EditableKeyValuePair<string, object>("Source level (dB)", "", true));
            Properties.Add(new EditableKeyValuePair<string, object>("Frequency (Hz)", "", true));
            Properties.Add(new EditableKeyValuePair<string, object>("Vertical beam width (deg)", "", true));
            Properties.Add(new EditableKeyValuePair<string, object>("Depression/Elevation angle (deg)", "", true));
            Properties.Add(new EditableKeyValuePair<string, object>("Maximum propagation radius (m)", "", true));
        }
    }

    public class SourcePropertiesViewModel : TreeViewItemPropertiesViewModel
    {
        Source _source;
        public Source Source
        {
            get { return _source; }
            set
            {
                _source = value;
                GenerateProperties();
                WindowTitle = "Source Properties: " + _source.SourceName;
            }
        }

        void GenerateProperties()
        {
            //Properties.Add(new EditableKeyValuePair<string, string>("Source Type:", Source.SourceType ?? "N/A"));
            Properties.Add(new EditableKeyValuePair<string, object>("Source Type:", "", true));
        }
    }

    public class PlatformPropertiesViewModel : TreeViewItemPropertiesViewModel
    {
        Platform _platform;
        public Platform Platform
        {
            get { return _platform; }
            set
            {
                _platform = value;
                GenerateProperties();
                WindowTitle = "Platform Properties: " + Platform.PlatformName;
            }
        }

        void GenerateProperties()
        {
            Properties.Add(new EditableKeyValuePair<string, object>("Platform Type: ", "", true));
            Properties.Add(new EditableKeyValuePair<string, object>("Description: ", "", true));
        }
    } 
#endif

}