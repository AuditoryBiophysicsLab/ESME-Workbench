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
    public class TreeViewItemPropertiesViewModel : ViewModelBase
    {
        [Initialize]
        public List<EditableKeyValuePair<string, object>> Properties { get; set; }
        public string WindowTitle { get; set; }


        #region commands

        #region CancelCommand
        public SimpleCommand<object, EventToCommandArgs> CancelCommand { get { return _cancel ?? (_cancel = new SimpleCommand<object, EventToCommandArgs>(o=>CloseDialog(false))); } }
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
        #region public AnalysisPoint AnalysisPoint { get; set; }
        AnalysisPoint _analysisPoint;

        public AnalysisPoint AnalysisPoint
        {
            get { return _analysisPoint; }
            set
            {
                _analysisPoint = value;
                GenerateProperties();
            }
        }
        #endregion

        void GenerateProperties()
        {
            WindowTitle = "Analysis Point Properties";
            Properties.Add(new EditableKeyValuePair<string, object>("Location:", AnalysisPoint.Geo.ToString()));
            Properties.Add(new EditableKeyValuePair<string, object>("Number of Platforms:", AnalysisPoint.Scenario.Platforms.Count.ToString(CultureInfo.InvariantCulture)));
            Properties.Add(new EditableKeyValuePair<string, object>("Number of Transmission Losses:", AnalysisPoint.TransmissionLosses.Count.ToString(CultureInfo.InvariantCulture)));
            Properties.Add(new EditableKeyValuePair<string, object>("Number of Computed Radials:",
                                                                    AnalysisPoint.TransmissionLosses.SelectMany(r => r.Radials).Count().ToString(CultureInfo.InvariantCulture)));
            Properties.Add(new EditableKeyValuePair<string, object>("Size on Disk:", AnalysisPointSize));
        }

        string AnalysisPointSize
        {
            get
            {
                var size = AnalysisPoint.TransmissionLosses.Sum(tl => tl.Radials.Select(radial => Directory.GetFiles(Path.GetDirectoryName(radial.BasePath), Path.GetFileName(radial.BasePath + ".*"))).Select(files => files.Select(file => new FileInfo(file)).Select(fi => fi.Length).Sum()).Sum());

                switch ((int)Math.Log10(size))
                {
                    case 0:
                    case 1:
                    case 2:
                        return string.Format("{0}b", size);
                    case 3:
                    case 4:
                    case 5:
                        return string.Format("{0}K", size >> 10);
                    case 6:
                    case 7:
                    case 8:
                        return string.Format("{0}M", size >> 20);
                    case 9:
                    case 10:
                    case 11:
                        return string.Format("{0}G", size >> 30);
                    case 12:
                    case 13:
                    case 14:
                        return string.Format("{0}T", size >> 40);
                    default:
                        return string.Format("{0}b", size);
                }
            }
        }
    }

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