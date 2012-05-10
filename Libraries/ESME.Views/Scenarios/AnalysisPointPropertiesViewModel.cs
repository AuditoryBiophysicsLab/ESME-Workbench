using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using ESME.Scenarios;
using HRC.Aspects;
using HRC.Services;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.Scenarios
{
    /// <summary>
    /// To create and show the view as a dialog:
    /// var vm = new AnalysisPointPropertiesViewModel {...};
    /// var result = _visualizerService.ShowDialog("AnalysisPointPropertiesView", vm);
    /// if ((!result.HasValue) || (!result.Value)) return;
    /// 
    /// To create and show the view as a window:
    /// var vm = new AnalysisPointPropertiesViewModel {...};
    /// var window = _visualizerService.ShowWindow("AnalysisPointPropertiesView", vm);
    /// </summary>
    public class AnalysisPointPropertiesViewModel : ViewModelBase
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
            Properties.Add(new EditableKeyValuePair<string, string>("Location:", AnalysisPoint.Geo.ToString()));
            Properties.Add(new EditableKeyValuePair<string, string>("Number of Platforms:", AnalysisPoint.Scenario.Platforms.Count.ToString(CultureInfo.InvariantCulture)));
            Properties.Add(new EditableKeyValuePair<string, string>("Number of Transmission Losses:", AnalysisPoint.TransmissionLosses.Count.ToString(CultureInfo.InvariantCulture)));
            Properties.Add(new EditableKeyValuePair<string, string>("Number of Computed Radials:", AnalysisPoint.TransmissionLosses.SelectMany(r => r.Radials).Count().ToString(CultureInfo.InvariantCulture)));
            Properties.Add(new EditableKeyValuePair<string, string>("Size on Disk:", AnalysisPointSize));
        }

        string AnalysisPointSize
        {
            get
            {
                //var size = AnalysisPoint.TransmissionLosses.Sum(transmissionLoss => transmissionLoss.Radials.Select(radial => Directory.GetFiles(Path.GetDirectoryName(radial.BasePath),Path.GetFileName(radial.BasePath + ".*"))).Select(files => files.Select(file => new FileInfo(file)).Select(info => info.Length).Sum()).Sum());
                long size = 0;
                foreach (var tl in AnalysisPoint.TransmissionLosses)
                {
                    foreach (var radial in tl.Radials)
                    {
                        var files = Directory.GetFiles(Path.GetDirectoryName(radial.BasePath), Path.GetFileName(radial.BasePath + ".*"));
                        foreach (var file in files)
                        {
                            var fi = new FileInfo(file);
                            size += fi.Length;
                        }
                    }
                }
                
                switch((int)Math.Log10(size))
                {
                    case 0:
                    case 1:
                    case 2:
                        return string.Format("{0}b", size);
                    case 3:
                    case 4:
                    case 5:
                        return string.Format("{0}K", size>>10);
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

        [Initialize]
        public List<EditableKeyValuePair<string, string>> Properties { get; set; }

        public string WindowTitle { get; set; }

        #region commands
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

    public interface IIsEditable
    {
        bool IsEditable { get; set; }
    }

    public class EditableKeyValuePair<TKey, TValue> : IIsEditable
    {
        public TKey Key { get; set; }
        public TValue Value { get; set; }
        public bool IsEditable { get; set; }

        public EditableKeyValuePair()
        {

        }
        public EditableKeyValuePair(TKey key, TValue value, bool isEditable = false)
        {
            Key = key;
            Value = value;
            IsEditable = isEditable;
        }
    }

    public class EditableKeyValuePairTemplateSelector : DataTemplateSelector
    {
        public DataTemplate TrueTemplate { get; set; }
        public DataTemplate FalseTemplate { get; set; }

        public override DataTemplate SelectTemplate(object item, DependencyObject container)
        {
            var isEditable = item as IIsEditable;
            if (isEditable == null) return null;
            return isEditable.IsEditable ? TrueTemplate : FalseTemplate;
        }
    }
}
