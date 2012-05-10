using System.Collections.Generic;
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
            Properties.Add(new EditableKeyValuePair<string, string>("Location: ", AnalysisPoint.Geo.ToString()));
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

        public EditableKeyValuePair() {
            
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
