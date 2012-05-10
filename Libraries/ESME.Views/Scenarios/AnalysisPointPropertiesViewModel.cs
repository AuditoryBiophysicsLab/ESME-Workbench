using System.Collections.Generic;
using System.Windows;
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

        void GenerateProperties()
        {
            AnalysisPointProperties.Add(new EditableKeyValuePair<string, string>
            {
                Key = "hello",
                Value = "world",
                IsEditable = false,
            });
            AnalysisPointProperties.Add(new EditableKeyValuePair<string, string>
            {
                Key = "Zarro",
                Value = "boogs",
                IsEditable = true,
            });
            
        }

        [Initialize]
        public List<EditableKeyValuePair<string, string>> AnalysisPointProperties { get; set; }

        #region commands
        #region OkCommand
        public SimpleCommand<object, EventToCommandArgs> OkCommand { get { return _ok ?? (_ok = new SimpleCommand<object, EventToCommandArgs>(o => CloseDialog(null))); } }
        SimpleCommand<object, EventToCommandArgs> _ok;
        #endregion

        #region ViewClosingCommand
        public SimpleCommand<object, EventToCommandArgs> ViewClosingCommand { get { return _viewClosing ?? (_viewClosing = new SimpleCommand<object, EventToCommandArgs>(o => Properties.Settings.Default.Save())); } }
        SimpleCommand<object, EventToCommandArgs> _viewClosing;
        #endregion
        #endregion
    }

    public class EditableKeyValuePair<TKey, TValue>
    {
        public TKey Key { get; set; }
        public TValue Value { get; set; }
        public bool IsEditable { get; set; }
    }


}
