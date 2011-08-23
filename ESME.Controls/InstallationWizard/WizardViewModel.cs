using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using Cinch;
using HRC.Validation;

namespace ESME.Views.InstallationWizard
{
    class WizardViewModel:ValidatingViewModel 
    {
        public WizardViewModel( ) {
            
        }

        #region public string FileName { get; set; }

        public string FileName
        {
            get { return _fileName; }
            set
            {
                if (_fileName == value) return;
                _fileName = value;
                NotifyPropertyChanged(FileNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs FileNameChangedEventArgs = ObservableHelper.CreateArgs<WizardViewModel>(x => x.FileName);
        string _fileName;

        #endregion


        #region public List<WizardPanelInfo> Panels { get; set; }

        public List<WizardPanelInfo> Panels
        {
            get { return _panels; }
            set
            {
                if (_panels == value) return;
                _panels = value;
                NotifyPropertyChanged(PanelsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs PanelsChangedEventArgs = ObservableHelper.CreateArgs<WizardViewModel>(x => x.Panels);
        List<WizardPanelInfo> _panels;

        #endregion

    }

    public class WizardPanelInfo:ValidatingViewModel
    {
        #region public string DescriptiveText { get; set; }

        public string DescriptiveText
        {
            get { return _descriptiveText; }
            set
            {
                if (_descriptiveText == value) return;
                _descriptiveText = value;
                NotifyPropertyChanged(DescriptiveTextChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DescriptiveTextChangedEventArgs = ObservableHelper.CreateArgs<WizardPanelInfo>(x => x.DescriptiveText);
        string _descriptiveText;

        #endregion

        #region public string UserResponse { get; set; }

        public string UserResponse
        {
            get { return _userResponse; }
            set
            {
                if (_userResponse == value) return;
                _userResponse = value;
                NotifyPropertyChanged(UserResponseChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs UserResponseChangedEventArgs = ObservableHelper.CreateArgs<WizardPanelInfo>(x => x.UserResponse);
        string _userResponse;

        #endregion

        #region public string FieldName { get; set; }

        public string FieldName
        {
            get { return _fieldName; }
            set
            {
                if (_fieldName == value) return;
                _fieldName = value;
                NotifyPropertyChanged(FieldNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs FieldNameChangedEventArgs = ObservableHelper.CreateArgs<WizardPanelInfo>(x => x.FieldName);
        string _fieldName;

        #endregion
        
    }
    
}
