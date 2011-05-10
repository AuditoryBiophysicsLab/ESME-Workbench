using System.Collections.Generic;
using System.ComponentModel;
using Cinch;
using ESME.TransmissionLoss;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    public class ModeAcousticModelSelectionViewModel : ViewModelBase
    {
        readonly NemoModeToAcousticModelNameMap _modeMap;
        public ModeAcousticModelSelectionViewModel(NemoModeToAcousticModelNameMap modeMap, List<TransmissionLossAlgorithm> validModels)
        {
            _modeMap = modeMap;
            ValidModels = validModels;
            ModeToAcousticModelMapList = new List<ModeToAcousticModelMapViewModel>();
            foreach (var curMode in modeMap)
                ModeToAcousticModelMapList.Add(new ModeToAcousticModelMapViewModel(curMode.Key, curMode.Value, validModels));
        }

        #region public List<ModeToAcousticModelMapViewModel> ModeToAcousticModelMapList { get; set; }

        public List<ModeToAcousticModelMapViewModel> ModeToAcousticModelMapList
        {
            get { return _modeToAcousticModelMapList; }
            set
            {
                if (_modeToAcousticModelMapList == value) return;
                _modeToAcousticModelMapList = value;
                NotifyPropertyChanged(ModeToAcousticModelMapListChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ModeToAcousticModelMapListChangedEventArgs = ObservableHelper.CreateArgs<ModeAcousticModelSelectionViewModel>(x => x.ModeToAcousticModelMapList);
        List<ModeToAcousticModelMapViewModel> _modeToAcousticModelMapList;

        #endregion

        #region public List<TransmissionLossAlgorithm> ValidModels { get; set; }

        public List<TransmissionLossAlgorithm> ValidModels
        {
            get { return _validModels; }
            set
            {
                if (_validModels == value) return;
                _validModels = value;
                NotifyPropertyChanged(ValidModelNamesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ValidModelNamesChangedEventArgs = ObservableHelper.CreateArgs<ModeAcousticModelSelectionViewModel>(x => x.ValidModels);
        List<TransmissionLossAlgorithm> _validModels;

        #endregion

        #region OkCommand

        public SimpleCommand<object, object> OkCommand
        {
            get { return _ok ?? (_ok = new SimpleCommand<object, object>(delegate
                                                                         {
                                                                             // Write the modified entries back to the original list
                                                                             foreach (var mapEntry in ModeToAcousticModelMapList) _modeMap[mapEntry.ModeName] = mapEntry.Model;
                                                                             CloseActivePopUpCommand.Execute(true);
                                                                         })); }
        }

        SimpleCommand<object, object> _ok;

        #endregion
    }

    public class ModeToAcousticModelMapViewModel : ViewModelBase
    {
        public ModeToAcousticModelMapViewModel(string modeName, TransmissionLossAlgorithm model, List<TransmissionLossAlgorithm> validModels)
        {
            ModeName = modeName;
            Model = model;
            ValidModels = validModels;
            SelectedIndex = validModels.IndexOf(Model);
        }

        #region public string ModeName { get; set; }

        public string ModeName
        {
            get { return _modeName; }
            set
            {
                if (_modeName == value) return;
                _modeName = value;
                NotifyPropertyChanged(ModeNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ModeNameChangedEventArgs = ObservableHelper.CreateArgs<ModeToAcousticModelMapViewModel>(x => x.ModeName);
        string _modeName;

        #endregion

        #region public TransmissionLossAlgorithm Model { get; set; }

        public TransmissionLossAlgorithm Model
        {
            get { return _model; }
            set
            {
                if (_model == value) return;
                _model = value;
                NotifyPropertyChanged(ModelNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ModelNameChangedEventArgs = ObservableHelper.CreateArgs<ModeToAcousticModelMapViewModel>(x => x.Model);
        TransmissionLossAlgorithm _model;

        #endregion

        #region public int SelectedIndex { get; set; }

        public int SelectedIndex
        {
            get { return ValidModels.IndexOf(Model); }
            set
            {
                Model = ValidModels[value];
                NotifyPropertyChanged(SelectedIndexChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedIndexChangedEventArgs = ObservableHelper.CreateArgs<ModeToAcousticModelMapViewModel>(x => x.SelectedIndex);

        #endregion

        #region public List<TransmissionLossAlgorithm> ValidModels { get; set; }

        public List<TransmissionLossAlgorithm> ValidModels
        {
            get { return _validModels; }
            set
            {
                if (_validModels == value) return;
                _validModels = value;
                NotifyPropertyChanged(ValidModelNamesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ValidModelNamesChangedEventArgs = ObservableHelper.CreateArgs<ModeToAcousticModelMapViewModel>(x => x.ValidModels);
        List<TransmissionLossAlgorithm> _validModels;

        #endregion
    }
}