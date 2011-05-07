using System.Collections.Generic;
using System.ComponentModel;
using Cinch;
using ESME.TransmissionLoss;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    public class ModeAcousticModelSelectionViewModel : ViewModelBase
    {
        readonly NemoModeToAcousticModelNameMap _modeMap;
        public ModeAcousticModelSelectionViewModel(NemoModeToAcousticModelNameMap modeMap, List<string> validModelNames)
        {
            _modeMap = modeMap;
            ValidModelNames = validModelNames;
            ModeToAcousticModelMapList = new List<ModeToAcousticModelMapViewModel>();
            foreach (var curMode in modeMap)
                ModeToAcousticModelMapList.Add(new ModeToAcousticModelMapViewModel(curMode.Key, curMode.Value, validModelNames));
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

        #region public List<string> ValidModelNames { get; set; }

        public List<string> ValidModelNames
        {
            get { return _validModelNames; }
            set
            {
                if (_validModelNames == value) return;
                _validModelNames = value;
                NotifyPropertyChanged(ValidModelNamesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ValidModelNamesChangedEventArgs = ObservableHelper.CreateArgs<ModeAcousticModelSelectionViewModel>(x => x.ValidModelNames);
        List<string> _validModelNames;

        #endregion

        #region OkCommand

        public SimpleCommand<object, object> OkCommand
        {
            get { return _ok ?? (_ok = new SimpleCommand<object, object>(delegate
                                                                         {
                                                                             // Write the modified entries back to the original list
                                                                             foreach (var mapEntry in ModeToAcousticModelMapList) _modeMap[mapEntry.ModeName] = mapEntry.ModelName;
                                                                             CloseActivePopUpCommand.Execute(true);
                                                                         })); }
        }

        SimpleCommand<object, object> _ok;

        #endregion

    }

    public class ModeToAcousticModelMapViewModel : ViewModelBase
    {
        public ModeToAcousticModelMapViewModel(string modeName, string modelName, List<string> validModelNames)
        {
            ModeName = modeName;
            ModelName = modelName;
            ValidModelNames = validModelNames;
            SelectedIndex = validModelNames.IndexOf(ModelName);
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

        #region public string ModelName { get; set; }

        public string ModelName
        {
            get { return _modelName; }
            set
            {
                if (_modelName == value) return;
                _modelName = value;
                NotifyPropertyChanged(ModelNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ModelNameChangedEventArgs = ObservableHelper.CreateArgs<ModeToAcousticModelMapViewModel>(x => x.ModelName);
        string _modelName;

        #endregion

        #region public int SelectedIndex { get; set; }

        public int SelectedIndex
        {
            get { return ValidModelNames.IndexOf(ModelName); }
            set
            {
                ModelName = ValidModelNames[value];
                NotifyPropertyChanged(SelectedIndexChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedIndexChangedEventArgs = ObservableHelper.CreateArgs<ModeToAcousticModelMapViewModel>(x => x.SelectedIndex);

        #endregion

        #region public List<string> ValidModelNames { get; set; }

        public List<string> ValidModelNames
        {
            get { return _validModelNames; }
            set
            {
                if (_validModelNames == value) return;
                _validModelNames = value;
                NotifyPropertyChanged(ValidModelNamesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ValidModelNamesChangedEventArgs = ObservableHelper.CreateArgs<ModeToAcousticModelMapViewModel>(x => x.ValidModelNames);
        List<string> _validModelNames;

        #endregion

    }
}