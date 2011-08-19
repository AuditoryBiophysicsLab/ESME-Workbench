using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Windows.Input;
using Cinch;
using ESME.Environment.NAVO;

namespace ESME.Views.EnvironmentBuilder
{
    public class EnvironmentExtractionViewModel : ViewModelBase
    {
        readonly string _selectedOverlay;
        readonly string _selectedBathymetry;
        public EnvironmentExtractionViewModel(string selectedOverlay, string selectedBathymetry)
        {
            _selectedOverlay = selectedOverlay;
            _selectedBathymetry = selectedBathymetry;
            if (HFEVAEnabled) GenerateHFEVA = true;
            foreach (var item in MonthCheckboxes) item.PropertyChanged += (o, args) => UpdateNote();
            foreach (var item in SeasonCheckboxes) item.PropertyChanged += (o, args) => UpdateNote();
            UpdateNote();
        }

        #region public bool GenerateHFEVA { get; set; }

        public bool GenerateHFEVA
        {
            get { return _generateHFEVA; }
            set
            {
                if (_generateHFEVA == value) return;
                _generateHFEVA = value;
                NotifyPropertyChanged(GenerateHFEVAChangedEventArgs);
                UpdateNote();
            }
        }

        static readonly PropertyChangedEventArgs GenerateHFEVAChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentExtractionViewModel>(x => x.GenerateHFEVA);
        bool _generateHFEVA;

        #endregion

        #region public bool HFEVAEnabled { get; set; }

        public bool HFEVAEnabled
        {
            get { return !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.BSTDirectory); }
        }

        #endregion

        #region public bool GenerateHFBL { get; set; }

        public bool GenerateHFBL
        {
            get { return _generateHFBL; }
            set
            {
                if (_generateHFBL == value) return;
                _generateHFBL = value;
                NotifyPropertyChanged(GenerateHFBLChangedEventArgs);
                UpdateNote();
            }
        }

        static readonly PropertyChangedEventArgs GenerateHFBLChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentExtractionViewModel>(x => x.GenerateHFBL);
        bool _generateHFBL;

        #endregion

        #region public bool HFBLEnabled { get; set; }

        public bool HFBLEnabled
        {
            get { return !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.HFBLEXEPath); }
        }

        #endregion

        #region public bool GenerateLFBLHFB { get; set; }

        public bool GenerateLFBLHFB
        {
            get { return _generateLFBLHFB; }
            set
            {
                if (_generateLFBLHFB == value) return;
                _generateLFBLHFB = value;
                NotifyPropertyChanged(GenerateLFBLHFBChangedEventArgs);
                UpdateNote();
            }
        }

        static readonly PropertyChangedEventArgs GenerateLFBLHFBChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentExtractionViewModel>(x => x.GenerateLFBLHFB);
        bool _generateLFBLHFB;

        #endregion

        #region public bool LFBLHFBEnabled { get; set; }

        public bool LFBLHFBEnabled
        {
            get { return !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.LFBLEXEPath) || !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.HFBLEXEPath); }
        }

        #endregion

        #region public bool GenerateLFBLPE { get; set; }

        public bool GenerateLFBLPE
        {
            get { return _generateLFBLPE; }
            set
            {
                if (_generateLFBLPE == value) return;
                _generateLFBLPE = value;
                NotifyPropertyChanged(GenerateLFBLPEChangedEventArgs);
                UpdateNote();
            }
        }

        static readonly PropertyChangedEventArgs GenerateLFBLPEChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentExtractionViewModel>(x => x.GenerateLFBLPE);
        bool _generateLFBLPE;

        #endregion

        #region public bool LFBLPEEnabled { get; set; }

        public bool LFBLPEEnabled
        {
            get { return !string.IsNullOrEmpty(Globals.AppSettings.NAVOConfiguration.LFBLEXEPath); }
        }

        #endregion

        #region SelectAllMonthsCommand

        SimpleCommand<object, object> _selectAllMonths;

        public SimpleCommand<object, object> SelectAllMonthsCommand
        {
            get { return _selectAllMonths ?? (_selectAllMonths = new SimpleCommand<object, object>(delegate { foreach (var month in MonthCheckboxes) month.IsChecked = true; })); }
        }

        #endregion

        #region UnselectAllMonthsCommand

        SimpleCommand<object, object> _unselectAllMonths;

        public SimpleCommand<object, object> UnselectAllMonthsCommand
        {
            get { return _unselectAllMonths ?? (_unselectAllMonths = new SimpleCommand<object, object>(delegate { foreach (var month in MonthCheckboxes) month.IsChecked = false; })); }
        }

        #endregion

        #region SelectAllSeasonsCommand

        SimpleCommand<object, object> _selectAllSeasons;

        public SimpleCommand<object, object> SelectAllSeasonsCommand
        {
            get { return _selectAllSeasons ?? (_selectAllSeasons = new SimpleCommand<object, object>(delegate { foreach (var month in SeasonCheckboxes) month.IsChecked = true; })); }
        }

        #endregion

        #region UnselectAllSeasonsCommand

        SimpleCommand<object, object> _unselectAllSeasons;

        public SimpleCommand<object, object> UnselectAllSeasonsCommand
        {
            get { return _unselectAllSeasons ?? (_unselectAllSeasons = new SimpleCommand<object, object>(delegate { foreach (var month in SeasonCheckboxes) month.IsChecked = false; })); }
        }

        #endregion

        #region public CheckboxSettings MonthCheckboxes { get; set; }

        public CheckboxSettings MonthCheckboxes
        {
            get { return _monthCheckboxes; }
            set
            {
                if (_monthCheckboxes == value) return;
                if (_monthCheckboxes != null) _monthCheckboxes.CollectionChanged -= MonthCheckboxesCollectionChanged;
                _monthCheckboxes = value;
                if (_monthCheckboxes != null) _monthCheckboxes.CollectionChanged += MonthCheckboxesCollectionChanged;
                NotifyPropertyChanged(MonthCheckboxesChangedEventArgs);
            }
        }

        void MonthCheckboxesCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(MonthCheckboxesChangedEventArgs); }
        static readonly PropertyChangedEventArgs MonthCheckboxesChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentExtractionViewModel>(x => x.MonthCheckboxes);

        static CheckboxSettings _monthCheckboxes = new CheckboxSettings
        {
                new CheckboxSetting
                {
                        TimePeriod = NAVOTimePeriod.January
                },
                new CheckboxSetting
                {
                        TimePeriod = NAVOTimePeriod.February
                },
                new CheckboxSetting
                {
                        TimePeriod = NAVOTimePeriod.March
                },
                new CheckboxSetting
                {
                        TimePeriod = NAVOTimePeriod.April
                },
                new CheckboxSetting
                {
                        TimePeriod = NAVOTimePeriod.May
                },
                new CheckboxSetting
                {
                        TimePeriod = NAVOTimePeriod.June
                },
                new CheckboxSetting
                {
                        TimePeriod = NAVOTimePeriod.July
                },
                new CheckboxSetting
                {
                        TimePeriod = NAVOTimePeriod.August
                },
                new CheckboxSetting
                {
                        TimePeriod = NAVOTimePeriod.September
                },
                new CheckboxSetting
                {
                        TimePeriod = NAVOTimePeriod.October
                },
                new CheckboxSetting
                {
                        TimePeriod = NAVOTimePeriod.November
                },
                new CheckboxSetting
                {
                        TimePeriod = NAVOTimePeriod.December
                },
        };

        #endregion

        #region public CheckboxSettings SeasonCheckboxes { get; set; }

        public CheckboxSettings SeasonCheckboxes
        {
            get { return _seasonCheckboxes; }
            set
            {
                if (_seasonCheckboxes == value) return;
                if (_seasonCheckboxes != null) _seasonCheckboxes.CollectionChanged -= SeasonCheckboxesCollectionChanged;
                _seasonCheckboxes = value;
                if (_seasonCheckboxes != null) _seasonCheckboxes.CollectionChanged += SeasonCheckboxesCollectionChanged;
                NotifyPropertyChanged(SeasonCheckboxesChangedEventArgs);
            }
        }

        void SeasonCheckboxesCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(SeasonCheckboxesChangedEventArgs); }
        static readonly PropertyChangedEventArgs SeasonCheckboxesChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentExtractionViewModel>(x => x.SeasonCheckboxes);
        static CheckboxSettings _seasonCheckboxes = new CheckboxSettings
        {
                new CheckboxSetting
                {
                        TimePeriod = NAVOTimePeriod.Spring
                },
                new CheckboxSetting
                {
                        TimePeriod = NAVOTimePeriod.Summer
                },
                new CheckboxSetting
                {
                        TimePeriod = NAVOTimePeriod.Fall
                },
                new CheckboxSetting
                {
                        TimePeriod = NAVOTimePeriod.Winter
                },
                new CheckboxSetting
                {
                        TimePeriod = NAVOTimePeriod.Warm
                },
                new CheckboxSetting
                {
                        TimePeriod = NAVOTimePeriod.Cold
                },
        };

        #endregion

        #region public string Note { get; set; }

        public string Note
        {
            get { return _note; }
            set
            {
                if (_note == value) return;
                _note = value;
                NotifyPropertyChanged(NoteChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NoteChangedEventArgs = ObservableHelper.CreateArgs<BathymetryExtractionViewModel>(x => x.Note);
        string _note;

        void UpdateNote()
        {
            CommandManager.InvalidateRequerySuggested();
            if (!(GenerateHFEVA || GenerateHFBL || GenerateLFBLHFB || GenerateLFBLPE))
            {
                if ((MonthCheckboxes.SelectedTimePeriods.Count() == 0) && (SeasonCheckboxes.SelectedTimePeriods.Count() == 0))
                    Note = "You must select at least one bottom type database\nYou must select at least one time period";
                else Note = "You must select at least one bottom type database";
                return;
            }
            if ((MonthCheckboxes.SelectedTimePeriods.Count() == 0) && (SeasonCheckboxes.SelectedTimePeriods.Count() == 0))
            {
                Note = "You must select at least one time period";
                return;
            }
            var selectedDatabases = new List<string>();
            if (GenerateHFEVA) selectedDatabases.Add("");
            if (GenerateHFBL) selectedDatabases.Add("-hfbl");
            if (GenerateLFBLHFB) selectedDatabases.Add("-lfbl-hfb");
            if (GenerateLFBLPE) selectedDatabases.Add("-lfbl-pe");

            EnvironmentDescriptors.Clear();
            var sb = new StringBuilder();
            foreach (var database in selectedDatabases)
            {
                foreach (var timePeriod in MonthCheckboxes.SelectedTimePeriods)
                    EnvironmentDescriptors.Add(new EnvironmentDescriptor
                    {
                        EnvironmentName = string.Format("{0}_env_{1}{2}.dat", _selectedBathymetry, timePeriod, database),
                        TimePeriod = timePeriod,
                    });
                foreach (var timePeriod in SeasonCheckboxes.SelectedTimePeriods)
                    EnvironmentDescriptors.Add(new EnvironmentDescriptor
                    {
                        EnvironmentName = string.Format("{0}_env_{1}{2}.dat", _selectedBathymetry, timePeriod, database),
                        TimePeriod = timePeriod,
                    });
            }
            foreach (var environmentDescriptor in EnvironmentDescriptors)
                sb.Append(string.Format("{0}\n", environmentDescriptor.EnvironmentName));
            sb.Remove(sb.Length - 2, 2);
            Note = string.Format("Environment file count: {0}\nBoundary overlay: {1}.\nFiles to be created:\n{2}", EnvironmentDescriptors.Count, _selectedOverlay, sb);
        }

        #endregion

        #region List<EnvironmentDescriptors> EnvironmentDescriptor { get; set; }

        public List<EnvironmentDescriptor> EnvironmentDescriptors
        {
            get { return _environmentDescriptor; }
        }

        readonly List<EnvironmentDescriptor> _environmentDescriptor = new List<EnvironmentDescriptor>();

        #endregion

        #region OkCommand
        public SimpleCommand<object, object> OkCommand
        {
            get { return _ok ?? (_ok = new SimpleCommand<object, object>(
                delegate { return (GenerateHFEVA || GenerateHFBL || GenerateLFBLHFB || GenerateLFBLPE) && (MonthCheckboxes.IsAtLeastOneChecked || SeasonCheckboxes.IsAtLeastOneChecked); }, 
                delegate { OkHandler(); })); }
        }

        SimpleCommand<object, object> _ok;

        void OkHandler()
        {
            CloseActivePopUpCommand.Execute(true);
        }
        #endregion
    }

    public class EnvironmentDescriptor
    {
        public string EnvironmentName { get; set; }
        public string SedimentDatabaseName { get; set; }
        public NAVOTimePeriod TimePeriod { get; set; }
    }

    public class CheckboxSettings : ObservableCollection<CheckboxSetting>
    {
        public CheckboxSetting this[string caption]
        {
            get
            {
                foreach (var setting in this.Where(setting => setting.Caption == caption))
                    return setting;
                throw new IndexOutOfRangeException("CheckboxSettings: Specified setting \"" + caption + "\" not found");
            }
        }

        public bool IsAtLeastOneChecked { get { return this.Aggregate(false, (current, setting) => current | setting.IsChecked); } }

        public IEnumerable<NAVOTimePeriod> SelectedTimePeriods
        {
            get { return this.Where(setting => setting.IsChecked).Select(setting => setting.TimePeriod); }
        }
    }

    public class CheckboxSetting : ViewModelBase
    {
        public CheckboxSetting() { IsChecked = false; }

        #region public string Caption { get; set; }

        public string Caption
        {
            get { return TimePeriod.ToString(); }
        }

        #endregion

        public NAVOTimePeriod TimePeriod { get; set; }

        #region public bool IsChecked { get; set; }

        public bool IsChecked
        {
            get { return _isChecked; }
            set
            {
                if (_isChecked == value) return;
                _isChecked = value;
                NotifyPropertyChanged(IsCheckedChangedEventArgs);
                CommandManager.InvalidateRequerySuggested();
            }
        }

        static readonly PropertyChangedEventArgs IsCheckedChangedEventArgs = ObservableHelper.CreateArgs<CheckboxSetting>(x => x.IsChecked);
        bool _isChecked;

        #endregion
    }

}
