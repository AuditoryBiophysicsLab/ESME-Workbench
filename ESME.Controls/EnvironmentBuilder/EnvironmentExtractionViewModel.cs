using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using System.Text;
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

            foreach (var item in MonthCheckboxes) item.PropertyChanged += (o, args) => UpdateNote();
            foreach (var item in SeasonCheckboxes) item.PropertyChanged += (o, args) => UpdateNote();
            UpdateNote();
        }

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
            if ((MonthCheckboxes.SelectedTimePeriods.Count() == 0) && (SeasonCheckboxes.SelectedTimePeriods.Count() == 0))
            {
                Note = "Note: You must select at least one time period to extract environment data.";
                return;
            }

            EnvironmentDescriptors.Clear();
            var sb = new StringBuilder();
            foreach (var timePeriod in MonthCheckboxes.SelectedTimePeriods)
                EnvironmentDescriptors.Add(new EnvironmentDescriptor
                {
                        EnvironmentName = string.Format("{0}_{1}", _selectedBathymetry, timePeriod),
                        TimePeriod = timePeriod,
                });
            foreach (var timePeriod in SeasonCheckboxes.SelectedTimePeriods)
                EnvironmentDescriptors.Add(new EnvironmentDescriptor
                {
                        EnvironmentName = string.Format("{0}_{1}", _selectedBathymetry, timePeriod),
                        TimePeriod = timePeriod,
                });
            foreach (var environmentDescriptor in EnvironmentDescriptors)
                sb.Append(string.Format("{0}, ", environmentDescriptor.EnvironmentName));
            sb.Remove(sb.Length - 2, 2);
            Note = string.Format("Note: Environment data will be extracted within the bounds of the overlay {0}.  The following environment files will be created:\n{1}", _selectedOverlay, sb);
        }

        #endregion

        #region public List<EnvironmentDescriptors> EnvironmentDescriptor { get; set; }

        public List<EnvironmentDescriptor> EnvironmentDescriptors
        {
            get { return _environmentDescriptor ?? (_environmentDescriptor = new List<EnvironmentDescriptor>()); }
            set
            {
                if (_environmentDescriptor == value) return;
                _environmentDescriptor = value;
                NotifyPropertyChanged(EnvironmentDescriptorChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs EnvironmentDescriptorChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentExtractionViewModel>(x => x.EnvironmentDescriptors);
        List<EnvironmentDescriptor> _environmentDescriptor;

        #endregion

        #region OkCommand
        public SimpleCommand<object, object> OkCommand
        {
            get { return _ok ?? (_ok = new SimpleCommand<object, object>(delegate { return true; }, delegate { OkHandler(); })); }
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
            }
        }

        static readonly PropertyChangedEventArgs IsCheckedChangedEventArgs = ObservableHelper.CreateArgs<CheckboxSetting>(x => x.IsChecked);
        bool _isChecked;

        #endregion
    }

}
