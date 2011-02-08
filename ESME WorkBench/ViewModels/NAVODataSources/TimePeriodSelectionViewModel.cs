using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using System.Text;
using Cinch;
using ESME.Environment.NAVO;
using ESMEWorkBench.Data;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.NAVODataSources
{
     [ExportViewModel("TimePeriodSelectionViewModel")]
    class TimePeriodSelectionViewModel: ViewModelBase
    {
         #region LaunchSeasonConfigurationViewCommand

         public SimpleCommand<object, object> LaunchSeasonConfigurationViewCommand
         {
             get { return _launchSeasonConfigurationView ?? (_launchSeasonConfigurationView = new SimpleCommand<object, object>(delegate
                                                                                                                                {
                                                                                                                                   

                                                                                                                                })); }
         }

         SimpleCommand<object, object> _launchSeasonConfigurationView;

         #endregion

         #region SelectAllMonthsCommand

         public SimpleCommand<object, object> SelectAllMonthsCommand
         {
             get { return _selectAllMonths ?? (_selectAllMonths = new SimpleCommand<object, object>(delegate { January = February = March = April = May = June = July = August = September = October = November = December = true; })); }
         }

         SimpleCommand<object, object> _selectAllMonths;

         #endregion

         #region UnselectAllMonthsCommand

         public SimpleCommand<object, object> UnselectAllMonthsCommand
         {
             get { return _unselectAllMonths ?? (_unselectAllMonths = new SimpleCommand<object, object>(delegate { January = February = March = April = May = June = July = August = September = October = November = December = false; })); }
         }

         SimpleCommand<object, object> _unselectAllMonths;

         #endregion

         #region SelectAllSeasonsCommand

         public SimpleCommand<object, object> SelectAllSeasonsCommand
         {
             get { return _selectAllSeasons ?? (_selectAllSeasons = new SimpleCommand<object, object>(delegate { Winter = Spring = Summer = Fall = Warm = Cold = true; })); }
         }

         SimpleCommand<object, object> _selectAllSeasons;

         #endregion

         #region UnselectAllSeasonsCommand

         public SimpleCommand<object, object> UnselectAllSeasonsCommand
         {
             get { return _unselectAllSeasons ?? (_unselectAllSeasons = new SimpleCommand<object, object>(delegate { Winter = Spring = Summer = Fall = Warm = Cold = false; })); }
         }

         SimpleCommand<object, object> _unselectAllSeasons;

         #endregion

         #region OkCommand

         public SimpleCommand<object, object> OkCommand
         {
             get
             {
                 return _ok ?? (_ok = new SimpleCommand<object, object>(delegate
                 {
                   if(January) SelectedPeriods.Add(NAVOTimePeriod.January);
                   if (February) SelectedPeriods.Add(NAVOTimePeriod.February);
                   if (March) SelectedPeriods.Add(NAVOTimePeriod.March);
                   if (April) SelectedPeriods.Add(NAVOTimePeriod.April);
                   if (May) SelectedPeriods.Add(NAVOTimePeriod.May);
                   if (June) SelectedPeriods.Add(NAVOTimePeriod.June);
                   if (July) SelectedPeriods.Add(NAVOTimePeriod.July);
                   if (August) SelectedPeriods.Add(NAVOTimePeriod.August);
                   if (September) SelectedPeriods.Add(NAVOTimePeriod.September);
                   if (October) SelectedPeriods.Add(NAVOTimePeriod.October);
                   if (November) SelectedPeriods.Add(NAVOTimePeriod.November);
                   if (December) SelectedPeriods.Add(NAVOTimePeriod.December);
                   if (Winter) SelectedPeriods.Add(NAVOTimePeriod.Winter);
                   if (Spring) SelectedPeriods.Add(NAVOTimePeriod.Spring);
                   if (Summer) SelectedPeriods.Add(NAVOTimePeriod.Summer);
                   if (Fall) SelectedPeriods.Add(NAVOTimePeriod.Fall);
                   if (Warm) SelectedPeriods.Add(NAVOTimePeriod.Warm);
                   if (Cold) SelectedPeriods.Add(NAVOTimePeriod.Cold);
                                                                                        
             })); }
         }

         SimpleCommand<object, object> _ok;

         #endregion

         #region public ObservableCollection<NAVOTimePeriod> SelectedPeriods { get; set; }

         public ObservableCollection<NAVOTimePeriod> SelectedPeriods
         {
             get { return _selectedPeriods; }
             set
             {
                 if (_selectedPeriods == value) return;
                 if (_selectedPeriods != null) _selectedPeriods.CollectionChanged -= SelectedPeriodsCollectionChanged;
                 _selectedPeriods = value;
                 if (_selectedPeriods != null) _selectedPeriods.CollectionChanged += SelectedPeriodsCollectionChanged;
                 NotifyPropertyChanged(SelectedPeriodsChangedEventArgs);
             }
         }

         void SelectedPeriodsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(SelectedPeriodsChangedEventArgs); }
         static readonly PropertyChangedEventArgs SelectedPeriodsChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.SelectedPeriods);
         ObservableCollection<NAVOTimePeriod> _selectedPeriods;

         #endregion
         
         #region public bool January { get; set; }

         public bool January
         {
             get { return _january; }
             set
             {
                 if (_january == value) return;
                 _january = value;
                 NotifyPropertyChanged(JanuaryChangedEventArgs);
             }
         }

         static readonly PropertyChangedEventArgs JanuaryChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.January);
         bool _january;

         #endregion

         #region public bool February { get; set; }

         public bool February
         {
             get { return _february; }
             set
             {
                 if (_february == value) return;
                 _february = value;
                 NotifyPropertyChanged(FebruaryChangedEventArgs);
             }
         }

         static readonly PropertyChangedEventArgs FebruaryChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.February);
         bool _february;

         #endregion

         #region public bool March { get; set; }

         public bool March
         {
             get { return _march; }
             set
             {
                 if (_march == value) return;
                 _march = value;
                 NotifyPropertyChanged(MarchChangedEventArgs);
             }
         }

         static readonly PropertyChangedEventArgs MarchChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.March);
         bool _march;

         #endregion

         #region public bool April { get; set; }

         public bool April
         {
             get { return _april; }
             set
             {
                 if (_april == value) return;
                 _april = value;
                 NotifyPropertyChanged(AprilChangedEventArgs);
             }
         }

         static readonly PropertyChangedEventArgs AprilChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.April);
         bool _april;

         #endregion

         #region public bool May { get; set; }

         public bool May
         {
             get { return _may; }
             set
             {
                 if (_may == value) return;
                 _may = value;
                 NotifyPropertyChanged(MayChangedEventArgs);
             }
         }

         static readonly PropertyChangedEventArgs MayChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.May);
         bool _may;

         #endregion

         #region public bool June { get; set; }

         public bool June
         {
             get { return _june; }
             set
             {
                 if (_june == value) return;
                 _june = value;
                 NotifyPropertyChanged(JuneChangedEventArgs);
             }
         }

         static readonly PropertyChangedEventArgs JuneChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.June);
         bool _june;

         #endregion

         #region public bool July { get; set; }

         public bool July
         {
             get { return _july; }
             set
             {
                 if (_july == value) return;
                 _july = value;
                 NotifyPropertyChanged(JulyChangedEventArgs);
             }
         }

         static readonly PropertyChangedEventArgs JulyChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.July);
         bool _july;

         #endregion

         #region public bool August { get; set; }

         public bool August
         {
             get { return _august; }
             set
             {
                 if (_august == value) return;
                 _august = value;
                 NotifyPropertyChanged(AugustChangedEventArgs);
             }
         }

         static readonly PropertyChangedEventArgs AugustChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.August);
         bool _august;

         #endregion

         #region public bool September { get; set; }

         public bool September
         {
             get { return _september; }
             set
             {
                 if (_september == value) return;
                 _september = value;
                 NotifyPropertyChanged(SeptemberChangedEventArgs);
             }
         }

         static readonly PropertyChangedEventArgs SeptemberChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.September);
         bool _september;

         #endregion

         #region public bool October { get; set; }

         public bool October
         {
             get { return _october; }
             set
             {
                 if (_october == value) return;
                 _october = value;
                 NotifyPropertyChanged(OctoberChangedEventArgs);
             }
         }

         static readonly PropertyChangedEventArgs OctoberChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.October);
         bool _october;

         #endregion

         #region public bool November { get; set; }

         public bool November
         {
             get { return _november; }
             set
             {
                 if (_november == value) return;
                 _november = value;
                 NotifyPropertyChanged(NovemberChangedEventArgs);
             }
         }

         static readonly PropertyChangedEventArgs NovemberChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.November);
         bool _november;

         #endregion

         #region public bool December { get; set; }

         public bool December
         {
             get { return _december; }
             set
             {
                 if (_december == value) return;
                 _december = value;
                 NotifyPropertyChanged(DecemberChangedEventArgs);
             }
         }

         static readonly PropertyChangedEventArgs DecemberChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.December);
         bool _december;

         #endregion

         #region public bool Spring { get; set; }

         public bool Spring
         {
             get { return _spring; }
             set
             {
                 if (_spring == value) return;
                 _spring = value;
                 NotifyPropertyChanged(SpringChangedEventArgs);
             }
         }

         static readonly PropertyChangedEventArgs SpringChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.Spring);
         bool _spring;

         #endregion

         #region public bool Summer { get; set; }

         public bool Summer
         {
             get { return _summer; }
             set
             {
                 if (_summer == value) return;
                 _summer = value;
                 NotifyPropertyChanged(SummerChangedEventArgs);
             }
         }

         static readonly PropertyChangedEventArgs SummerChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.Summer);
         bool _summer;

         #endregion

         #region public bool Fall { get; set; }

         public bool Fall
         {
             get { return _fall; }
             set
             {
                 if (_fall == value) return;
                 _fall = value;
                 NotifyPropertyChanged(FallChangedEventArgs);
             }
         }

         static readonly PropertyChangedEventArgs FallChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.Fall);
         bool _fall;

         #endregion

         #region public bool Winter { get; set; }

         public bool Winter
         {
             get { return _winter; }
             set
             {
                 if (_winter == value) return;
                 _winter = value;
                 NotifyPropertyChanged(WinterChangedEventArgs);
             }
         }

         static readonly PropertyChangedEventArgs WinterChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.Winter);
         bool _winter;

         #endregion

         #region public bool Warm { get; set; }

         public bool Warm
         {
             get { return _warm; }
             set
             {
                 if (_warm == value) return;
                 _warm = value;
                 NotifyPropertyChanged(WarmChangedEventArgs);
             }
         }

         static readonly PropertyChangedEventArgs WarmChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.Warm);
         bool _warm;

         #endregion

         #region public bool Cold { get; set; }

         public bool Cold
         {
             get { return _cold; }
             set
             {
                 if (_cold == value) return;
                 _cold = value;
                 NotifyPropertyChanged(ColdChangedEventArgs);
             }
         }

         static readonly PropertyChangedEventArgs ColdChangedEventArgs = ObservableHelper.CreateArgs<TimePeriodSelectionViewModel>(x => x.Cold);
         bool _cold;

         #endregion




    }
}
