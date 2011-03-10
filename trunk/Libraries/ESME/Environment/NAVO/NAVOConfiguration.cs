﻿using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Text;
using Cinch;
using HRC.Utility;

namespace ESME.Environment.NAVO
{
    [Serializable]
    public class NAVOConfiguration : SerializableData<NAVOConfiguration>
    {
        #region public NAVOTimePeriod SpringStartMonth { get; set; }

        public NAVOTimePeriod SpringStartMonth
        {
            get { return _springStartMonth; }
            set
            {
                if (_springStartMonth == value) return;
                _springStartMonth = value;
                NotifyPropertyChanged(SpringStartMonthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SpringStartMonthChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.SpringStartMonth);
        NAVOTimePeriod _springStartMonth = NAVOTimePeriod.March;

        #endregion

        #region public NAVOTimePeriod SummerStartMonth { get; set; }

        public NAVOTimePeriod SummerStartMonth
        {
            get { return _summerStartMonth; }
            set
            {
                if (_summerStartMonth == value) return;
                _summerStartMonth = value;
                NotifyPropertyChanged(SummerStartMonthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SummerStartMonthChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.SummerStartMonth);
        NAVOTimePeriod _summerStartMonth = NAVOTimePeriod.June;

        #endregion

        #region public NAVOTimePeriod FallStartMonth { get; set; }

        public NAVOTimePeriod FallStartMonth
        {
            get { return _fallStartMonth; }
            set
            {
                if (_fallStartMonth == value) return;
                _fallStartMonth = value;
                NotifyPropertyChanged(FallStartMonthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs FallStartMonthChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.FallStartMonth);
        NAVOTimePeriod _fallStartMonth = NAVOTimePeriod.September;

        #endregion

        #region public NAVOTimePeriod WinterStartMonth { get; set; }

        public NAVOTimePeriod WinterStartMonth
        {
            get { return _winterStartMonth; }
            set
            {
                if (_winterStartMonth == value) return;
                _winterStartMonth = value;
                NotifyPropertyChanged(WinterStartMonthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs WinterStartMonthChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.WinterStartMonth);
        NAVOTimePeriod _winterStartMonth = NAVOTimePeriod.December;

        #endregion

        #region public NAVOTimePeriod ColdSeasonStartMonth { get; set; }

        public NAVOTimePeriod ColdSeasonStartMonth
        {
            get { return _coldSeasonStartMonth; }
            set
            {
                if (_coldSeasonStartMonth == value) return;
                _coldSeasonStartMonth = value;
                NotifyPropertyChanged(ColdSeasonStartMonthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ColdSeasonStartMonthChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.ColdSeasonStartMonth);
        NAVOTimePeriod _coldSeasonStartMonth = NAVOTimePeriod.December;

        #endregion

        #region public NAVOTimePeriod WarmSeasonStartMonth { get; set; }

        public NAVOTimePeriod WarmSeasonStartMonth
        {
            get { return _warmSeasonStartMonth; }
            set
            {
                if (_warmSeasonStartMonth == value) return;
                _warmSeasonStartMonth = value;
                NotifyPropertyChanged(WarmSeasonStartMonthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs WarmSeasonStartMonthChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.WarmSeasonStartMonth);
        NAVOTimePeriod _warmSeasonStartMonth = NAVOTimePeriod.June;

        #endregion


        #region public string GDEMDirectory { get; set; }

        public string GDEMDirectory
        {
            get { return _gDEMDirectory; }
            set
            {
                if (_gDEMDirectory == value) return;
                _gDEMDirectory = value;
                NotifyPropertyChanged(GDEMDirectoryChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs GDEMDirectoryChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.GDEMDirectory);
        string _gDEMDirectory;

        #endregion

        #region public string SMGCDirectory { get; set; }

        public string SMGCDirectory
        {
            get { return _sMGCDirectory; }
            set
            {
                if (_sMGCDirectory == value) return;
                _sMGCDirectory = value;
                NotifyPropertyChanged(SMGCDirectoryChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SMGCDirectoryChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.SMGCDirectory);
        string _sMGCDirectory;

        #endregion

        #region public string DBDBDirectory { get; set; }

        public string DBDBDirectory
        {
            get { return _dBDBDirectory; }
            set
            {
                if (_dBDBDirectory == value) return;
                _dBDBDirectory = value;
                NotifyPropertyChanged(DBDBDirectoryChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DBDBDirectoryChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.DBDBDirectory);
        string _dBDBDirectory;

        #endregion

        #region public string BSTDirectory { get; set; }

        public string BSTDirectory
        {
            get { return _bSTDirectory; }
            set
            {
                if (_bSTDirectory == value) return;
                _bSTDirectory = value;
                NotifyPropertyChanged(BSTDirectoryChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BSTDirectoryChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.BSTDirectory);
        string _bSTDirectory;

        #endregion

        #region public string BSTEXEPath { get; set; }

        public string BSTEXEPath
        {
            get { return _bSTEXEPath; }
            set
            {
                if (_bSTEXEPath == value) return;
                _bSTEXEPath = value;
                NotifyPropertyChanged(BSTEXEPathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BSTEXEPathChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.BSTEXEPath);
        string _bSTEXEPath;

        #endregion

        #region public string DBDBEXEPath { get; set; }

        public string DBDBEXEPath
        {
            get { return _dBDBEXEPath; }
            set
            {
                if (_dBDBEXEPath == value) return;
                _dBDBEXEPath = value;
                NotifyPropertyChanged(DBDBEXEPathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DBDBEXEPathChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.DBDBEXEPath);
        string _dBDBEXEPath;

        #endregion

        #region public string GDEMEXEPath { get; set; }

        public string GDEMEXEPath
        {
            get { return _gDEMEXEPath; }
            set
            {
                if (_gDEMEXEPath == value) return;
                _gDEMEXEPath = value;
                NotifyPropertyChanged(GDEMEXEPathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs GDEMEXEPathChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.GDEMEXEPath);
        string _gDEMEXEPath;

        #endregion

        #region public string BSTSelectedResolution { get; set; }

        public string BSTSelectedResolution
        {
            get { return _bSTSelectedResolution; }
            set
            {
                if (_bSTSelectedResolution == value) return;
                _bSTSelectedResolution = value;
                NotifyPropertyChanged(BSTSelectedResolutionChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BSTSelectedResolutionChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.BSTSelectedResolution);
        string _bSTSelectedResolution;

        #endregion

        #region public string DBDBSelectedResolution { get; set; }

        public string DBDBSelectedResolution
        {
            get { return _dBDBSelectedResolution; }
            set
            {
                if (_dBDBSelectedResolution == value) return;
                _dBDBSelectedResolution = value;
                NotifyPropertyChanged(DBDBSelectedResolutionChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DBDBSelectedResolutionChangedEventArgs = ObservableHelper.CreateArgs<NAVOConfiguration>(x => x.DBDBSelectedResolution);
        string _dBDBSelectedResolution;

        #endregion

        public bool IsValid
        {
            get
            {
                if (!string.IsNullOrEmpty(BSTDirectory) && !string.IsNullOrEmpty(DBDBDirectory) && !string.IsNullOrEmpty(GDEMDirectory) && !string.IsNullOrEmpty(SMGCDirectory))
                    if (File.Exists(BSTDirectory) && File.Exists(DBDBDirectory) && Directory.Exists(GDEMDirectory) && Directory.Exists(SMGCDirectory))
                        if (!string.IsNullOrEmpty(BSTEXEPath) && !string.IsNullOrEmpty(DBDBEXEPath))
                            if (File.Exists(BSTEXEPath) && File.Exists(DBDBEXEPath)) return true;
                return false;
            }
        }


    }
}