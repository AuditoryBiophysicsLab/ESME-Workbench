﻿using System;
using System.ComponentModel;
using System.Windows;
using System.Xml.Serialization;
using Cinch;

namespace ESMEWorkBench.Data
{
    [Serializable]
    public class Experiment : SerializableData<Experiment>
    {
        #region public string Comments { get; set; }

        [XmlElement]
        public string Comments
        {
            get { return _comments; }
            set
            {
                if (_comments == value) return;
                _comments = value;
                NotifyPropertyChanged(NameChangedEventArgs);
            }
        }

        [XmlIgnore] static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.Comments);
        [XmlIgnore] string _comments;

        #endregion

        #region public string Author { get; set; }

        [XmlElement]
        public string Author
        {
            get { return _author; }
            set
            {
                if (_author == value) return;
                _author = value;
                NotifyPropertyChanged(AuthorChangedEventArgs);
            }
        }

        [XmlIgnore] static readonly PropertyChangedEventArgs AuthorChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.Author);
        [XmlIgnore] string _author;

        #endregion

        #region public DateTime Created { get; set; }

        [XmlElement]
        public DateTime Created
        {
            get { return _created; }
            set
            {
                if (_created == value) return;
                _created = value;
                NotifyPropertyChanged(CreatedChangedEventArgs);
            }
        }

        [XmlIgnore] static readonly PropertyChangedEventArgs CreatedChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.Created);
        [XmlIgnore] DateTime _created;

        #endregion

        #region public DateTime LastModified { get; set; }

        [XmlElement]
        public DateTime LastModified
        {
            get { return _lastModified; }
            set
            {
                if (_lastModified == value) return;
                _lastModified = value;
                NotifyPropertyChanged(LastModifiedChangedEventArgs);
            }
        }

        [XmlIgnore] static readonly PropertyChangedEventArgs LastModifiedChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.LastModified);
        [XmlIgnore] DateTime _lastModified;

        #endregion

        #region public string ModifiedBy { get; set; }

        [XmlElement]
        public string ModifiedBy
        {
            get { return _modifiedBy; }
            set
            {
                if (_modifiedBy == value) return;
                _modifiedBy = value;
                NotifyPropertyChanged(ModifiedByChangedEventArgs);
            }
        }

        [XmlIgnore] static readonly PropertyChangedEventArgs ModifiedByChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.ModifiedBy);
        [XmlIgnore] string _modifiedBy;

        #endregion

        #region public string WindSpeedFileName { get; set; }

        [XmlElement]
        public string WindSpeedFileName
        {
            get { return _windSpeedFileName; }
            set
            {
                if (_windSpeedFileName == value) return;
                _windSpeedFileName = value;
                NotifyPropertyChanged(WindSpeedFileNameChangedEventArgs);
            }
        }

        [XmlIgnore] static readonly PropertyChangedEventArgs WindSpeedFileNameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.WindSpeedFileName);
        [XmlIgnore] string _windSpeedFileName;

        #endregion

        #region public string SoundSpeedFileName { get; set; }

        [XmlElement]
        public string SoundSpeedFileName
        {
            get { return _soundSpeedFileName; }
            set
            {
                if (_soundSpeedFileName == value) return;
                _soundSpeedFileName = value;
                NotifyPropertyChanged(SoundSpeedFileNameChangedEventArgs);
            }
        }

        [XmlIgnore] static readonly PropertyChangedEventArgs SoundSpeedFileNameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.SoundSpeedFileName);
        [XmlIgnore] string _soundSpeedFileName;

        #endregion

        #region public string BottomTypeFileName { get; set; }

        [XmlElement]
        public string BottomTypeFileName
        {
            get { return _bottomTypeFileName; }
            set
            {
                if (_bottomTypeFileName == value) return;
                _bottomTypeFileName = value;
                NotifyPropertyChanged(BottomTypeFileNameChangedEventArgs);
            }
        }

        [XmlIgnore] static readonly PropertyChangedEventArgs BottomTypeFileNameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.BottomTypeFileName);
        [XmlIgnore] string _bottomTypeFileName;

        #endregion

        #region public string BathymetryFileName { get; set; }

        [XmlElement]
        public string BathymetryFileName
        {
            get { return _bathymetryFileName; }
            set
            {
                if (_bathymetryFileName == value) return;
                _bathymetryFileName = value;
                NotifyPropertyChanged(BathymetryFileNameChangedEventArgs);
            }
        }

        [XmlIgnore] static readonly PropertyChangedEventArgs BathymetryFileNameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.BathymetryFileName);
        [XmlIgnore] string _bathymetryFileName;

        #endregion

        #region public string ScenarioFileName { get; set; }

        [XmlElement]
        public string ScenarioFileName
        {
            get { return _scenarioFileName; }
            set
            {
                if (_scenarioFileName == value) return;
                _scenarioFileName = value;
                NotifyPropertyChanged(ScenarioFileNameChangedEventArgs);
            }
        }

        [XmlIgnore] static readonly PropertyChangedEventArgs ScenarioFileNameChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.ScenarioFileName);
        [XmlIgnore] string _scenarioFileName;

        #endregion

        #region public bool IsChanged { get; set; }

        [XmlIgnore]
        public bool IsChanged
        {
            get { return _isChanged; }
            set
            {
                if (_isChanged == value) return;
                _isChanged = value;
                NotifyPropertyChanged(IsChangedChangedEventArgs);
            }
        }

        [XmlIgnore] static readonly PropertyChangedEventArgs IsChangedChangedEventArgs = ObservableHelper.CreateArgs<Experiment>(x => x.IsChanged);
        [XmlIgnore] bool _isChanged;

        #endregion
        
        public Experiment()
        {
            Author = Environment.UserName;
            Created = DateTime.Now;
            PropertyChanged += delegate(object s, PropertyChangedEventArgs e) { if (e.PropertyName != "IsChanged") IsChanged = true; };
        }

        public Experiment(string fileName)
            : this()
        {
            FileName = fileName;
        }

        public Experiment(Experiment that) { CopyFrom(that); }

        public new void Save()
        {
            SaveAs(FileName);
        }

        public new void Save(string fileName)
        {
            FileName = fileName;
            SaveAs(FileName);
        }

        public new void SaveAs(string fileName)
        {
            LastModified = DateTime.Now;
            ModifiedBy = Environment.UserName;
            base.SaveAs(fileName);
        }

        public static void Test()
        {
            var test = new Experiment
                       {
                           WindSpeedFileName = "wind",
                           SoundSpeedFileName = "sound",
                           BottomTypeFileName = "bottom",
                           BathymetryFileName = "bathymetry",
                           ScenarioFileName = "scenario"
                       };
            test.Save("test.esme");
        }
    }
}