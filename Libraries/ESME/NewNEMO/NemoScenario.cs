using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Xml.Serialization;
using Cinch;
using HRC.Utility;

namespace ESME.NewNEMO
{
    [Serializable]
    [XmlRoot("Scenario")]
    public class NemoScenario : PropertyChangedBase
    {
        public static void Test(string fileName)
        {
            var loaded = Load(fileName);
            loaded.Save(Path.Combine(Path.GetDirectoryName(fileName), Path.GetFileNameWithoutExtension(fileName) + ".save"));
        }

        public static NemoScenario Load(string fileName)
        {
            var result = XmlSerializer<NemoScenario>.Load(fileName, null);
            return result;
        }

        public virtual void Save(string fileName)
        {
            var serializer = new XmlSerializer<NemoScenario> { Data = this };
            serializer.Save(fileName, null);
        }

        #region public string PsmVersion { get; set; }
        [XmlElement("psmVersion", IsNullable = true)]
        public string PsmVersion
        {
            get { return _psmVersion; }
            set
            {
                if (_psmVersion == value) return;
                _psmVersion = value;
                NotifyPropertyChanged(PsmVersionChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs PsmVersionChangedEventArgs = ObservableHelper.CreateArgs<NemoScenario>(x => x.PsmVersion);
        string _psmVersion;

        #endregion

        #region public string EventName { get; set; }
        [XmlElement("eventName", IsNullable = true)]
        public string EventName
        {
            get { return _eventName; }
            set
            {
                if (_eventName == value) return;
                _eventName = value;
                NotifyPropertyChanged(EventNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs EventNameChangedEventArgs = ObservableHelper.CreateArgs<NemoScenario>(x => x.EventName);
        string _eventName;

        #endregion

        #region public string CreationTime { get; set; }
        [XmlElement("creationTime", IsNullable = true)]
        public string CreationTime
        {
            get { return _creationTime; }
            set
            {
                if (_creationTime == value) return;
                _creationTime = value;
                NotifyPropertyChanged(CreationTimeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CreationTimeChangedEventArgs = ObservableHelper.CreateArgs<NemoScenario>(x => x.CreationTime);
        string _creationTime;

        #endregion

        #region public string Description { get; set; }
        [XmlElement("description", IsNullable = true)]
        public string Description
        {
            get { return _description; }
            set
            {
                if (_description == value) return;
                _description = value;
                NotifyPropertyChanged(DescriptionChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DescriptionChangedEventArgs = ObservableHelper.CreateArgs<NemoScenario>(x => x.Description);
        string _description;

        #endregion

        #region public string AnalystName { get; set; }
        [XmlElement("analystName", IsNullable = true)]
        public string AnalystName
        {
            get { return _analystName; }
            set
            {
                if (_analystName == value) return;
                _analystName = value;
                NotifyPropertyChanged(AnalystNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs AnalystNameChangedEventArgs = ObservableHelper.CreateArgs<NemoScenario>(x => x.AnalystName);
        string _analystName;

        #endregion

        #region public string StartTime { get; set; }
        [XmlElement("startTime", IsNullable = true)]
        public string StartTime
        {
            get { return _startTime; }
            set
            {
                if (_startTime == value) return;
                _startTime = value;
                NotifyPropertyChanged(StartTimeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs StartTimeChangedEventArgs = ObservableHelper.CreateArgs<NemoScenario>(x => x.StartTime);
        string _startTime;

        #endregion

        #region public string Duration { get; set; }
        [XmlElement("duration", IsNullable = true)]
        public string Duration
        {
            get { return _duration; }
            set
            {
                if (_duration == value) return;
                _duration = value;
                NotifyPropertyChanged(DurationChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DurationChangedEventArgs = ObservableHelper.CreateArgs<NemoScenario>(x => x.Duration);
        string _duration;

        #endregion

        #region public string SimAreaName { get; set; }
        [XmlElement("simAreaName", IsNullable = true)]
        public string SimAreaName
        {
            get { return _simAreaName; }
            set
            {
                if (_simAreaName == value) return;
                _simAreaName = value;
                NotifyPropertyChanged(SimAreaNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SimAreaNameChangedEventArgs = ObservableHelper.CreateArgs<NemoScenario>(x => x.SimAreaName);
        string _simAreaName;

        #endregion

        #region public string TimeFrame { get; set; }
        [XmlElement("timeFrame", IsNullable = true)]
        public string TimeFrame
        {
            get { return _timeFrame; }
            set
            {
                if (_timeFrame == value) return;
                _timeFrame = value;
                NotifyPropertyChanged(TimeFrameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TimeFrameChangedEventArgs = ObservableHelper.CreateArgs<NemoScenario>(x => x.TimeFrame);
        string _timeFrame;

        #endregion

        #region public List<NemoPlatform> Platforms { get; set; }
        [XmlElement("Platform", IsNullable = true)]
        public List<NemoPlatform> Platforms
        {
            get { return _platforms; }
            set
            {
                if (_platforms == value) return;
                _platforms = value;
                NotifyPropertyChanged(PlatformsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs PlatformsChangedEventArgs = ObservableHelper.CreateArgs<NemoScenario>(x => x.Platforms);
        List<NemoPlatform> _platforms;

        #endregion

        #region public List<NemoSpecies> Species { get; set; }
        [XmlElement("animals", IsNullable = true)]
        public List<NemoSpecies> Species
        {
            get { return _species; }
            set
            {
                if (_species == value) return;
                _species = value;
                NotifyPropertyChanged(SpeciesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SpeciesChangedEventArgs = ObservableHelper.CreateArgs<NemoScenario>(x => x.Species);
        List<NemoSpecies> _species;

        #endregion
    }

    public class NemoPlatform : PropertyChangedBase
    {
        #region public string PsmName { get; set; }
        [XmlElement("psmName", IsNullable = true)]
        public string PsmName
        {
            get { return _psmName; }
            set
            {
                if (_psmName == value) return;
                _psmName = value;
                NotifyPropertyChanged(PsmNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs PsmNameChangedEventArgs = ObservableHelper.CreateArgs<NemoPlatform>(x => x.PsmName);
        string _psmName;

        #endregion

        #region public string PsmId { get; set; }
        [XmlElement("psmId", IsNullable = true)]
        public string PsmId
        {
            get { return _psmId; }
            set
            {
                if (_psmId == value) return;
                _psmId = value;
                NotifyPropertyChanged(PsmIdChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs PsmIdChangedEventArgs = ObservableHelper.CreateArgs<NemoPlatform>(x => x.PsmId);
        string _psmId;

        #endregion

        #region public string Name { get; set; }
        [XmlElement("name", IsNullable = true)]
        public string Name
        {
            get { return _name; }
            set
            {
                if (_name == value) return;
                _name = value;
                NotifyPropertyChanged(NameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<NemoPlatform>(x => x.Name);
        string _name;

        #endregion

        #region public string Id { get; set; }
        [XmlElement("id", IsNullable = true)]
        public string Id
        {
            get { return _id; }
            set
            {
                if (_id == value) return;
                _id = value;
                NotifyPropertyChanged(IdChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IdChangedEventArgs = ObservableHelper.CreateArgs<NemoPlatform>(x => x.Id);
        string _id;

        #endregion

        #region public string StartTime { get; set; }
        [XmlElement("startTime", IsNullable = true)]
        public string StartTime
        {
            get { return _startTime; }
            set
            {
                if (_startTime == value) return;
                _startTime = value;
                NotifyPropertyChanged(StartTimeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs StartTimeChangedEventArgs = ObservableHelper.CreateArgs<NemoPlatform>(x => x.StartTime);
        string _startTime;

        #endregion

        #region public string Duration { get; set; }
        [XmlElement("duration", IsNullable = true)]
        public string Duration
        {
            get { return _duration; }
            set
            {
                if (_duration == value) return;
                _duration = value;
                NotifyPropertyChanged(DurationChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DurationChangedEventArgs = ObservableHelper.CreateArgs<NemoPlatform>(x => x.Duration);
        string _duration;

        #endregion

        #region public int? Priority { get; set; }
        [XmlElement("priority", IsNullable = true)]
        public int? Priority
        {
            get { return _priority; }
            set
            {
                if (_priority == value) return;
                _priority = value;
                NotifyPropertyChanged(PriorityChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs PriorityChangedEventArgs = ObservableHelper.CreateArgs<NemoPlatform>(x => x.Priority);
        int? _priority;

        #endregion

        #region public bool? UseParentTimeSpan { get; set; }
        [XmlElement("useParentTimeSpan", IsNullable = true)]
        public bool? UseParentTimeSpan
        {
            get { return _useParentTimeSpan; }
            set
            {
                if (_useParentTimeSpan == value) return;
                _useParentTimeSpan = value;
                NotifyPropertyChanged(UseParentTimeSpanChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs UseParentTimeSpanChangedEventArgs = ObservableHelper.CreateArgs<NemoPlatform>(x => x.UseParentTimeSpan);
        bool? _useParentTimeSpan;

        #endregion

        #region public string Type { get; set; }
        [XmlElement("type", IsNullable = true)]
        public string Type
        {
            get { return _type; }
            set
            {
                if (_type == value) return;
                _type = value;
                NotifyPropertyChanged(TypeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TypeChangedEventArgs = ObservableHelper.CreateArgs<NemoPlatform>(x => x.Type);
        string _type;

        #endregion

        #region public string Description { get; set; }
        [XmlElement("description", IsNullable = true)]
        public string Description
        {
            get { return _description; }
            set
            {
                if (_description == value) return;
                _description = value;
                NotifyPropertyChanged(DescriptionChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DescriptionChangedEventArgs = ObservableHelper.CreateArgs<NemoPlatform>(x => x.Description);
        string _description;

        #endregion

        #region public string Launcher { get; set; }
        [XmlElement("launcher", IsNullable = true)]
        public string Launcher
        {
            get { return _launcher; }
            set
            {
                if (_launcher == value) return;
                _launcher = value;
                NotifyPropertyChanged(LauncherChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LauncherChangedEventArgs = ObservableHelper.CreateArgs<NemoPlatform>(x => x.Launcher);
        string _launcher;

        #endregion

        #region public string Towwer { get; set; }
        [XmlElement("towwer", IsNullable = true)]
        public string Towwer
        {
            get { return _towwer; }
            set
            {
                if (_towwer == value) return;
                _towwer = value;
                NotifyPropertyChanged(TowwerChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TowwerChangedEventArgs = ObservableHelper.CreateArgs<NemoPlatform>(x => x.Towwer);
        string _towwer;

        #endregion

        #region public int? RepeatCount { get; set; }
        [XmlElement("repeatCount", IsNullable = true)]
        public int? RepeatCount
        {
            get { return _repeatCount; }
            set
            {
                if (_repeatCount == value) return;
                _repeatCount = value;
                NotifyPropertyChanged(RepeatCountChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RepeatCountChangedEventArgs = ObservableHelper.CreateArgs<NemoPlatform>(x => x.RepeatCount);
        int? _repeatCount;

        #endregion

        #region public List<NemoSource> Sources { get; set; }
        [XmlElement("Source", IsNullable = true)]
        public List<NemoSource> Sources
        {
            get { return _sources; }
            set
            {
                if (_sources == value) return;
                _sources = value;
                NotifyPropertyChanged(SourcesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SourcesChangedEventArgs = ObservableHelper.CreateArgs<NemoPlatform>(x => x.Sources);
        List<NemoSource> _sources;

        #endregion

        #region public NemoTrackdef TrackDef { get; set; }
        [XmlElement("trackDef", IsNullable = true)]
        public NemoTrackdef TrackDef
        {
            get { return _trackDef; }
            set
            {
                if (_trackDef == value) return;
                _trackDef = value;
                NotifyPropertyChanged(TrackDefChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TrackDefChangedEventArgs = ObservableHelper.CreateArgs<NemoPlatform>(x => x.TrackDef);
        NemoTrackdef _trackDef;

        #endregion
    }

    public class NemoTrackdef : PropertyChangedBase
    {
        #region public string TrackType { get; set; }
        [XmlElement("trackType", IsNullable = true)]
        public string TrackType
        {
            get { return _trackType; }
            set
            {
                if (_trackType == value) return;
                _trackType = value;
                NotifyPropertyChanged(TrackTypeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TrackTypeChangedEventArgs = ObservableHelper.CreateArgs<NemoTrackdef>(x => x.TrackType);
        string _trackType;

        #endregion

        #region public string StartTime { get; set; }
        [XmlElement("startTime", IsNullable = true)]
        public string StartTime
        {
            get { return _startTime; }
            set
            {
                if (_startTime == value) return;
                _startTime = value;
                NotifyPropertyChanged(StartTimeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs StartTimeChangedEventArgs = ObservableHelper.CreateArgs<NemoTrackdef>(x => x.StartTime);
        string _startTime;

        #endregion

        #region public string Duration { get; set; }
        [XmlElement("duration", IsNullable = true)]
        public string Duration
        {
            get { return _duration; }
            set
            {
                if (_duration == value) return;
                _duration = value;
                NotifyPropertyChanged(DurationChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DurationChangedEventArgs = ObservableHelper.CreateArgs<NemoTrackdef>(x => x.Duration);
        string _duration;

        #endregion

        #region public bool? Random { get; set; }
        [XmlElement("random", IsNullable = true)]
        public bool? Random
        {
            get { return _random; }
            set
            {
                if (_random == value) return;
                _random = value;
                NotifyPropertyChanged(RandomChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RandomChangedEventArgs = ObservableHelper.CreateArgs<NemoTrackdef>(x => x.Random);
        bool? _random;

        #endregion

        #region public bool? OpsBounds { get; set; }
        [XmlElement("opsBounds", IsNullable = true)]
        public bool? OpsBounds
        {
            get { return _opsBounds; }
            set
            {
                if (_opsBounds == value) return;
                _opsBounds = value;
                NotifyPropertyChanged(OpsBoundsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OpsBoundsChangedEventArgs = ObservableHelper.CreateArgs<NemoTrackdef>(x => x.OpsBounds);
        bool? _opsBounds;

        #endregion

        #region public bool? OpsTimes { get; set; }
        [XmlElement("opsTimes", IsNullable = true)]
        public bool? OpsTimes
        {
            get { return _opsTimes; }
            set
            {
                if (_opsTimes == value) return;
                _opsTimes = value;
                NotifyPropertyChanged(OpsTimesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OpsTimesChangedEventArgs = ObservableHelper.CreateArgs<NemoTrackdef>(x => x.OpsTimes);
        bool? _opsTimes;

        #endregion

        #region public double? InitialLatitude { get; set; }
        [XmlElement("initialLatitude", IsNullable = true)]
        public double? InitialLatitude
        {
            get { return _initialLatitude; }
            set
            {
                if (_initialLatitude == value) return;
                _initialLatitude = value;
                NotifyPropertyChanged(InitialLatitudeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs InitialLatitudeChangedEventArgs = ObservableHelper.CreateArgs<NemoTrackdef>(x => x.InitialLatitude);
        double? _initialLatitude;

        #endregion

        #region public double? InitialLongitude { get; set; }
        [XmlElement("initialLongitude", IsNullable = true)]
        public double? InitialLongitude
        {
            get { return _initialLongitude; }
            set
            {
                if (_initialLongitude == value) return;
                _initialLongitude = value;
                NotifyPropertyChanged(InitialLongitudeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs InitialLongitudeChangedEventArgs = ObservableHelper.CreateArgs<NemoTrackdef>(x => x.InitialLongitude);
        double? _initialLongitude;

        #endregion

        #region public float? InitialHeight { get; set; }
        [XmlElement("initialHeight", IsNullable = true)]
        public float? InitialHeight
        {
            get { return _initialHeight; }
            set
            {
                if (_initialHeight == value) return;
                _initialHeight = value;
                NotifyPropertyChanged(InitialHeightChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs InitialHeightChangedEventArgs = ObservableHelper.CreateArgs<NemoTrackdef>(x => x.InitialHeight);
        float? _initialHeight;

        #endregion

        #region public double? InitialCourse { get; set; }
        [XmlElement("initialCourse", IsNullable = true)]
        public double? InitialCourse
        {
            get { return _initialCourse; }
            set
            {
                if (_initialCourse == value) return;
                _initialCourse = value;
                NotifyPropertyChanged(InitialCourseChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs InitialCourseChangedEventArgs = ObservableHelper.CreateArgs<NemoTrackdef>(x => x.InitialCourse);
        double? _initialCourse;

        #endregion

        #region public float? InitialSpeed { get; set; }
        [XmlElement("initialSpeed", IsNullable = true)]
        public float? InitialSpeed
        {
            get { return _initialSpeed; }
            set
            {
                if (_initialSpeed == value) return;
                _initialSpeed = value;
                NotifyPropertyChanged(InitialSpeedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs InitialSpeedChangedEventArgs = ObservableHelper.CreateArgs<NemoTrackdef>(x => x.InitialSpeed);
        float? _initialSpeed;

        #endregion

        #region public string LimitFileName { get; set; }
        [XmlElement("limitFileName", IsNullable = true)]
        public string LimitFileName
        {
            get { return _limitFileName; }
            set
            {
                if (_limitFileName == value) return;
                _limitFileName = value;
                NotifyPropertyChanged(LimitFileNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LimitFileNameChangedEventArgs = ObservableHelper.CreateArgs<NemoTrackdef>(x => x.LimitFileName);
        string _limitFileName;

        #endregion
    }

    public class NemoSource : PropertyChangedBase
    {
        #region public string PsmName { get; set; }
        [XmlElement("psmName", IsNullable = true)]
        public string PsmName
        {
            get { return _psmName; }
            set
            {
                if (_psmName == value) return;
                _psmName = value;
                NotifyPropertyChanged(PsmNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs PsmNameChangedEventArgs = ObservableHelper.CreateArgs<NemoSource>(x => x.PsmName);
        string _psmName;

        #endregion

        #region public string PsmId { get; set; }
        [XmlElement("psmId", IsNullable = true)]
        public string PsmId
        {
            get { return _psmId; }
            set
            {
                if (_psmId == value) return;
                _psmId = value;
                NotifyPropertyChanged(PsmIdChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs PsmIdChangedEventArgs = ObservableHelper.CreateArgs<NemoSource>(x => x.PsmId);
        string _psmId;

        #endregion

        #region public string Name { get; set; }
        [XmlElement("name", IsNullable = true)]
        public string Name
        {
            get { return _name; }
            set
            {
                if (_name == value) return;
                _name = value;
                NotifyPropertyChanged(NameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<NemoSource>(x => x.Name);
        string _name;

        #endregion

        #region public string Id { get; set; }
        [XmlElement("id", IsNullable = true)]
        public string Id
        {
            get { return _id; }
            set
            {
                if (_id == value) return;
                _id = value;
                NotifyPropertyChanged(IdChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IdChangedEventArgs = ObservableHelper.CreateArgs<NemoSource>(x => x.Id);
        string _id;

        #endregion

        #region public string StartTime { get; set; }
        [XmlElement("startTime", IsNullable = true)]
        public string StartTime
        {
            get { return _startTime; }
            set
            {
                if (_startTime == value) return;
                _startTime = value;
                NotifyPropertyChanged(StartTimeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs StartTimeChangedEventArgs = ObservableHelper.CreateArgs<NemoSource>(x => x.StartTime);
        string _startTime;

        #endregion

        #region public string Duration { get; set; }
        [XmlElement("duration", IsNullable = true)]
        public string Duration
        {
            get { return _duration; }
            set
            {
                if (_duration == value) return;
                _duration = value;
                NotifyPropertyChanged(DurationChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DurationChangedEventArgs = ObservableHelper.CreateArgs<NemoSource>(x => x.Duration);
        string _duration;

        #endregion

        #region public int? Priority { get; set; }
        [XmlElement("priority", IsNullable = true)]
        public int? Priority
        {
            get { return _priority; }
            set
            {
                if (_priority == value) return;
                _priority = value;
                NotifyPropertyChanged(PriorityChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs PriorityChangedEventArgs = ObservableHelper.CreateArgs<NemoSource>(x => x.Priority);
        int? _priority;

        #endregion

        #region public bool? UseParentTimeSpan { get; set; }
        [XmlElement("useParentTimeSpan", IsNullable = true)]
        public bool? UseParentTimeSpan
        {
            get { return _useParentTimeSpan; }
            set
            {
                if (_useParentTimeSpan == value) return;
                _useParentTimeSpan = value;
                NotifyPropertyChanged(UseParentTimeSpanChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs UseParentTimeSpanChangedEventArgs = ObservableHelper.CreateArgs<NemoSource>(x => x.UseParentTimeSpan);
        bool? _useParentTimeSpan;

        #endregion

        #region public string Type { get; set; }
        [XmlElement("type", IsNullable = true)]
        public string Type
        {
            get { return _type; }
            set
            {
                if (_type == value) return;
                _type = value;
                NotifyPropertyChanged(TypeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TypeChangedEventArgs = ObservableHelper.CreateArgs<NemoSource>(x => x.Type);
        string _type;

        #endregion

        #region public string Description { get; set; }
        [XmlElement("description", IsNullable = true)]
        public string Description
        {
            get { return _description; }
            set
            {
                if (_description == value) return;
                _description = value;
                NotifyPropertyChanged(DescriptionChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DescriptionChangedEventArgs = ObservableHelper.CreateArgs<NemoSource>(x => x.Description);
        string _description;

        #endregion

        #region public List<NemoMode> Modes { get; set; }
        [XmlElement("Mode", IsNullable = true)]
        public List<NemoMode> Modes
        {
            get { return _modes; }
            set
            {
                if (_modes == value) return;
                _modes = value;
                NotifyPropertyChanged(ModesChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ModesChangedEventArgs = ObservableHelper.CreateArgs<NemoSource>(x => x.Modes);
        List<NemoMode> _modes;

        #endregion
    }

    public class NemoMode : PropertyChangedBase
    {
        #region public string PsmName { get; set; }
        [XmlElement("psmName", IsNullable = true)]
        public string PsmName
        {
            get { return _psmName; }
            set
            {
                if (_psmName == value) return;
                _psmName = value;
                NotifyPropertyChanged(PsmNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs PsmNameChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.PsmName);
        string _psmName;

        #endregion

        #region public string PsmId { get; set; }
        [XmlElement("psmId", IsNullable = true)]
        public string PsmId
        {
            get { return _psmId; }
            set
            {
                if (_psmId == value) return;
                _psmId = value;
                NotifyPropertyChanged(PsmIdChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs PsmIdChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.PsmId);
        string _psmId;

        #endregion

        #region public string Name { get; set; }
        [XmlElement("name", IsNullable = true)]
        public string Name
        {
            get { return _name; }
            set
            {
                if (_name == value) return;
                _name = value;
                NotifyPropertyChanged(NameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.Name);
        string _name;

        #endregion

        #region public string Id { get; set; }
        [XmlElement("id", IsNullable = true)]
        public string Id
        {
            get { return _id; }
            set
            {
                if (_id == value) return;
                _id = value;
                NotifyPropertyChanged(IdChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IdChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.Id);
        string _id;

        #endregion

        #region public string StartTime { get; set; }
        [XmlElement("startTime", IsNullable = true)]
        public string StartTime
        {
            get { return _startTime; }
            set
            {
                if (_startTime == value) return;
                _startTime = value;
                NotifyPropertyChanged(StartTimeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs StartTimeChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.StartTime);
        string _startTime;

        #endregion

        #region public string Duration { get; set; }
        [XmlElement("duration", IsNullable = true)]
        public string Duration
        {
            get { return _duration; }
            set
            {
                if (_duration == value) return;
                _duration = value;
                NotifyPropertyChanged(DurationChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DurationChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.Duration);
        string _duration;

        #endregion

        #region public int? Priority { get; set; }
        [XmlElement("priority", IsNullable = true)]
        public int? Priority
        {
            get { return _priority; }
            set
            {
                if (_priority == value) return;
                _priority = value;
                NotifyPropertyChanged(PriorityChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs PriorityChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.Priority);
        int? _priority;

        #endregion

        #region public bool? UseParentTimeSpan { get; set; }
        [XmlElement("useParentTimeSpan", IsNullable = true)]
        public bool? UseParentTimeSpan
        {
            get { return _useParentTimeSpan; }
            set
            {
                if (_useParentTimeSpan == value) return;
                _useParentTimeSpan = value;
                NotifyPropertyChanged(UseParentTimeSpanChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs UseParentTimeSpanChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.UseParentTimeSpan);
        bool? _useParentTimeSpan;

        #endregion

        #region public string Type { get; set; }
        [XmlElement("type", IsNullable = true)]
        public string Type
        {
            get { return _type; }
            set
            {
                if (_type == value) return;
                _type = value;
                NotifyPropertyChanged(TypeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TypeChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.Type);
        string _type;

        #endregion

        #region public string State { get; set; }
        [XmlElement("state", IsNullable = true)]
        public string State
        {
            get { return _state; }
            set
            {
                if (_state == value) return;
                _state = value;
                NotifyPropertyChanged(StateChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs StateChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.State);
        string _state;

        #endregion

        #region public string Linked { get; set; }
        [XmlElement("linked", IsNullable = true)]
        public string Linked
        {
            get { return _linked; }
            set
            {
                if (_linked == value) return;
                _linked = value;
                NotifyPropertyChanged(LinkedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LinkedChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.Linked);
        string _linked;

        #endregion

        #region public float? ActiveTime { get; set; }
        [XmlElement("activeTime", IsNullable = true)]
        public float? ActiveTime
        {
            get { return _activeTime; }
            set
            {
                if (_activeTime == value) return;
                _activeTime = value;
                NotifyPropertyChanged(ActiveTimeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ActiveTimeChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.ActiveTime);
        float? _activeTime;

        #endregion

        #region public float? DepthOffset { get; set; }
        [XmlElement("depthOffset", IsNullable = true)]
        public float? DepthOffset
        {
            get { return _depthOffset; }
            set
            {
                if (_depthOffset == value) return;
                _depthOffset = value;
                NotifyPropertyChanged(DepthOffsetChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DepthOffsetChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.DepthOffset);
        float? _depthOffset;

        #endregion

        #region public float? LowFrequency { get; set; }
        [XmlElement("lowFrequency", IsNullable = true)]
        public float? LowFrequency
        {
            get { return _lowFrequency; }
            set
            {
                if (_lowFrequency == value) return;
                _lowFrequency = value;
                NotifyPropertyChanged(LowFrequencyChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LowFrequencyChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.LowFrequency);
        float? _lowFrequency;

        #endregion

        #region public float? HighFrequency { get; set; }
        [XmlElement("highFrequency", IsNullable = true)]
        public float? HighFrequency
        {
            get { return _highFrequency; }
            set
            {
                if (_highFrequency == value) return;
                _highFrequency = value;
                NotifyPropertyChanged(HighFrequencyChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs HighFrequencyChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.HighFrequency);
        float? _highFrequency;

        #endregion

        #region public string PulseInterval { get; set; }
        [XmlElement("pulseInterval", IsNullable = true)]
        public string PulseInterval
        {
            get { return _pulseInterval; }
            set
            {
                if (_pulseInterval == value) return;
                _pulseInterval = value;
                NotifyPropertyChanged(PulseIntervalChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs PulseIntervalChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.PulseInterval);
        string _pulseInterval;

        #endregion

        #region public float? PulseLength { get; set; }
        [XmlElement("pulseLength", IsNullable = true)]
        public float? PulseLength
        {
            get { return _pulseLength; }
            set
            {
                if (_pulseLength == value) return;
                _pulseLength = value;
                NotifyPropertyChanged(PulseLengthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs PulseLengthChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.PulseLength);
        float? _pulseLength;

        #endregion

        #region public float? HorizontalBeamWidth { get; set; }
        [XmlElement("horizontalBeamWidth", IsNullable = true)]
        public float? HorizontalBeamWidth
        {
            get { return _horizontalBeamWidth; }
            set
            {
                if (_horizontalBeamWidth == value) return;
                _horizontalBeamWidth = value;
                NotifyPropertyChanged(HorizontalBeamWidthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs HorizontalBeamWidthChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.HorizontalBeamWidth);
        float? _horizontalBeamWidth;

        #endregion

        #region public float? VerticalBeamWidth { get; set; }
        [XmlElement("verticalBeamWidth", IsNullable = true)]
        public float? VerticalBeamWidth
        {
            get { return _verticalBeamWidth; }
            set
            {
                if (_verticalBeamWidth == value) return;
                _verticalBeamWidth = value;
                NotifyPropertyChanged(VerticalBeamWidthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs VerticalBeamWidthChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.VerticalBeamWidth);
        float? _verticalBeamWidth;

        #endregion

        #region public float? DepthElAngle { get; set; }
        [XmlElement("depthElAngle", IsNullable = true)]
        public float? DepthElAngle
        {
            get { return _depthElAngle; }
            set
            {
                if (_depthElAngle == value) return;
                _depthElAngle = value;
                NotifyPropertyChanged(DepthElAngleChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DepthElAngleChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.DepthElAngle);
        float? _depthElAngle;

        #endregion

        #region public float? RelativeBeamAngle { get; set; }
        [XmlElement("relativeBeamAngle", IsNullable = true)]
        public float? RelativeBeamAngle
        {
            get { return _relativeBeamAngle; }
            set
            {
                if (_relativeBeamAngle == value) return;
                _relativeBeamAngle = value;
                NotifyPropertyChanged(RelativeBeamAngleChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RelativeBeamAngleChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.RelativeBeamAngle);
        float? _relativeBeamAngle;

        #endregion

        #region public float? Radius { get; set; }
        [XmlElement("radius", IsNullable = true)]
        public float? Radius
        {
            get { return _radius; }
            set
            {
                if (_radius == value) return;
                _radius = value;
                NotifyPropertyChanged(RadiusChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RadiusChangedEventArgs = ObservableHelper.CreateArgs<NemoMode>(x => x.Radius);
        float? _radius;

        #endregion
    }

    public class NemoSpecies : PropertyChangedBase
    {
        #region public string RecordVersion { get; set; }
        [XmlElement("recordVersion", IsNullable = true)]
        public string RecordVersion
        {
            get { return _recordVersion; }
            set
            {
                if (_recordVersion == value) return;
                _recordVersion = value;
                NotifyPropertyChanged(RecordVersionChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RecordVersionChangedEventArgs = ObservableHelper.CreateArgs<NemoSpecies>(x => x.RecordVersion);
        string _recordVersion;

        #endregion

        #region public string SpeciesName { get; set; }
        [XmlElement("speciesName", IsNullable = true)]
        public string SpeciesName
        {
            get { return _speciesName; }
            set
            {
                if (_speciesName == value) return;
                _speciesName = value;
                NotifyPropertyChanged(SpeciesNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SpeciesNameChangedEventArgs = ObservableHelper.CreateArgs<NemoSpecies>(x => x.SpeciesName);
        string _speciesName;

        #endregion

        #region public string SpeciesFile { get; set; }
        [XmlElement("speciesFile", IsNullable = true)]
        public string SpeciesFile
        {
            get { return _speciesFile; }
            set
            {
                if (_speciesFile == value) return;
                _speciesFile = value;
                NotifyPropertyChanged(SpeciesFileChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SpeciesFileChangedEventArgs = ObservableHelper.CreateArgs<NemoSpecies>(x => x.SpeciesFile);
        string _speciesFile;

        #endregion

        #region public int? SpeciesCode { get; set; }
        [XmlElement("speciesCode", IsNullable = true)]
        public int? SpeciesCode
        {
            get { return _speciesCode; }
            set
            {
                if (_speciesCode == value) return;
                _speciesCode = value;
                NotifyPropertyChanged(SpeciesCodeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SpeciesCodeChangedEventArgs = ObservableHelper.CreateArgs<NemoSpecies>(x => x.SpeciesCode);
        int? _speciesCode;

        #endregion

        #region public int? TotalAnimats { get; set; }
        [XmlElement("totalAnimats", IsNullable = true)]
        public int? TotalAnimats
        {
            get { return _totalAnimats; }
            set
            {
                if (_totalAnimats == value) return;
                _totalAnimats = value;
                NotifyPropertyChanged(TotalAnimatsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TotalAnimatsChangedEventArgs = ObservableHelper.CreateArgs<NemoSpecies>(x => x.TotalAnimats);
        int? _totalAnimats;

        #endregion

        #region public int? Population { get; set; }
        [XmlElement("population", IsNullable = true)]
        public int? Population
        {
            get { return _population; }
            set
            {
                if (_population == value) return;
                _population = value;
                NotifyPropertyChanged(PopulationChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs PopulationChangedEventArgs = ObservableHelper.CreateArgs<NemoSpecies>(x => x.Population);
        int? _population;

        #endregion

        #region public float? TrackAreaPopulation { get; set; }
        [XmlElement("trackAreaPopulation", IsNullable = true)]
        public float? TrackAreaPopulation
        {
            get { return _trackAreaPopulation; }
            set
            {
                if (_trackAreaPopulation == value) return;
                _trackAreaPopulation = value;
                NotifyPropertyChanged(TrackAreaPopulationChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TrackAreaPopulationChangedEventArgs = ObservableHelper.CreateArgs<NemoSpecies>(x => x.TrackAreaPopulation);
        float? _trackAreaPopulation;

        #endregion

        #region public int? TrackAreaTotal { get; set; }
        [XmlElement("trackAreaTotal", IsNullable = true)]
        public int? TrackAreaTotal
        {
            get { return _trackAreaTotal; }
            set
            {
                if (_trackAreaTotal == value) return;
                _trackAreaTotal = value;
                NotifyPropertyChanged(TrackAreaTotalChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TrackAreaTotalChangedEventArgs = ObservableHelper.CreateArgs<NemoSpecies>(x => x.TrackAreaTotal);
        int? _trackAreaTotal;

        #endregion

        #region public float? SimAreaPopulation { get; set; }
        [XmlElement("simAreaPopulation", IsNullable = true)]
        public float? SimAreaPopulation
        {
            get { return _simAreaPopulation; }
            set
            {
                if (_simAreaPopulation == value) return;
                _simAreaPopulation = value;
                NotifyPropertyChanged(SimAreaPopulationChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SimAreaPopulationChangedEventArgs = ObservableHelper.CreateArgs<NemoSpecies>(x => x.SimAreaPopulation);
        float? _simAreaPopulation;

        #endregion

        #region public int? SimAreaTotal { get; set; }
        [XmlElement("simAreaTotal", IsNullable = true)]
        public int? SimAreaTotal
        {
            get { return _simAreaTotal; }
            set
            {
                if (_simAreaTotal == value) return;
                _simAreaTotal = value;
                NotifyPropertyChanged(SimAreaTotalChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SimAreaTotalChangedEventArgs = ObservableHelper.CreateArgs<NemoSpecies>(x => x.SimAreaTotal);
        int? _simAreaTotal;

        #endregion
    }
}
