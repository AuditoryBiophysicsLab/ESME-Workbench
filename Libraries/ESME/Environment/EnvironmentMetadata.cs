using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Text;
using System.Xml.Serialization;
using Cinch;
using ESME.Model;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment
{
    [Serializable]
    public class EnvironmentMetadata : SerializableData<EnvironmentMetadata>, IHasName, ISupportValidation, IMightBeDirty, IHaveBasePath
    {
        public EnvironmentMetadata() { PropertyChanged += delegate { IsDirty = true; }; }

        #region public string EnvironmentFilename { get; set; }

        public string EnvironmentFilename
        {
            get { return _environmentFilename; }
            set
            {
                if (_environmentFilename == value) return;
                _environmentFilename = value;
                NotifyPropertyChanged(EnvironmentFilenameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs EnvironmentFilenameChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentMetadata>(x => x.EnvironmentFilename);
        string _environmentFilename;

        #endregion

        #region public string TimePeriod { get; set; }

        public string TimePeriod
        {
            get { return _timePeriod; }
            set
            {
                if (_timePeriod == value) return;
                _timePeriod = value;
                NotifyPropertyChanged(TimePeriodChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TimePeriodChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentMetadata>(x => x.TimePeriod);
        string _timePeriod;

        #endregion

        #region public string CreationDateTime { get; set; }

        public string CreationDateTime
        {
            get { return _creationDateTime; }
            set
            {
                if (_creationDateTime == value) return;
                _creationDateTime = value;
                NotifyPropertyChanged(CreationDateTimeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CreationDateTimeChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentMetadata>(x => x.CreationDateTime);
        string _creationDateTime;

        #endregion

        #region public string BathymetryFilename { get; set; }

        public string BathymetryFilename
        {
            get { return _bathymetryFilename; }
            set
            {
                if (_bathymetryFilename == value) return;
                _bathymetryFilename = value;
                NotifyPropertyChanged(BathymetryFilenameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BathymetryFilenameChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentMetadata>(x => x.BathymetryFilename);
        string _bathymetryFilename;

        #endregion

        #region public string TemperatureSourceFilename { get; set; }

        public string TemperatureSourceFilename
        {
            get { return _temperatureSourceFilename; }
            set
            {
                if (_temperatureSourceFilename == value) return;
                _temperatureSourceFilename = value;
                NotifyPropertyChanged(TemperatureSourceFilenameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TemperatureSourceFilenameChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentMetadata>(x => x.TemperatureSourceFilename);
        string _temperatureSourceFilename;

        #endregion

        #region public string SalinitySourceFilename { get; set; }

        public string SalinitySourceFilename
        {
            get { return _salinitySourceFilename; }
            set
            {
                if (_salinitySourceFilename == value) return;
                _salinitySourceFilename = value;
                NotifyPropertyChanged(SalinitySourceFilenameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SalinitySourceFilenameChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentMetadata>(x => x.SalinitySourceFilename);
        string _salinitySourceFilename;

        #endregion

        #region public string WindspeedSourceFilename { get; set; }

        public string WindspeedSourceFilename
        {
            get { return _windspeedSourceFilename; }
            set
            {
                if (_windspeedSourceFilename == value) return;
                _windspeedSourceFilename = value;
                NotifyPropertyChanged(WindspeedSourceFilenameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs WindspeedSourceFilenameChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentMetadata>(x => x.WindspeedSourceFilename);
        string _windspeedSourceFilename;

        #endregion

        #region public string BottomtypeSourceFilename { get; set; }

        public string BottomtypeSourceFilename
        {
            get { return _bottomtypeSourceFilename; }
            set
            {
                if (_bottomtypeSourceFilename == value) return;
                _bottomtypeSourceFilename = value;
                NotifyPropertyChanged(BottomtypeSourceFilenameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BottomtypeSourceFilenameChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentMetadata>(x => x.BottomtypeSourceFilename);
        string _bottomtypeSourceFilename;

        #endregion

        #region public GeoRect BoundingBox { get; set; }

        public GeoRect BoundingBox
        {
            get { return _boundingBox; }
            set
            {
                if (_boundingBox == value) return;
                _boundingBox = value;
                NotifyPropertyChanged(BoundingBoxChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BoundingBoxChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentMetadata>(x => x.BoundingBox);
        GeoRect _boundingBox;

        #endregion

        #region public string BasePath { get; internal set; }

        [XmlIgnore]
        public string BasePath
        {
            get { return _basePath; }
            set
            {
                if (_basePath == value) return;
                _basePath = value;
                NotifyPropertyChanged(SimAreaBasePathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SimAreaBasePathChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentMetadata>(x => x.BasePath);
        string _basePath;

        #endregion

        [XmlIgnore]
        public string Name
        {
            get { return EnvironmentFilename; }
            set { EnvironmentFilename = value; }
        }

        [XmlIgnore]
        public bool IsValid { get; private set; }

        [XmlIgnore]
        public string ValidationErrorText { get; private set; }

        public void Validate()
        {
            if (string.IsNullOrEmpty(BasePath)) throw new ApplicationException("EnvironmentMetadata: Cannot validate, because BasePath is not set");
            if (!Directory.Exists(BasePath)) throw new ApplicationException("EnvironmentMetadata: Cannot validate, because the BasePath points to a directory that does not exist: \"" + BasePath + "\"");

            var envFile = Path.Combine(BasePath, "Environment", EnvironmentFilename);
            var bathyFile = Path.Combine(BasePath, "Bathymetry", BathymetryFilename);
            ValidationErrorText = "";

            if (!File.Exists(envFile)) ValidationErrorText += "Environment file does not exist";

            if (!File.Exists(bathyFile))
            {
                if (!string.IsNullOrEmpty(ValidationErrorText)) ValidationErrorText += ", ";
                ValidationErrorText += "Bathymetry file does not exist";
            }

            if (!string.IsNullOrEmpty(ValidationErrorText))
            {
                IsValid = false;
                return;
            }

            if (BoundingBox == null)
            {
                var bathymetry = Environment2DData.FromYXZ(bathyFile, -1);
                BoundingBox = new GeoRect(bathymetry.GeoRect);
            }

            IsValid = true;
        }

        [XmlIgnore]
        public bool IsDirty { get; set; }
    }


    [Serializable]
    public class BathymetryMetadata : SerializableData<BathymetryMetadata>, IHasName, ISupportValidation, IMightBeDirty, IHaveBasePath
    {
        public BathymetryMetadata() { PropertyChanged += delegate { IsDirty = true; }; }

        #region public string BathymetryFilename { get; set; }

        public string BathymetryFilename
        {
            get { return _bathymetryFilename; }
            set
            {
                if (_bathymetryFilename == value) return;
                _bathymetryFilename = value;
                NotifyPropertyChanged(BathymetryFilenameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BathymetryFilenameChangedEventArgs = ObservableHelper.CreateArgs<BathymetryMetadata>(x => x.BathymetryFilename);
        string _bathymetryFilename;

        #endregion

        #region public DateTime CreationDateTime { get; set; }

        public DateTime CreationDateTime
        {
            get { return _creationDateTime; }
            set
            {
                if (_creationDateTime == value) return;
                _creationDateTime = value;
                NotifyPropertyChanged(CreationDateTimeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CreationDateTimeChangedEventArgs = ObservableHelper.CreateArgs<BathymetryMetadata>(x => x.CreationDateTime);
        DateTime _creationDateTime;

        #endregion

        #region public string BathymetrySourceFilename { get; set; }

        public string BathymetrySourceFilename
        {
            get { return _bathymetrySourceFilename; }
            set
            {
                if (_bathymetrySourceFilename == value) return;
                _bathymetrySourceFilename = value;
                NotifyPropertyChanged(BathymetrySourceFilenameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BathymetrySourceFilenameChangedEventArgs = ObservableHelper.CreateArgs<BathymetryMetadata>(x => x.BathymetrySourceFilename);
        string _bathymetrySourceFilename;

        #endregion

        #region public string ExtractionCommand { get; set; }

        public string ExtractionCommand
        {
            get { return _extractionCommand; }
            set
            {
                if (_extractionCommand == value) return;
                _extractionCommand = value;
                NotifyPropertyChanged(ExtractionCommandChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExtractionCommandChangedEventArgs = ObservableHelper.CreateArgs<BathymetryMetadata>(x => x.ExtractionCommand);
        string _extractionCommand;

        #endregion

        #region public float Resolution { get; set; }

        /// <summary>
        ///   Resolution, in minutes
        /// </summary>
        public float Resolution
        {
            get { return _resolution; }
            set
            {
                if (_resolution == value) return;
                _resolution = value;
                NotifyPropertyChanged(ResolutionChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ResolutionChangedEventArgs = ObservableHelper.CreateArgs<BathymetryMetadata>(x => x.Resolution);
        float _resolution;

        #endregion

        #region public string OperationalAreaName { get; set; }

        public string OperationalAreaName
        {
            get { return _operationalAreaName; }
            set
            {
                if (_operationalAreaName == value) return;
                _operationalAreaName = value;
                NotifyPropertyChanged(OperationalAreaNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OperationalAreaNameChangedEventArgs = ObservableHelper.CreateArgs<BathymetryMetadata>(x => x.OperationalAreaName);
        string _operationalAreaName;

        #endregion

        #region public GeoRect BoundingBox { get; set; }

        public GeoRect BoundingBox
        {
            get { return _boundingBox; }
            set
            {
                if (_boundingBox == value) return;
                _boundingBox = value;
                NotifyPropertyChanged(BoundingBoxChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BoundingBoxChangedEventArgs = ObservableHelper.CreateArgs<BathymetryMetadata>(x => x.BoundingBox);
        GeoRect _boundingBox;

        #endregion

        #region public string BasePath { get; set; }

        [XmlIgnore]
        public string BasePath
        {
            get { return _basePath; }
            set
            {
                if (_basePath == value) return;
                _basePath = value;
                NotifyPropertyChanged(SimAreaBasePathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SimAreaBasePathChangedEventArgs = ObservableHelper.CreateArgs<BathymetryMetadata>(x => x.BasePath);
        string _basePath;

        #endregion

        [XmlIgnore]
        public string Name
        {
            get { return BathymetryFilename; }
            set { BathymetryFilename = value; }
        }

        [XmlIgnore]
        public bool IsValid { get; private set; }

        [XmlIgnore]
        public string ValidationErrorText { get; private set; }

        public void Validate()
        {
            if (string.IsNullOrEmpty(BasePath)) throw new ApplicationException("BathymetryMetadata: Cannot validate, because BasePath is not set");
            if (!Directory.Exists(BasePath)) throw new ApplicationException("BathymetryMetadata: Cannot validate, because the BasePath points to a directory that does not exist: \"" + BasePath + "\"");

            var bathyFile = Path.Combine(BasePath, "Bathymetry", BathymetryFilename);
            Environment2DData bathymetry = null;

            if (!File.Exists(bathyFile))
            {
                IsValid = false;
                ValidationErrorText = "Bathymetry file does not exist";
                return;
            }

            if (Resolution <= 0)
            {
                bathymetry = Environment2DData.FromYXZ(BathymetryFilename, -1);
                Resolution = (float) (bathymetry.LatitudinalResolution * 60);
            }

            if (string.IsNullOrEmpty(OperationalAreaName))
            {
                OperationalAreaName = Path.GetFileName(BasePath);
            }

            if (BoundingBox == null)
            {
                if (bathymetry == null) bathymetry = Environment2DData.FromYXZ(BathymetryFilename, -1);
                BoundingBox = new GeoRect(bathymetry.GeoRect);
            }

            IsValid = true;
        }

        [XmlIgnore]
        public bool IsDirty { get; set; }
    }


    [Serializable]
    public class MetadataFile<T> : SerializableData<MetadataFile<T>>, IList<T>, IMightBeDirty
        where T : class, IHasName, ISupportValidation, IMightBeDirty, IHaveBasePath, new()
    {
        static readonly Type[] ExtraTypes = {
                                                typeof (GeoRect), typeof (DateTime), typeof (string), typeof (float),
                                            };

        public static MetadataFile<T> Load(string fileName)
        {
            var result = Load(fileName, ExtraTypes);
            if (result == null) throw new ApplicationException("Unable to load metadata file: \"" + fileName + "\"");
            result._fileName = fileName;
            result.Validate();
            result.IsDirty = false;
            return result;
        }

        public void Save()
        {
            Save(_fileName, ExtraTypes);
            foreach (var item in _list) item.IsDirty = false;
            IsDirty = false;
        }

        string _fileName;

        readonly List<T> _list = new List<T>();

        public IEnumerator<T> GetEnumerator() { return _list.GetEnumerator(); }
        IEnumerator IEnumerable.GetEnumerator() { return _list.GetEnumerator(); }
        public void Add(T item) 
        { 
            _list.Add(item);
            IsDirty = true;
        }
        public void Clear()
        {
            _list.Clear();
            IsDirty = true;
        }
        public bool Contains(T item) { return _list.Contains(item); }
        public void CopyTo(T[] array, int arrayIndex) { _list.CopyTo(array); }
        public bool Remove(T item)
        {
            IsDirty = true;
            return _list.Remove(item);
        }

        [XmlIgnore]
        public int Count
        {
            get { return _list.Count; }
        }

        [XmlIgnore]
        public bool IsReadOnly
        {
            get { return false; }
        }

        public int IndexOf(T item) { return _list.IndexOf(item); }
        public void Insert(int index, T item)
        {
            _list.Insert(index, item);
            IsDirty = true;
        }

        public void RemoveAt(int index)
        {
            _list.RemoveAt(index);
            IsDirty = true;
        }

        public T this[int index]
        {
            get { return _list[index]; }
            set { _list[index] = value; }
        }

        public T this[string name]
        {
            get { return _list.Find(x => x.Name == name); }
        }

        [XmlIgnore]
        public bool IsValid { get; protected set; }

        [XmlIgnore]
        public string FileExtension { get; set; }

        [XmlIgnore]
        public string ValidationErrorText { get; protected set; }

        public virtual void Validate()
        {
            string searchPattern;
            
            if (FileExtension.StartsWith(".")) searchPattern = "*" + FileExtension;
            else searchPattern = "*." + FileExtension;

            var myFiles = Directory.GetFiles(Path.GetDirectoryName(_fileName), searchPattern);

            // Check all the files and add any that are not found in the list
            foreach (var fileName in myFiles.Select(Path.GetFileName).Where(fileName => this[fileName] == null))
                Add(new T
                    {
                        Name = fileName,
                    });

            var isValid = true;
            var sb = new StringBuilder();
            foreach (var element in _list)
            {
                element.BasePath = Path.GetDirectoryName(Path.GetDirectoryName(_fileName)); // this is essentially the directory containing the directory that contains file we loaded, or in simpler terms ".."
                element.Validate();
                if (element.IsValid) continue;
                isValid = false;
                sb.AppendLine(string.Format("{0}: {1}", element.Name, element.ValidationErrorText));
            }
            if (!isValid) ValidationErrorText = sb.ToString();
            IsValid = isValid;
        }

        [XmlIgnore]
        public bool IsDirty
        {
            get { return _isDirty || _list.Any(element => element.IsDirty); }
            set { _isDirty = value; }
        }

        bool _isDirty;
    }

    public class BathymetryMetadataFile : MetadataFile<BathymetryMetadata>
    {
        public BathymetryMetadataFile() { FileExtension = "txt"; }
    }

    public class EnvironmentMetadataFile : MetadataFile<EnvironmentMetadata>
    {
        public EnvironmentMetadataFile() { FileExtension = ".dat"; }
    }

    public class SimAreaMetadata : ViewModelBase
    {
        public SimAreaMetadata(string simAreaBasePath)
        {
            SimAreaBasePath = simAreaBasePath;
            EnvironmentPath = Path.Combine(SimAreaBasePath, "Environment");
            BathymetryPath = Path.Combine(SimAreaBasePath, "Bathymetry");
            BathymetryMetadataFile = (BathymetryMetadataFile) BathymetryMetadataFile.Load(Path.Combine(BathymetryPath, "bathymetry.xml"));
            EnvironmentMetadataFile = (EnvironmentMetadataFile) MetadataFile<EnvironmentMetadata>.Load(Path.Combine(BathymetryPath, "environment.xml"));
            BathymetryMetadataFile.Validate();
            EnvironmentMetadataFile.Validate();
        }

        #region public BathymetryMetadataFile BathymetryMetadataFile { get; set; }

        public BathymetryMetadataFile BathymetryMetadataFile
        {
            get { return _bathymetryMetadataFile; }
            set
            {
                if (_bathymetryMetadataFile == value) return;
                _bathymetryMetadataFile = value;
                NotifyPropertyChanged(BathymetryMetadataFileChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BathymetryMetadataFileChangedEventArgs = ObservableHelper.CreateArgs<SimAreaMetadata>(x => x.BathymetryMetadataFile);
        BathymetryMetadataFile _bathymetryMetadataFile;

        #endregion

        #region publicEnvironmentMetadataFile EnvironmentMetadataFile { get; set; }

        public EnvironmentMetadataFile EnvironmentMetadataFile
        {
            get { return _environmentMetadataFile; }
            set
            {
                if (_environmentMetadataFile == value) return;
                _environmentMetadataFile = value;
                NotifyPropertyChanged(EnvironmentMetadataFileChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs EnvironmentMetadataFileChangedEventArgs = ObservableHelper.CreateArgs<SimAreaMetadata>(x => x.EnvironmentMetadataFile);
        EnvironmentMetadataFile _environmentMetadataFile;

        #endregion

        #region public string BasePath { get; set; }

        public string SimAreaBasePath
        {
            get { return _simAreaBasePath; }
            set
            {
                if (_simAreaBasePath == value) return;
                _simAreaBasePath = value;
                NotifyPropertyChanged(SimAreaBasePathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SimAreaBasePathChangedEventArgs = ObservableHelper.CreateArgs<SimAreaMetadata>(x => x.SimAreaBasePath);
        string _simAreaBasePath;

        #endregion

        #region public string EnvironmentPath { get; set; }

        public string EnvironmentPath
        {
            get { return _environmentPath; }
            set
            {
                if (_environmentPath == value) return;
                _environmentPath = value;
                NotifyPropertyChanged(EnvironmentPathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs EnvironmentPathChangedEventArgs = ObservableHelper.CreateArgs<SimAreaMetadata>(x => x.EnvironmentPath);
        string _environmentPath;

        #endregion

        #region public string BathymetryPath { get; set; }

        public string BathymetryPath
        {
            get { return _bathymetryPath; }
            set
            {
                if (_bathymetryPath == value) return;
                _bathymetryPath = value;
                NotifyPropertyChanged(BathymetryPathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BathymetryPathChangedEventArgs = ObservableHelper.CreateArgs<SimAreaMetadata>(x => x.BathymetryPath);
        string _bathymetryPath;

        #endregion
    }
}