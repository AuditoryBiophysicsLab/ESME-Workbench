using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows.Threading;
using System.Xml.Serialization;
using Cinch;
using ESME.Environment;
using ESME.Model;
using ESME.NEMO;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.TransmissionLoss.REFMS
{
    public class ExplosivePoint : EarthCoordinate, ISupportValidation
    {
        public ExplosivePoint() { }

        public ExplosivePoint(Geo location, NemoPlatform platform, NemoMode mode, float delta) : base(location)
        {
            ExplosionDepth = Math.Abs(platform.Trackdefs[0].InitialHeight) + mode.DepthOffset;
            Delta = delta;
            SoundSources = new List<SoundSource> {new SoundSource(location, mode, 1)};
        }

        #region public Geo OldLocation { get; set; }

        public Geo OldLocation
        {
            get { return _oldLocation; }
            set
            {
                if (_oldLocation == value) return;
                _oldLocation = value;
                NotifyPropertyChanged(OldLocationChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OldLocationChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.OldLocation);
        Geo _oldLocation;

        #endregion

        #region public double ExplosionDepth { get; set; }

        public double ExplosionDepth
        {
            get { return _explosionDepth; }
            set
            {
                if (_explosionDepth == value) return;
                _explosionDepth = value;
                NotifyPropertyChanged(ExplosionDepthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExplosionDepthChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.ExplosionDepth);
        double _explosionDepth;

        #endregion

        #region public string SVPFileName { get; set; }

        public string SVPFileName
        {
            get { return _svpFileName; }
            set
            {
                if (_svpFileName == value) return;
                _svpFileName = value;
                NotifyPropertyChanged(SVPFileNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SVPFileNameChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.SVPFileName);
        string _svpFileName;

        #endregion

        #region public bool IsEnabled { get; set; }

        public bool IsEnabled
        {
            get { return _isEnabled; }
            set
            {
                if (_isEnabled == value) return;
                _isEnabled = value;
                NotifyPropertyChanged(IsEnabledChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsEnabledChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.IsEnabled);
        bool _isEnabled;

        #endregion

        #region public double Delta { get; set; }

        public double Delta
        {
            get { return _delta; }
            set
            {
                if (_delta == value) return;
                _delta = value;
                NotifyPropertyChanged(DeltaChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DeltaChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.Delta);
        double _delta;

        #endregion

        #region public int SourceCount { get; set; }

        public int SourceCount
        {
            get { return _sourceCount; }
            set
            {
                if (_sourceCount == value) return;
                _sourceCount = value;
                NotifyPropertyChanged(SourceCountChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SourceCountChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.SourceCount);
        int _sourceCount;

        #endregion

        #region public double DepthLimit { get; set; }

        public double DepthLimit
        {
            get { return _depthLimit; }
            set
            {
                if (_depthLimit == value) return;
                _depthLimit = value;
                NotifyPropertyChanged(DepthLimitChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DepthLimitChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.DepthLimit);
        double _depthLimit;

        #endregion

        #region public bool BottomReflectionsEnabled { get; set; }

        public bool BottomReflectionsEnabled
        {
            get { return _bottomReflectionsEnabled; }
            set
            {
                if (_bottomReflectionsEnabled == value) return;
                _bottomReflectionsEnabled = value;
                NotifyPropertyChanged(BottomReflectionsEnabledChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BottomReflectionsEnabledChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.BottomReflectionsEnabled);
        bool _bottomReflectionsEnabled;

        #endregion

        #region public bool BottomExponentialEnabled { get; set; }

        public bool BottomExponentialEnabled
        {
            get { return _bottomExponentialEnabled; }
            set
            {
                if (_bottomExponentialEnabled == value) return;
                _bottomExponentialEnabled = value;
                NotifyPropertyChanged(BottomExponentialEnabledChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BottomExponentialEnabledChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.BottomExponentialEnabled);
        bool _bottomExponentialEnabled;

        #endregion

        #region INotifyPropertyChanged Members

        public event PropertyChangedEventHandler PropertyChanged;
        protected void NotifyPropertyChanged(PropertyChangedEventArgs e)
        {
            var handlers = PropertyChanged;
            if (handlers == null) return;
            foreach (PropertyChangedEventHandler handler in handlers.GetInvocationList())
            {
                if (handler.Target is DispatcherObject)
                    ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => handler(this, e));
                else
                    handler(this, e);
            }
        }

        #endregion

        #region public WeakReference<Bathymetry> Bathymetry { get; set; }

        [XmlIgnore]
        public WeakReference<Bathymetry> Bathymetry
        {
            get { return _bathymetry ?? (_bathymetry = new WeakReference<Bathymetry>(null)); }
            set
            {
                if (_bathymetry == value) return;
                _bathymetry = value;
                foreach (var soundSource in SoundSources) soundSource.Bathymetry = _bathymetry;
            }
        }

        WeakReference<Bathymetry> _bathymetry;

        #endregion

        public List<SoundSource> SoundSources { get; set; }

        #region public bool IsValid { get; set; }

        [XmlIgnore]
        public bool IsValid
        {
            get
            {
                Validate();
                return _isValid;
            }
            private set
            {
                if (_isValid == value) return;
                _isValid = value;
                NotifyPropertyChanged(IsValidChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsValidChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.IsValid);
        bool _isValid;

        #endregion

        #region public string ValidationErrorText { get; set; }
        [XmlIgnore]
        public string ValidationErrorText
        {
            get
            {
                Validate();
                return _validationErrorText;
            }
            private set
            {
                if (_validationErrorText == value) return;
                _validationErrorText = value;
                IsValid = string.IsNullOrEmpty(_validationErrorText);
                NotifyPropertyChanged(ValidationErrorTextChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ValidationErrorTextChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePoint>(x => x.ValidationErrorText);
        string _validationErrorText;

        #endregion

        public void Validate()
        {
            if ((Bathymetry == null) || (Bathymetry.Target == null))
            {
                ValidationErrorText = "Unable to validate";
                return;
            }

            var bathymetry = Bathymetry.Target;
            if (!bathymetry.Samples.GeoRect.Contains(this))
            {
                ValidationErrorText = "Explosive point not contained within bathymetry bounds";
                return;
            }

            var sourcesInError = new List<string>();

            foreach (var source in SoundSources)
            {
                source.Validate();
                if (!source.IsValid) sourcesInError.Add(source.Name);
            }
            switch (sourcesInError.Count)
            {
                case 0:
                    ValidationErrorText = null;
                    break;
                case 1:
                    ValidationErrorText = string.Format("Source {0} has errors", sourcesInError[0]);
                    break;
                default:
                    var result = new StringBuilder();
                    result.AppendLine("The following sources have errors:");
                    foreach (var sourceName in sourcesInError) result.AppendLine(string.Format("  • {0}", sourceName));
                    ValidationErrorText = result.ToString();
                    break;
            }
        }
    }

    public class SVPLayer
    {
        public double Thickness { get; set; }
        public double SoundVelocity { get; set; }
        public double Depth { get; set; }
        public double Salinity { get; set; }
        public double Temperature { get; set; }
        public override string ToString() { return string.Format("{0,13:0.0000} {1,13:0.0000} {2,9:0.00000}    0            D= {3,13:0.0000}", Thickness, SoundVelocity, Density, Depth); }
        public static SVPLayer Parse(string layerLine)
        {
            var fields = layerLine.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
            return new SVPLayer
            {
                Thickness = double.Parse(fields[0]),
                SoundVelocity = double.Parse(fields[1]),
                //Density = double.Parse(fields[2]),
                Depth = double.Parse(fields[5]),
            };
        }

        /// <summary>
        /// Calculate the density of seawater, at a given depth, temperature and salinity
        /// </summary>
        public double Density
        {
            get
            {
                var depthKm = Depth / 1000.0;
                var c = 999.83 + (5.053 * depthKm) - (0.048 * (depthKm * depthKm));
                var beta = 0.808 - (0.0085 * depthKm);
                var alpha = 0.0708 * (1.0 + (0.351 * depthKm) + 0.068 * (1.0 - (0.0683 * depthKm)) * Temperature);
                var gamma = 0.0030 * (1.0 - (0.059 * depthKm) - 0.012 * (1.0 - (0.0640 * depthKm)) * Temperature);

                return (c + (beta * Salinity) - (alpha * Temperature) - (gamma * (35.0 - Salinity)) * Temperature) / 1000.0;
            }
        }
    }

    public class SVPFile : EarthCoordinate
    {
        public List<SVPLayer> Layers { get; private set; }
        public double Delta { get; private set; }

        public static SVPFile Create(double[] depths, double[] temps, double[] salinities, double[] soundspeeds, BottomLossData bottomLossData, double delta)
        {
            var maxDepth = depths.Last();
            // this creates the depth increments
            var nodes = (int)Math.Floor(maxDepth);
            var dinc = Math.Floor(maxDepth) / (nodes - 1);
            var depthIncs = new double[nodes];

            for (var m = 0; m < nodes; m++)
            {
                var tmp = Math.Round((m * dinc) * 1000000.0) / 1000000.0;
                depthIncs[m] = tmp;
            }
            // this implements the spline function
            var interp = new SplineInterpolator();
            var temperatureFunction = interp.Interpolate(depths, temps);
            var salinityFunction = interp.Interpolate(depths, salinities);
            var soundspeedFunction = interp.Interpolate(depths, soundspeeds);

            var td = new double[depthIncs.Length];
            var sd = new double[depthIncs.Length];
            var svpd = new double[depthIncs.Length];
            for (var inc = 0; inc < depthIncs.Length; inc++)
            {
                try
                {
                    td[inc] = temperatureFunction.Value(depthIncs[inc]);
                    sd[inc] = salinityFunction.Value(depthIncs[inc]);
                    svpd[inc] = soundspeedFunction.Value(depthIncs[inc]);
                }
                catch {}
            }
            // this is the implementation of the layers function

            nodes = depthIncs.Length;
            var result = new SVPFile {Layers = new List<SVPLayer>()};
            int start = 0, prev = 0;
            for (var inc = 0; inc < nodes; inc++)
            {
                var depthmid = inc == 0 ? depthIncs[0] * 0.5 : (depthIncs[inc] + depthIncs[inc - 1]) * 0.5;

                var deltaSVP = Math.Abs(svpd[inc] - svpd[start]);

                if (deltaSVP >= delta || (inc == (nodes - 1)))
                {
                    start = inc;
                    result.Layers.Add(new SVPLayer
                    {
                        Depth = depthIncs[inc],
                        Thickness = Math.Abs(depthIncs[prev] - depthIncs[start]),
                        SoundVelocity = soundspeedFunction.Value(depthmid),
                        Salinity = sd[inc],
                        Temperature = td[inc],
                    });
                    prev = inc;
                }
            }
            return result;
        }

        public void Write(string fileName)
        {
            using (var writer = new StreamWriter(fileName, false))
            {
                writer.WriteLine("Layers: {0}", Layers.Count);
                foreach (var layer in Layers) writer.WriteLine(layer);
                writer.WriteLine();
                writer.WriteLine();
                writer.WriteLine(" DELTA {0,9:0.0000}", Delta);
                writer.WriteLine("LOCATION: {0,10:##0.000000} {1,10:##0.000000}", Latitude, Longitude);
            }
        }

        public static SVPFile Read(string fileName)
        {
            var result = new SVPFile();
            using (var reader = new StreamReader(fileName))
            {
                var fields = reader.ReadLine().Split(new[] {' '}, StringSplitOptions.RemoveEmptyEntries);
                var fieldCount = int.Parse(fields[1]);
                result.Layers = new List<SVPLayer>();
                for (var i = 0; i < fieldCount; i++)
                    result.Layers.Add(SVPLayer.Parse(reader.ReadLine()));
                var curLine = reader.ReadLine().Trim();
                while (string.IsNullOrEmpty(curLine)) curLine = reader.ReadLine().Trim();
                fields = curLine.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                result.Delta = double.Parse(fields[1]);
                fields = reader.ReadLine().Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                result.Latitude = double.Parse(fields[1]);
                result.Longitude = double.Parse(fields[2]);
            }
            return result;
        }
    }
}
