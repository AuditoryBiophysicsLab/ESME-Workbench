using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Windows.Threading;
using Cinch;
using HRC.Navigation;

namespace ESME.TransmissionLoss.REFMS
{
    public class ExplosivePoint : EarthCoordinate, INotifyPropertyChanged
    {
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

        #region public float Delta { get; set; }

        public float Delta
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
        float _delta;

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

        #region public float DepthLimit { get; set; }

        public float DepthLimit
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
        float _depthLimit;

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
    }

    public class SVPLayer
    {
        public double Thickness { get; set; }
        public double SoundVelocity { get; set; }
        public double Density { get; set; }
        public double Depth { get; set; }
        public override string ToString() { return string.Format("{0,13:0.0000} {1,13:0.0000} {2,9:0.00000}    0            D= {3,13:0.0000}", Thickness, SoundVelocity, Density, Depth); }
        public static SVPLayer Parse(string layerLine)
        {
            var fields = layerLine.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
            return new SVPLayer
            {
                Thickness = double.Parse(fields[0]),
                SoundVelocity = double.Parse(fields[1]),
                Density = double.Parse(fields[2]),
                Depth = double.Parse(fields[5]),
            };
        }

        /// <summary>
        /// Calculate the density of seawater, at a given depth, temperature and salinity
        /// </summary>
        /// <param name="depth">Depth, in meters</param>
        /// <param name="temp">Temperature, in degrees Celsius</param>
        /// <param name="salinity">Salinity, in parts per thousand</param>
        /// <returns></returns>
        public static double SaltWaterDensity(double depth, double temp, double salinity)
        {
            var depthKm = depth / 1000.0;
            var c = 999.83 + (5.053 * depthKm) - (0.048 * (depthKm * depthKm));
            var beta = 0.808 - (0.0085 * depthKm);
            var alpha = 0.0708 * (1.0 + (0.351 * depthKm) + 0.068 * (1.0 - (0.0683 * depthKm)) * temp);
            var gamma = 0.0030 * (1.0 - (0.059 * depthKm) - 0.012 * (1.0 - (0.0640 * depthKm)) * temp);

            return (c + (beta * salinity) - (alpha * temp) - (gamma * (35.0 - salinity)) * temp) / 1000.0;
        }
    }



    public class SVPFile : EarthCoordinate
    {
        public List<SVPLayer> Layers { get; set; }
        public double Delta;

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
