﻿using System.Collections.Generic;
using System.ComponentModel;
using Cinch;
using ESME;
using HRC.Collections;
using HRC.Plugins;
using MEFedMVVM.ViewModelLocator;

namespace DavesWPFTester
{
    [ExportViewModel("MainWindowViewModel")]
    public class MainWindowViewModel : ViewModelBase
    {
        public MainWindowViewModel() 
        {
            AllPlugins = PluginManager.FindPlugins<IESMEPlugin>(@"C:\Projects\ESME Deliverables\Plugins\Environmental Data Sources\InstallableNAVO\bin\Debug", 
                p => (p.PluginType == PluginType.EnvironmentalDataSource) && p.IsSelectable, k => k.Subtype);
#if false
            var context = new MainDataContext();
            var query = from it in context.Companies
                        orderby it.CompanyID
                        select it;

            foreach (Company comp in query)
                Console.WriteLine("{0} | {1} | {2}", comp.CompanyID, comp.CompanyName, comp.AddressCountry);

            Console.ReadLine();
#endif
        }

        #region public ObservableConcurrentDictionary<string, Dictionary<string, IESMEPlugin>> AllPlugins { get; set; }

        public ObservableConcurrentDictionary<string, Dictionary<string, IESMEPlugin>> AllPlugins
        {
            get { return _allPlugins; }
            set
            {
                if (_allPlugins == value) return;
                _allPlugins = value;
                NotifyPropertyChanged(AllPluginsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs AllPluginsChangedEventArgs = ObservableHelper.CreateArgs<MainWindowViewModel>(x => x.AllPlugins);
        ObservableConcurrentDictionary<string, Dictionary<string, IESMEPlugin>> _allPlugins;

        #endregion
    }

#if false
    [ExportViewModel("MainWindowViewModel")]
    public class MainWindowViewModel : ViewModelBase
    {
        readonly IMessageBoxService _messageBoxService;
        readonly IHRCOpenFileService _openFileService;
        readonly IHRCSaveFileService _saveFileService;
        readonly IViewAwareStatus _viewAwareStatus;
        readonly IUIVisualizerService _visualizerService;

        [ImportingConstructor]
        public MainWindowViewModel(IViewAwareStatus viewAwareStatus, IMessageBoxService messageBoxService, IHRCOpenFileService openFileService, IHRCSaveFileService saveFileService,
                                   IUIVisualizerService visualizerService)
        {
            var leftCenter = new Geo(42.3463356, -71.0976282);
            var rightCenter = new Geo(leftCenter.Offset(Geo.KilometersToRadians(0.03), Geo.DegreesToRadians(90)));
            LeftMap = new GoogleMap(leftCenter, 21);
            LeftMap.DownloadCompleted += DownloadCompleted;
            RightMap = new GoogleMap(rightCenter, 21);
            RightMap.DownloadCompleted += DownloadCompleted;
            var test = new RequestValidationServiceService();
            //test.processAOI()
        }

        void DownloadCompleted(object sender, EventArgs args)
        {
            if (LeftMap.IsDownloading || RightMap.IsDownloading) return;
            var rightPixels = new uint[RightMap.Image.PixelHeight];
            var leftPixels = new uint[LeftMap.Image.PixelHeight];
            Color[] leftColors, rightColors;
            RightMap.Image.CopyPixels(new Int32Rect(0, 0, 1, RightMap.Image.PixelHeight), rightPixels, 4, 0);
            rightColors = ToColorArray(rightPixels);
            for (var offset = 0; offset < LeftMap.Image.PixelWidth; offset++)
            {
                LeftMap.Image.CopyPixels(new Int32Rect(LeftMap.Image.PixelWidth - 1 - offset, 0, 1, LeftMap.Image.PixelHeight), leftPixels, 4, 0);
                leftColors = ToColorArray(leftPixels);
                var isMatch = true;
                for (var i = 0; i < rightPixels.Length; i++)
                {
                    if (rightPixels[i] != leftPixels[i])
                    {
                        isMatch = false;
                        break;
                    }
                }
                if (isMatch) Debugger.Break();
            }
        }

        Color[] ToColorArray(IList<uint> pixelValues)
        {
            var result = new Color[pixelValues.Count];
            for (var i = 0; i < pixelValues.Count; i++)
            {
                var a = (byte)(pixelValues[i] >> 24);
                var r = (byte)(pixelValues[i] >> 16);
                var g = (byte)(pixelValues[i] >> 8);
                var b = (byte)pixelValues[i];
                result[i] = Color.FromArgb(a, r, g, b);
            }
            return result;
        }

        #region public GoogleMap LeftMap { get; set; }

        public GoogleMap LeftMap
        {
            get { return _leftMap; }
            set
            {
                if (_leftMap == value) return;
                _leftMap = value;
                NotifyPropertyChanged(LeftMapChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LeftMapChangedEventArgs = ObservableHelper.CreateArgs<MainWindowViewModel>(x => x.LeftMap);
        GoogleMap _leftMap;

        #endregion

        #region public GoogleMap RightMap { get; set; }

        public GoogleMap RightMap
        {
            get { return _rightMap; }
            set
            {
                if (_rightMap == value) return;
                _rightMap = value;
                NotifyPropertyChanged(RightMapChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs RightMapChangedEventArgs = ObservableHelper.CreateArgs<MainWindowViewModel>(x => x.RightMap);
        GoogleMap _rightMap;

        #endregion
    }

    public class GoogleMap : PropertyChangedBase
    {
        const int MaxImageSize = 640;
        const int TrimHeight = 30;  // Trim off the Google logo at the bottom to allow edge matching to have a fighting chance to work

        const int PixelWidth = MaxImageSize - 1;
        const int PixelHeight = MaxImageSize - 1;

        public GoogleMap(Geo center, int zoomLevel) 
        {
            Location = center;
            ZoomLevel = zoomLevel;
            var rawImage = new BitmapImage(new Uri(string.Format(@"http://maps.googleapis.com/maps/api/staticmap?center={0:0.000000},{1:0.000000}&zoom={2}&size=639x639&maptype=hybrid&format=png32&feature:all%7Clement:labels%7Cvisibility:off&sensor=false", Location.Latitude, Location.Longitude, ZoomLevel)));
            IsDownloading = true;
            rawImage.DownloadCompleted += (s, e) =>
            {
                Image = new CroppedBitmap(rawImage, new Int32Rect(0, 0, PixelWidth, PixelHeight - TrimHeight));
                LocationX = MaxImageSize / 2;
                LocationY = (MaxImageSize - TrimHeight) / 2;
                IsDownloading = false;
                OnDownloadCompleted();
#if true
                using (var fileStream = new FileStream(string.Format("image{0:0.000000}_{1:0.000000}.jpg", center.Latitude, center.Longitude), FileMode.Create))
                {
                    var encoder = new JpegBitmapEncoder();
                    encoder.Frames.Add(BitmapFrame.Create(Image));
                    encoder.QualityLevel = 100;
                    encoder.Save(fileStream);
                }
#endif
            };
        }

        #region public Geo Location { get; set; }

        public Geo Location
        {
            get { return _location; }
            private set
            {
                if (_location == value) return;
                _location = value;
                NotifyPropertyChanged(CenterChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CenterChangedEventArgs = ObservableHelper.CreateArgs<GoogleMap>(x => x.Location);
        Geo _location;

        #endregion

        #region public int LocationX { get; set; }

        public int LocationX
        {
            get { return _locationX; }
            set
            {
                if (_locationX == value) return;
                _locationX = value;
                NotifyPropertyChanged(LocationXChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LocationXChangedEventArgs = ObservableHelper.CreateArgs<GoogleMap>(x => x.LocationX);
        int _locationX;

        #endregion

        #region public int LocationY { get; set; }

        public int LocationY
        {
            get { return _locationY; }
            set
            {
                if (_locationY == value) return;
                _locationY = value;
                NotifyPropertyChanged(LocationYChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LocationYChangedEventArgs = ObservableHelper.CreateArgs<GoogleMap>(x => x.LocationY);
        int _locationY;

        #endregion

        #region public int ZoomLevel { get; set; }

        public int ZoomLevel
        {
            get { return _zoomLevel; }
            private set
            {
                if (_zoomLevel == value) return;
                _zoomLevel = value;
                NotifyPropertyChanged(ZoomLevelChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ZoomLevelChangedEventArgs = ObservableHelper.CreateArgs<GoogleMap>(x => x.ZoomLevel);
        int _zoomLevel;

        #endregion

        #region public CroppedBitmap Image { get; set; }

        public CroppedBitmap Image
        {
            get { return _image; }
            set
            {
                if (_image == value) return;
                _image = value;
                NotifyPropertyChanged(CroppedImageChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CroppedImageChangedEventArgs = ObservableHelper.CreateArgs<GoogleMap>(x => x.Image);
        CroppedBitmap _image;

        #endregion

        #region public bool IsDownloading { get; set; }

        public bool IsDownloading
        {
            get { return _isDownloading; }
            private set
            {
                if (_isDownloading == value) return;
                _isDownloading = value;
                NotifyPropertyChanged(IsDownloadingChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsDownloadingChangedEventArgs = ObservableHelper.CreateArgs<GoogleMap>(x => x.IsDownloading);
        bool _isDownloading;

        #endregion


        public event EventHandler DownloadCompleted;
        protected virtual void OnDownloadCompleted()
        {
            if (DownloadCompleted != null)
                DownloadCompleted(this, new EventArgs());
        }
    }
#endif
}