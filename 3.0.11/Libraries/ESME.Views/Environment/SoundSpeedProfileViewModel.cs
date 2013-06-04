using System.IO;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Media;
using ESME.Environment;
using HRC;
using HRC.Aspects;
using HRC.Plotting;
using HRC.Services;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.Environment
{
    public class SoundSpeedProfileViewModel : ViewModelBase
    {
        readonly IHRCSaveFileService _saveFileService;

        public SoundSpeedProfileView View { get; set; }
        public SoundSpeedProfileWindowView WindowView { get; set; }
        [Initialize("Sound Speed Profile")] public string WindowTitle { get; set; }
        public string OutputFileName { get; set; }
        [Initialize] public FourAxisSeriesViewModel FourAxisSeriesViewModel { get; set; }
        public SoundSpeedProfile SoundSpeedProfile { get; set; }
        void SoundSpeedProfileChanged()
        {
            //WindowTitle = string.Format("Sound Speed Profile ({0:0.000}, {1:0.000})", SoundSpeedProfile.Latitude, SoundSpeedProfile.Longitude);
            OutputFileName = string.Format("Sound Speed Profile lat {0:0.000} lon {1:0.000}", SoundSpeedProfile.Latitude, SoundSpeedProfile.Longitude);
            var seriesData = (from sample in SoundSpeedProfile.Data
                              select new Point(sample.SoundSpeed, sample.Depth)).ToList();
            if (FourAxisSeriesViewModel.DataSeriesCollection.Count > 0) foreach (SeriesViewModelBase series in FourAxisSeriesViewModel.DataSeriesCollection) series.SeriesData = null;
            FourAxisSeriesViewModel.DataSeriesCollection.Clear();
            FourAxisSeriesViewModel.XAxis.DataRange.Reset();
            FourAxisSeriesViewModel.YAxis.DataRange.Reset();
            var lineSeries = new LineSeriesViewModel
            {
                SeriesData = seriesData,
                MarkerType = SeriesMarkerType.Circle,
                ItemToPoint = i => (Point)i,
                MarkerStrokeThickness = 1,
                MarkerStroke = Brushes.Blue,
                MarkerSize = 10,
                SeriesName = "Depth vs. Sound Speed",
                LineStroke = Brushes.Black,
                LineStrokeThickness = 1,
            };
            FourAxisSeriesViewModel.YAxis.IsInverted = true;
            //FourAxisSeriesViewModel.XAxis.DataRange.Add(new Range(seriesData.Select(p => p.X).Min(), seriesData.Select(p => p.X).Max()));
            //FourAxisSeriesViewModel.YAxis.DataRange.Add(new Range(seriesData.Select(p => p.X).Min(), seriesData.Select(p => p.X).Max()));
            FourAxisSeriesViewModel.DataSeriesCollection.Add(lineSeries);
            FourAxisSeriesViewModel.XAxis.VisibleRange = FourAxisSeriesViewModel.XAxis.DataRange.Expand(FourAxisSeriesViewModel.XAxis.DataRange.Size * .05);
            FourAxisSeriesViewModel.XAxis.Label = "Sound speed (m/s)";
            FourAxisSeriesViewModel.YAxis.Label = "Depth (m)";
            FourAxisSeriesViewModel.PlotTitle = OutputFileName;
        }

        [UsedImplicitly] readonly PropertyObserver<SoundSpeedProfileViewModel> _propertyObserver;

        public SoundSpeedProfileViewModel(IHRCSaveFileService saveFile)
        {
            _saveFileService = saveFile;
            _propertyObserver = new PropertyObserver<SoundSpeedProfileViewModel>(this)
                .RegisterHandler(p => p.SoundSpeedProfile, SoundSpeedProfileChanged);
            var axisRanges = new RangeCollection();
            axisRanges.Add(new Range(0.1, 10));
            DesignTimeData = new SoundSpeedProfileViewModel
            {
                FourAxisSeriesViewModel = new FourAxisSeriesViewModel
                {
                    BottomAxis =
                    {
                        Visibility = Visibility.Visible,
                        Label = "Sound speed (m/s)",
                    },
                    LeftAxis =
                    {
                        Visibility = Visibility.Visible,
                        Label = "Depth (m)",
                        IsInverted = true,
                    },
                    TopAxis = { Visibility = Visibility.Collapsed },
                    RightAxis = { Visibility = Visibility.Collapsed },
                },
            };
            DesignTimeData.FourAxisSeriesViewModel.BottomAxis.DataRange = axisRanges;
            DesignTimeData.FourAxisSeriesViewModel.LeftAxis.DataRange = axisRanges;
        }

        /// <summary>
        /// This constructor is only used for design time data
        /// </summary>
        public SoundSpeedProfileViewModel() {}

        static SoundSpeedProfileViewModel()
        {
            var axisRanges = new RangeCollection();
            axisRanges.Add(new Range(0.1, 10));
            DesignTimeData = new SoundSpeedProfileViewModel
            {
                FourAxisSeriesViewModel = new FourAxisSeriesViewModel
                {
                    BottomAxis =
                        {
                            Visibility = Visibility.Visible,
                            Label = "Sound speed (m/s)",
                        },
                    LeftAxis =
                        {
                            Visibility = Visibility.Visible,
                            Label = "Depth (m)",
                            IsInverted = true,
                        },
                    TopAxis = { Visibility = Visibility.Collapsed },
                    RightAxis = { Visibility = Visibility.Collapsed },
                },
            };
            DesignTimeData.FourAxisSeriesViewModel.BottomAxis.DataRange = axisRanges;
            DesignTimeData.FourAxisSeriesViewModel.LeftAxis.DataRange = axisRanges;
        }
        public static SoundSpeedProfileViewModel DesignTimeData { get; private set; }
        #region commands

        #region SaveToCSVCommand
        public SimpleCommand<object, object> SaveToCSVCommand
        {
            get
            {
                return _saveToCSV ??
                       (_saveToCSV =
                        new SimpleCommand<object, object>(delegate { return SoundSpeedProfile != null; },
                                                          delegate
                                                          {
                                                              _saveFileService.Filter = "Comma-Separated Value (*.csv)|*.csv";
                                                              _saveFileService.OverwritePrompt = true;
                                                              _saveFileService.FileName = OutputFileName;
                                                              var result = _saveFileService.ShowDialog(WindowView);
                                                              if (result.HasValue && result.Value)
                                                              {
                                                                  SaveToCSVHandler(_saveFileService.FileName);
                                                              }
                                                          }));
            }
        }

        SimpleCommand<object, object> _saveToCSV;

        void SaveToCSVHandler(string fileName)
        {
            using (var writer = new StreamWriter(fileName))
            {
                writer.WriteLine(string.Format("#Sound Speed Profile ({0:0.000} {1:0.000})", SoundSpeedProfile.Latitude, SoundSpeedProfile.Longitude));
                writer.WriteLine("Depth (m),SoundSpeed (m/s)");
                foreach (var point in (from d in SoundSpeedProfile.Data orderby d.Depth select d))
                {
                    writer.WriteLine(string.Format("{0:0.00},{1:0.000}", point.Depth, point.SoundSpeed));
                }
            }
        }
        #endregion

        #region SaveToImageCommand
        public SimpleCommand<object, object> SaveToImageCommand
        {
            get
            {
                return _saveToImage ??
                       (_saveToImage =
                        new SimpleCommand<object, object>(delegate { return SoundSpeedProfile != null; },
                                                          delegate
                                                          {
                                                              _saveFileService.Filter =
                                                                  "Portable Network Graphics (*.png)|*.png|Bitmap (*.bmp)|*.bmp|GIF (*.gif)|*.gif|JPEG (*.jpg)|*.jpg|TIFF (*.tiff)|*.tiff";
                                                              _saveFileService.OverwritePrompt = true;
                                                              _saveFileService.FileName = OutputFileName;
                                                              var result = _saveFileService.ShowDialog(WindowView);
                                                              if (result.HasValue && result.Value) View.ToImageFile(_saveFileService.FileName);
                                                          }));
            }
        }

        SimpleCommand<object, object> _saveToImage;
        #endregion

        #region CopyTextToClipboardCommand
        public SimpleCommand<object, object> CopyTextToClipboardCommand
        {
            get
            {
                return _copyTextToClipboard ??
                       (_copyTextToClipboard =
                        new SimpleCommand<object, object>(delegate { return SoundSpeedProfile != null; },
                                                          delegate { CopyTextToClipboardHandler(); }));
            }
        }

        SimpleCommand<object, object> _copyTextToClipboard;

        void CopyTextToClipboardHandler()
        {
            var sb = new StringBuilder();
            sb.AppendLine(string.Format("#Sound Speed Profile ({0:0.000},{1:0.000})",
                                        SoundSpeedProfile.Latitude,
                                        SoundSpeedProfile.Longitude));
            sb.AppendLine("Depth (m),SoundSpeed (m/s)");
            foreach (var point in (from d in SoundSpeedProfile.Data orderby d.Depth select d))
            {
                sb.AppendLine(string.Format("{0:0.00},{1:0.000}", point.Depth, point.SoundSpeed));
            }
            Clipboard.SetText(sb.ToString());
        }
        #endregion

        #region CopyImageToClipboardCommand
        public SimpleCommand<object, object> CopyImageToClipboardCommand
        {
            get
            {
                return _copyImageToClipboard ??
                       (_copyImageToClipboard =
                        new SimpleCommand<object, object>(delegate { return SoundSpeedProfile != null; },
                                                          delegate { Clipboard.SetImage(View.ToBitmapSource()); }));
            }
        }

        SimpleCommand<object, object> _copyImageToClipboard;
        #endregion

        #region ViewClosingCommand
        public SimpleCommand<object, object> ViewClosingCommand { get { return _viewClosing ?? (_viewClosing = new SimpleCommand<object, object>(ViewClosingHandler)); } }
        SimpleCommand<object, object> _viewClosing;
        static void ViewClosingHandler(object o) { Properties.Settings.Default.Save(); }
        #endregion

        #endregion
    }
}