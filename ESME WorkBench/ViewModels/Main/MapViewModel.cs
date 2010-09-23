using System;
using System.ComponentModel;
using System.IO;
using System.Reflection;
using System.Text;
using System.Threading;
using System.Windows;
using System.Windows.Input;
using Cinch;
using ESME.Environment;
using ESME.Model;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;
using ESMEWorkBench.ViewModels.Layers;
using ESMEWorkBench.ViewModels.TransmissionLoss;
using HRC.Navigation;
using MEFedMVVM.Common;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;
using BathymetryOutOfBoundsException = ESME.Environment.BathymetryOutOfBoundsException;

namespace ESMEWorkBench.ViewModels.Main
{
    public class MapViewModel : ViewModelBase
    {
        WpfMap _map;
        string _environmentFileName;

        bool _environmentFileLoaded;
        
        public string MapDLLVersion { get; private set; }

        public MapViewModel()
        {
            Globals.ViewAwareStatus.ViewLoaded += ViewAwareStatusServiceViewLoaded;

            Cursor = Cursors.Arrow;

            Globals.LayerDisplayViewModel = new LayerDisplayViewModel(this);

            ToggleBaseMapDisplayCommand = new SimpleCommand<object, object>(delegate (Object args)
            {
                var isChecked = (bool)args;
                BaseMapViewModel.IsChecked = isChecked;
                Properties.Settings.Default.ShowBasemap = isChecked;
            });

            ToggleGridOverlayDisplayCommand = new SimpleCommand<object, object>(delegate(Object args)
            {
                var isChecked = (bool)args;
                GridOverlayViewModel.IsChecked = isChecked;
                Properties.Settings.Default.ShowGrid = isChecked;
            });

            TogglePanZoomDisplayCommand = new SimpleCommand<object, object>(delegate(Object args)
            {
                var isChecked = (bool)args;
                _map.MapTools.PanZoomBar.Visibility = isChecked ? Visibility.Visible : Visibility.Hidden;
                Properties.Settings.Default.ShowPanZoom = isChecked;
            });

            AddShapefileCommand = new SimpleCommand<object, object>(delegate
            {
                Globals.OpenFileService.Filter = "ESRI Shapefiles (*.shp)|*.shp";
                var result = Globals.OpenFileService.ShowDialog(null);
                if (!result.HasValue || !result.Value) return;
                AddShapeFile(Globals.OpenFileService.FileName);
            });

            OpenEnvironmentFileCommand = new SimpleCommand<object, object>(delegate
            {
                Globals.OpenFileService.Filter = "ESME Environment File (*.eeb)|*.eeb";
                var result = Globals.OpenFileService.ShowDialog(null);
                if (!result.HasValue || !result.Value) return;
                AddEnvironmentFile(Globals.OpenFileService.FileName);
            });

            QuickLookCommand = new SimpleCommand<object, object>(delegate { return _environmentFileLoaded; }, delegate
                                                                                                              {
                                                                                                                  Cursor = Cursors.Cross;
                                                                                                                  IsQuickLookMode = true;
                                                                                                              });

            MouseMoveCommand = new SimpleCommand<object, object>(delegate(Object arg)
            {
                var args = (EventToCommandArgs)arg;
                var e = (MouseEventArgs)args.EventArgs;
                var point = e.MouseDevice.GetPosition(_map);
                var pointShape = ExtentHelper.ToWorldCoordinate(_map.CurrentExtent, (float)point.X, (float)point.Y, (float)_map.ActualWidth, (float)_map.ActualHeight);
                MouseLongitude = pointShape.X;
                MouseLatitude = pointShape.Y;
            });

            MouseLeftButtonUpCommand = new SimpleCommand<object, object>(delegate
            {
                if (!IsQuickLookMode) return;
                IsQuickLookMode = false;
                //Globals.MessageBoxService.ShowInformation(String.Format("Mouse click at ({0:0.000000}, {1:0.000000})", MouseLatitude, MouseLongitude));
                #region create transmission loss job. the new base class for all acoustic simulations!

                var transmissionLossJob = new TransmissionLossJob
                {
                    AcousticProperties = new AcousticProperties
                    {
                        SourceDepth = 10,
                        VerticalBeamWidth = 120,
                        DepressionElevationAngle =0f,
                        LowFrequency = 3500,
                        HighFrequency = 3500,
                    },
                    AnalysisPoint = new AnalysisPoint
                    {
                        IDField = 1,
                        Location = new EarthCoordinate(MouseLatitude, MouseLongitude),
                        RadialBearing = 0,
                        RadialCount = 16,

                    },
                    Radius = 10000,
                    MaxDepth = 3000,
                };

                #endregion

                #region create bellhop run file from tlj (and stuff)

                var environmentInformation = new EnvironmentInformation
                {
                    Bathymetry = new Bathymetry(_environmentFileName),
                    SoundSpeedField = new SoundSpeedField(_environmentFileName),
                    Sediment = SedimentTypes.SedimentArray[0],
                };

                var transmissionLossSettings = new TransmissionLossSettings
                {
                    DepthCellSize = 50,
                    RangeCellSize = 50,
                };

                var transmissionLossJobViewModel = new TransmissionLossJobViewModel
                                                   {
                                                       TransmissionLossJob = transmissionLossJob
                                                   };
                var result = Globals.UIVisualizerService.ShowDialog("TransmissionLossJobView", transmissionLossJobViewModel);
                if ((!result.HasValue) || (!result.Value))
                {
                    Cursor = Cursors.Arrow;
                    return;
                }

                Cursor = Cursors.Wait;
                Thread.Sleep(100);
                
                TransmissionLossField transmissionLossField;
                try
                {
                    var bellhopRunFile = BellhopRunFile.Create(transmissionLossJob, environmentInformation, transmissionLossSettings);
                    transmissionLossField = FieldCalculator.ComputeField(bellhopRunFile, null);
                }
                catch (BathymetryOutOfBoundsException)
                {
                    Globals.MessageBoxService.ShowError("Unable to run quick look.\nDid you click outside the bounds of the environment file?");
                    Cursor = Cursors.Arrow;
                    return;
                }
                catch (AggregateException ex)
                {
                    var sb = new StringBuilder();
                    foreach (var e in ex.InnerExceptions) sb.Append(e.Message + "\n");
                    Globals.MessageBoxService.ShowError("One or more errors occurred calculating transmission loss:\n" + sb);
                    Cursor = Cursors.Arrow;
                    return;
                }
                var transmissionLossViewModel = new TransmissionLossFieldViewModel(transmissionLossField);
                transmissionLossField.Filename = @".\test.tlf";
                transmissionLossField.Save();
                Globals.UIVisualizerService.Show("TransmissionLossView", transmissionLossViewModel, true, null);

                Cursor = Cursors.Arrow;
                #endregion
            });
            
            AddOverlayFileCommand = new SimpleCommand<object, object>(delegate
            {
                Globals.OpenFileService.Filter = "NUWC Overlay Files (*.ovr)|*.ovr";
                var result = Globals.OpenFileService.ShowDialog(null);
                if (!result.HasValue || !result.Value) return;
                AddOverlayFile(Globals.OpenFileService.FileName);
            });

            AddScenarioFileCommand = new SimpleCommand<object, object>(delegate { return CanAddScenarioFile();}, delegate
            {
                Globals.OpenFileService.Filter = "NUWC Scenario Files (*.nemo)|*.nemo";
                var result = Globals.OpenFileService.ShowDialog(null);
                if (!result.HasValue || !result.Value) return;
                AddScenarioFile(Globals.OpenFileService.FileName);
            });
        }

        public bool IsQuickLookMode { get; set; }

        #region public double MouseLatitude { get; set; }

        static readonly PropertyChangedEventArgs MouseLatitudeChangedEventArgs = ObservableHelper.CreateArgs<MapViewModel>(x => x.MouseLatitude);
        double _mouseLatitude;
        public double MouseLatitude
        {
            get { return _mouseLatitude; }
            set
            {
                if (_mouseLatitude == value) return;
                _mouseLatitude = value;
                NotifyPropertyChanged(MouseLatitudeChangedEventArgs);
            }
        }

        #endregion

        #region public Cursor Cursor { get; set; }

        static readonly PropertyChangedEventArgs CursorChangedEventArgs = ObservableHelper.CreateArgs<MapViewModel>(x => x.Cursor);
        Cursor _cursor;
        public Cursor Cursor
        {
            get { return _cursor; }
            set
            {
                if (_cursor == value) return;
                _cursor = value;
                NotifyPropertyChanged(CursorChangedEventArgs);
            }
        }

        #endregion

        #region public double MouseLongitude { get; set; }

        static readonly PropertyChangedEventArgs MouseLongitudeChangedEventArgs = ObservableHelper.CreateArgs<MapViewModel>(x => x.MouseLongitude);
        double _mouseLongitude;
        public double MouseLongitude
        {
            get { return _mouseLongitude; }
            set
            {
                if (_mouseLongitude == value) return;
                _mouseLongitude = value;
                NotifyPropertyChanged(MouseLongitudeChangedEventArgs);
            }
        }

        #endregion

        public LayerViewModel BaseMapViewModel { get; private set; }
        public LayerViewModel GridOverlayViewModel { get; private set; }

        public SimpleCommand<Object, Object> ToggleBaseMapDisplayCommand { get; private set; }
        public SimpleCommand<Object, Object> ToggleGridOverlayDisplayCommand { get; private set; }
        public SimpleCommand<Object, Object> TogglePanZoomDisplayCommand { get; private set; }

        public SimpleCommand<Object, Object> AddShapefileCommand { get; private set; }
        public SimpleCommand<Object, Object> AddOverlayFileCommand { get; private set; }
        public SimpleCommand<Object, Object> AddScenarioFileCommand { get; private set; }

        public SimpleCommand<Object, Object> OpenEnvironmentFileCommand { get; private set; }
        public SimpleCommand<Object, Object> QuickLookCommand { get; private set; }

        public SimpleCommand<Object, Object> MouseMoveCommand { get; private set; }
        public SimpleCommand<Object, Object> MouseLeftButtonUpCommand { get; private set; }

        private void ViewAwareStatusServiceViewLoaded()
        {
            if (Designer.IsInDesignMode)
                return;

            _map = ((MainWindow)Globals.ViewAwareStatus.View).Map1;
            MapDLLVersion = WpfMap.GetVersion();
            _map.MapUnit = GeographyUnit.DecimalDegree;
            _map.MapTools.PanZoomBar.HorizontalAlignment = HorizontalAlignment.Left;
            _map.MapTools.PanZoomBar.VerticalAlignment = VerticalAlignment.Top;
            _map.MapTools.Logo.IsEnabled = false;

            var appPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);

            BaseMapViewModel = new ShapefileLayerViewModel(Path.Combine(appPath, @"Sample GIS Data\Countries02.shp"))
            {
                IsChecked = Properties.Settings.Default.ShowBasemap
            };
            GridOverlayViewModel = new AdornmentLayerViewModel("Grid", new MyGraticuleAdornmentLayer())
            {
                IsChecked = Properties.Settings.Default.ShowGrid
            };

            _map.MapTools.PanZoomBar.Visibility = Properties.Settings.Default.ShowPanZoom ? Visibility.Visible : Visibility.Hidden;

            _map.CurrentExtent = new RectangleShape(new PointShape(-180, 90), new PointShape(180, -90));
            _map.ZoomToScale(_map.ZoomLevelScales[3]);

            if (Globals.IsInitializeExperimentNeeded)
            {
                if ((Globals.Experiment.ScenarioFileName != null) && (File.Exists(Globals.Experiment.ScenarioFileName)))
                   AddScenarioFile(Globals.Experiment.ScenarioFileName);
                else Globals.Experiment.ScenarioFileName = null;
            }
        }

        public void AddShapeFile(string filename)
        {
            var overlayLayer = new ShapefileLayerViewModel(filename);
            Globals.LayerDisplayViewModel.Layers.Add(overlayLayer);
        }
        
        public void AddOverlayFile(string filename)
        {
            try
            {
                var overlayLayer = new OverlayFileLayerViewModel(filename);
                Globals.LayerDisplayViewModel.Layers.Add(overlayLayer);
            }
            catch (Exception e)
            {
                Globals.MessageBoxService.ShowError(string.Format("Error opening overlay file {0}:\n{1}",
                                                           filename, e.Message));
            }
        }

        public bool CanAddScenarioFile()
        {
            return ((Globals.AppSettings.ScenarioDataDirectory != null) && (Directory.Exists(Globals.AppSettings.ScenarioDataDirectory)));
        }

        public void AddScenarioFile(string filename)
        {
            if (!CanAddScenarioFile()) return;
            if ((Globals.Experiment.ScenarioFileName != null) && (filename != Globals.Experiment.ScenarioFileName))
            {
                if (File.Exists(Globals.Experiment.ScenarioFileName))
                {
                    var result = Globals.MessageBoxService.ShowYesNo("This experiment already has a scenario file.  Replace it with this one?", CustomDialogIcons.Exclamation);
                    if (result == CustomDialogResults.No) return;
                    Globals.LayerDisplayViewModel.RemoveScenarioLayers();
                }
            }
            try
            {
                var overlayLayer = new ScenarioFileLayerViewModel(filename, Globals.AppSettings.ScenarioDataDirectory);
                Globals.LayerDisplayViewModel.Layers.Add(overlayLayer);
                Globals.Experiment.ScenarioFileName = filename;
            }
            catch (Exception e)
            {
                Globals.MessageBoxService.ShowError(string.Format("Error opening scenario file {0}:\n{1}",
                                                           filename, e.Message));
            }
        }

        public void AddEnvironmentFile(string filename)
        {
            _environmentFileName = filename;
            Bathymetry bathymetry;
            try
            {
                bathymetry = new Bathymetry(_environmentFileName);
            }
            catch (Exception e)
            {
                Globals.MessageBoxService.ShowError(string.Format("Error opening bathymetry file {0}:\n{1}",
                                                           filename, e.Message));
                return;
            }
            var overlayLayer = new OverlayShapesLayerViewModel(null, Path.GetFileNameWithoutExtension(_environmentFileName) + " bathymetry bounds")
            {
                Overlay = new LayerOverlay(),
            };

            Overlays.Add(overlayLayer.Overlay);
            overlayLayer.OverlayShapes.Add(bathymetry.BoundingBox);
            overlayLayer.CommitShapes();

            _environmentFileLoaded = true;

            Globals.LayerDisplayViewModel.Layers.Add(overlayLayer);
        }

        public GeoCollection<Overlay> Overlays { get { return _map.Overlays; } }
        public AdornmentOverlay AdornmentOverlay { get { return _map.AdornmentOverlay; } }
        public void Refresh() {_map.Refresh();}
    }
}