using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Windows.Forms;
using System.Windows.Media;
using Cinch;
using ESMEWorkBench.ViewModels.Main;
using ESMEWorkBench.ViewModels.Map;

namespace ESMEWorkBench.ViewModels.Layers
{
    public class LayerViewModel : ViewModelBase
    {
        public LayerViewModel(MapLayer mapLayer)
        {
            MapLayer = mapLayer;
            LayerType = MapLayer.LayerType;
            ContextMenu = new List<MenuItemViewModel<LayerViewModel>>();
            CanChangeLineColor = true;
            CanBeRemoved = true;
            Name = MapLayer.Name;
            IsChecked = true;
            CanBeReordered = true;
            ShowContextMenu = true;

            switch (LayerType)
            {
                case LayerType.Scenario:
                    CanBeRemoved = false;
                    break;
                case LayerType.Track:
                    CanBeRemoved = false;
                    CanChangeLineColor = false;
                    break;
                case LayerType.Shapefile:
                    CanChangeAreaColor = true;
                    break;
            }
        }

        #region public string Name { get; set; }

        static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.Name);
        string _name;

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

        #endregion

        #region public bool CanBeReordered { get; set; }

        public bool CanBeReordered
        {
            get { return _canBeReordered; }
            set
            {
                if (_canBeReordered == value) return;
                _canBeReordered = value;
                if (CanBeReordered)
                    ContextMenu.Add(new MenuItemViewModel<LayerViewModel>
                    {
                        Header = "Order",
                        Children = new List<MenuItemViewModel<LayerViewModel>>(),
                    });
                else ContextMenu.Remove(ContextMenu.Find(x => x.Header == "Order"));
                NotifyPropertyChanged(CanBeReorderedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CanBeReorderedChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.CanBeReordered);
        bool _canBeReordered;

        #endregion

        #region public bool CanBeRemoved { get; set; }

        public bool CanBeRemoved
        {
            get { return _canBeRemoved; }
            set
            {
                if (_canBeRemoved == value) return;
                _canBeRemoved = value;
                if (CanBeRemoved)
                    ContextMenu.Add(new MenuItemViewModel<LayerViewModel>
                    {
                        Header = "Remove",
                        Command = new SimpleCommand<LayerViewModel, LayerViewModel>(delegate
                        {
                            MediatorMessage.Send(MediatorMessage.RemoveLayer, MapLayer);
                        }),
                    });
                else ContextMenu.Remove(ContextMenu.Find(x => x.Header == "Remove"));
                NotifyPropertyChanged(CanBeRemovedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CanBeRemovedChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.CanBeRemoved);
        bool _canBeRemoved;

        #endregion

        #region public bool CanChangeLineColor { get; set; }

        public bool CanChangeLineColor
        {
            get { return _canChangeLineColor; }
            set
            {
                if (_canChangeLineColor == value) return;
                _canChangeLineColor = value;
                if (CanChangeLineColor)
                    ContextMenu.Add(new MenuItemViewModel<LayerViewModel>
                    {
                        Header = "Line Color",
                        Command = new SimpleCommand<LayerViewModel, LayerViewModel>(delegate
                        {
                            var colorDialog = new ColorDialog
                            {
                                AllowFullOpen = true,
                                AnyColor = true
                            };
                            var result = colorDialog.ShowDialog();
                            if (result == DialogResult.OK)
                            {
                                MapLayer.LineColor = Color.FromArgb(colorDialog.Color.A, colorDialog.Color.R, colorDialog.Color.G, colorDialog.Color.B);
                                MediatorMessage.Send(MediatorMessage.SetLayerLineColor, MapLayer);
                            }
                        }),
                    });
                else ContextMenu.Remove(ContextMenu.Find(x => x.Header == "Line Color"));
                NotifyPropertyChanged(CanBeRemovedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CanChangeLineColorChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.CanChangeLineColor);
        bool _canChangeLineColor;

        #endregion

        #region public bool CanChangeAreaColor { get; set; }

        public bool CanChangeAreaColor
        {
            get { return _canChangeAreaColor; }
            set
            {
                if (_canChangeAreaColor == value) return;
                _canChangeAreaColor = value;
                if (CanChangeAreaColor)
                    ContextMenu.Add(new MenuItemViewModel<LayerViewModel>
                    {
                        Header = "Area Color",
                        Command = new SimpleCommand<LayerViewModel, LayerViewModel>(delegate
                        {
                            var colorDialog = new ColorDialog
                            {
                                AllowFullOpen = true,
                                AnyColor = true
                            };
                            var result = colorDialog.ShowDialog();
                            if (result == DialogResult.OK)
                            {
                                MapLayer.AreaColor = Color.FromArgb(colorDialog.Color.A, colorDialog.Color.R, colorDialog.Color.G, colorDialog.Color.B);
                                MediatorMessage.Send(MediatorMessage.SetLayerAreaColor, MapLayer);
                            }
                        }),
                    });
                else ContextMenu.Remove(ContextMenu.Find(x => x.Header == "Area Color"));
                NotifyPropertyChanged(CanChangeAreaColorChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CanChangeAreaColorChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.CanChangeAreaColor);
        bool _canChangeAreaColor;

        #endregion

        #region public bool IsSelected { get; set; }

        static readonly PropertyChangedEventArgs IsSelectedChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.IsSelected);
        bool _isSelected;

        public bool IsSelected
        {
            get { return _isSelected; }
            set
            {
                if (_isSelected == value) return;
                _isSelected = value;
                //Console.WriteLine("{0} IsSelected={1}", Name, _isSelected);
                NotifyPropertyChanged(IsSelectedChangedEventArgs);
            }
        }

        #endregion

        #region public bool ShowContextMenu { get; set; }

        static readonly PropertyChangedEventArgs ShowContextMenuEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.ShowContextMenu);
        bool _showContextMenu;

        public bool ShowContextMenu
        {
            get { return _showContextMenu; }
            set
            {
                if (_showContextMenu == value) return;
                _showContextMenu = value;
                NotifyPropertyChanged(ShowContextMenuEventArgs);
            }
        }

        #endregion

        #region public List<MenuItemViewModel> ContextMenu { get; set; }

        static readonly PropertyChangedEventArgs ContextMenuChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.ContextMenu);
        List<MenuItemViewModel<LayerViewModel>> _contextMenu;

        public List<MenuItemViewModel<LayerViewModel>> ContextMenu
        {
            get { return _contextMenu; }
            set
            {
                if (_contextMenu == value) return;
                _contextMenu = value;
                NotifyPropertyChanged(ContextMenuChangedEventArgs);
            }
        }

        #endregion

        #region public bool IsChecked { get; set; }

        static readonly PropertyChangedEventArgs IsCheckedChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.IsChecked);
        bool _isChecked;

        public bool IsChecked
        {
            get { return _isChecked; }
            set
            {
                if (value == _isChecked) return;

                _isChecked = value;

                try
                {
                    MapLayer.IsVisible = _isChecked;
                }
                catch (NullReferenceException e)
                {
                    System.Diagnostics.Debug.WriteLine("Test!!!: " + e.Message);
                }

                NotifyPropertyChanged(IsCheckedChangedEventArgs);
            }
        }

        public MapLayer MapLayer { get; set; }

        public LayerType LayerType { get; set; }

        #endregion

        #region public LayerListViewModel LayerListViewModel { get; set; }

        public LayerListViewModel LayerListListViewModel
        {
            get { return _layerListViewModel; }
            set
            {
                if (_layerListViewModel == value) return;
                _layerListViewModel = value;
                var orderMenu = ContextMenu.First(x => x.Header == "Order");
                orderMenu.Children.Add(new MenuItemViewModel<LayerViewModel>
                {
                    Header = "Move to top",
                    Command = _layerListViewModel.MoveLayerToTopCommand,
                    CommandParameter = this,
                });
                orderMenu.Children.Add(new MenuItemViewModel<LayerViewModel>
                {
                    Header = "Move up",
                    Command = _layerListViewModel.MoveLayerUpCommand,
                    CommandParameter = this,
                });
                orderMenu.Children.Add(new MenuItemViewModel<LayerViewModel>
                {
                    Header = "Move down",
                    Command = _layerListViewModel.MoveLayerDownCommand,
                    CommandParameter = this,
                });
                orderMenu.Children.Add(new MenuItemViewModel<LayerViewModel>
                {
                    Header = "Move to bottom",
                    Command = _layerListViewModel.MoveLayerToBottomCommand,
                    CommandParameter = this,
                });
                NotifyPropertyChanged(LayerTreeViewModelChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LayerTreeViewModelChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.LayerListListViewModel);
        LayerListViewModel _layerListViewModel;

        #endregion
    }

    public enum LayerType
    {
        Shapefile,
        OverlayFile,
        Scenario,
        Track,
        WindSpeed,
        SoundSpeed,
        BottomType,
        Bathymetry,
    }
}