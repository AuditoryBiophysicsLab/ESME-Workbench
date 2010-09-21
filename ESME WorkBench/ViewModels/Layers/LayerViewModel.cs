using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using Cinch;
using ESMEWorkBench.ViewModels.Main;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Layers
{
    public abstract class LayerViewModel : ViewModelBase
    {
        LayerViewModel _parent;

        #region public string LayerName { get; set; }

        static readonly PropertyChangedEventArgs NameChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.LayerName);
        string _layerName;

        public string LayerName
        {
            get { return _layerName; }
            set
            {
                if (_layerName == value) return;
                _layerName = value;
                NotifyPropertyChanged(NameChangedEventArgs);
            }
        }

        #endregion

        #region public string FileName { get; set; }

        static readonly PropertyChangedEventArgs FileNameChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.FileName);
        string _fileName;

        public string FileName
        {
            get { return _fileName; }
            set
            {
                if (_fileName == value) return;
                _fileName = value;
                NotifyPropertyChanged(FileNameChangedEventArgs);
            }
        }

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
                //Console.WriteLine("{0} IsSelected={1}", LayerName, _isSelected);
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
        List<MenuItemViewModel> _contextMenu;

        public List<MenuItemViewModel> ContextMenu
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

        #region public bool? IsChecked { get; set; }

        static readonly PropertyChangedEventArgs IsCheckedChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.IsChecked);
        bool? _isChecked = false;

        public bool? IsChecked
        {
            get { return _isChecked; }
            set { SetIsChecked(value, true, true); }
        }

        void SetIsChecked(bool? value, bool updateChildren, bool updateParent)
        {
            if (value == _isChecked) return;

            _isChecked = value;

            if (updateChildren && _isChecked.HasValue) foreach (LayerViewModel child in Children) child.SetIsChecked(_isChecked, true, false);

            if (updateParent && _parent != null) _parent.VerifyCheckState();

            if (Overlay != null)
            {
                if ((_isChecked == null) || (!_isChecked.Value)) Overlay.IsVisible = false;
                else if (_isChecked.Value) Overlay.IsVisible = true;
            }

            NotifyPropertyChanged(IsCheckedChangedEventArgs);
        }

        void VerifyCheckState()
        {
            bool? state = null;
            for (int i = 0; i < Children.Count; ++i)
            {
                bool? current = Children[i].IsChecked;
                if (i == 0) state = current;
                else if (state != current)
                {
                    state = null;
                    break;
                }
            }
            SetIsChecked(state, false, true);
        }

        #endregion

        #region public LayersCollection Children { get; set; }

        static readonly PropertyChangedEventArgs ChildrenChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.Children);
        LayersCollection _children;

        public LayersCollection Children
        {
            get { return _children; }
            set
            {
                if (_children == value) return;
                _children = value;
                _children.CollectionChanged += ChildrenCollectionChanged;
                NotifyPropertyChanged(ChildrenChangedEventArgs);
            }
        }

        void ChildrenCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(ChildrenChangedEventArgs); }

        #endregion

        protected LayerViewModel(string name, string fileName, MapViewModel mapViewModel)
        {
            LayerName = name;
            FileName = fileName;
            MapViewModel = mapViewModel;
            Children = new LayersCollection();
            Children.CollectionChanged += Children_CollectionChanged;
            IsChecked = true;
            ShowContextMenu = true;
            ContextMenu = new List<MenuItemViewModel>
                          {
                              new MenuItemViewModel
                              {
                                  Header = "Order",
                                  Children = new List<MenuItemViewModel>
                                             {
                                                  new MenuItemViewModel
                                                  {
                                                      Header = "Bring to front",
                                                      Command = MapViewModel.LayerDisplayViewModel.MoveLayerToFrontCommand,
                                                      CommandParameter = this,
                                                  },
                                                  new MenuItemViewModel
                                                  {
                                                      Header = "Bring forward",
                                                      Command = MapViewModel.LayerDisplayViewModel.MoveLayerForwardCommand,
                                                      CommandParameter = this,
                                                  },
                                                  new MenuItemViewModel
                                                  {
                                                      Header = "Push backward",
                                                      Command = MapViewModel.LayerDisplayViewModel.MoveLayerBackCommand,
                                                      CommandParameter = this,
                                                  },
                                                  new MenuItemViewModel
                                                  {
                                                      Header = "Push to back",
                                                      Command = MapViewModel.LayerDisplayViewModel.MoveLayerToBackCommand,
                                                      CommandParameter = this,
                                                  },
                                             },
                              },
                              new MenuItemViewModel
                              {
                                  Header = "Remove",
                                  Command = new SimpleCommand<object, object>(obj => Remove()),
                              },
                          };
        }

        public MapViewModel MapViewModel { get; private set; }

        public Overlay Overlay { get; set; }

        void Children_CollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            if (e.OldItems != null)
                foreach (var oldLayer in e.OldItems.Cast<LayerViewModel>())
                {
                    oldLayer.Remove();
                }
        }

        void Initialize()
        {
            foreach (var child in Children)
            {
                child._parent = this;
                child.Initialize();
            }
        }

        public void Remove()
        {
            MapViewModel.Overlays.Remove(Overlay);
            MapViewModel.Refresh();
            MapViewModel.LayerDisplayViewModel.Layers.Remove(this);
        }
    }

    public abstract class LayerViewModel<T> : LayerViewModel
        where T : Layer
    {
        protected LayerViewModel(string name, string fileName, MapViewModel mapViewModel) : base(name, fileName, mapViewModel) { }

        #region public T LayerData { get; set; }

        static readonly PropertyChangedEventArgs LayerChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel<T>>(x => x.LayerData);
        T _layerData;

        public T LayerData
        {
            get { return _layerData; }
            set
            {
                if (_layerData == value) return;
                _layerData = value;
                NotifyPropertyChanged(LayerChangedEventArgs);
            }
        }

        #endregion
    }
}