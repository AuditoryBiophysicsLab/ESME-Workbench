using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using Cinch;
using ESMEWorkBench.ViewModels.Main;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMEWorkBench.ViewModels.Layers
{
    public class LayerViewModel : ViewModelBase
    {
        public LayerViewModel(string name, Overlay overlay) : this(name) { Overlay = overlay; }

        public LayerViewModel(string name) : this() { LayerName = name; }

        public LayerViewModel()
        {
            Children = new LayersCollection();
            IsChecked = true;
            ContextMenu = new List<MenuItemViewModel>();
            CanBeReordered = true;
            CanBeRemoved = true;
            ShowContextMenu = true;
            TileType = TileType.SingleTile;
        }

        #region public bool CanBeReordered { get; set; }

        public bool CanBeReordered
        {
            get { return _canBeReordered; }
            set
            {
                if (_canBeReordered == value) return;
                _canBeReordered = value;
                if (CanBeReordered)
                    ContextMenu.Add(new MenuItemViewModel
                    {
                        Header = "Order",
                        Children = new List<MenuItemViewModel>(),
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
                    ContextMenu.Add(new MenuItemViewModel
                    {
                        Header = "Remove",
                        Command = new SimpleCommand<object, object>(delegate
                        {
                            Remove();
                            Mediator.Instance.NotifyColleagues("RefreshMapMessage");
                        }),
                    });
                else ContextMenu.Remove(ContextMenu.Find(x => x.Header == "Remove"));
                NotifyPropertyChanged(CanBeRemovedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CanBeRemovedChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.CanBeRemoved);
        bool _canBeRemoved;

        #endregion

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

            if (updateChildren && _isChecked.HasValue) foreach (var child in Children) child.SetIsChecked(_isChecked, true, false);

            if (updateParent && Parent != null) Parent.VerifyCheckState();

            try
            {
                if (Overlay != null) Overlay.IsVisible = !_isChecked.HasValue || _isChecked.Value;
            }
            catch (NullReferenceException e)
            {
                System.Diagnostics.Debug.WriteLine(e.Message);
            }

            NotifyPropertyChanged(IsCheckedChangedEventArgs);
        }

        void VerifyCheckState()
        {
            bool? state = null;
            for (var i = 0; i < Children.Count; ++i)
            {
                var current = Children[i].IsChecked;
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

        void ChildrenCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Remove:
                    foreach (var oldItem in e.OldItems)
                    {
                        
                    }
                    break;
            }
            NotifyPropertyChanged(ChildrenChangedEventArgs);
        }

        #endregion

        #region public LayerTreeViewModel LayerTreeViewModel { get; set; }

        public LayerTreeViewModel LayerTreeTreeViewModel
        {
            get { return _layerTreeViewModel; }
            set
            {
                if (_layerTreeViewModel == value) return;
                _layerTreeViewModel = value;
                var orderMenu = ContextMenu.First(x => x.Header == "Order");
                orderMenu.Children.Add(new MenuItemViewModel
                {
                    Header = "Bring to front",
                    Command = _layerTreeViewModel.MoveLayerToFrontCommand,
                    CommandParameter = this,
                });
                orderMenu.Children.Add(new MenuItemViewModel
                {
                    Header = "Bring forward",
                    Command = _layerTreeViewModel.MoveLayerForwardCommand,
                    CommandParameter = this,
                });
                orderMenu.Children.Add(new MenuItemViewModel
                {
                    Header = "Push backward",
                    Command = _layerTreeViewModel.MoveLayerBackCommand,
                    CommandParameter = this,
                });
                orderMenu.Children.Add(new MenuItemViewModel
                {
                    Header = "Push to back",
                    Command = _layerTreeViewModel.MoveLayerToBackCommand,
                    CommandParameter = this,
                });
                NotifyPropertyChanged(LayerTreeViewModelChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LayerTreeViewModelChangedEventArgs = ObservableHelper.CreateArgs<LayerViewModel>(x => x.LayerTreeTreeViewModel);
        LayerTreeViewModel _layerTreeViewModel;

        #endregion

        public LayerViewModel Parent { get; set; }

        public TileType TileType { get; set; }

        public Overlay Overlay { get; set; }
        
        void Initialize()
        {
            foreach (var child in Children)
            {
                child.Parent = this;
                child.Initialize();
            }
        }

        public void Remove()
        {
            Mediator.Instance.NotifyColleagues("RemoveOverlayFromMapMessage", Overlay);
            //Globals.LayerDisplayViewModel.Layers.Remove(this);
        }
    }
}