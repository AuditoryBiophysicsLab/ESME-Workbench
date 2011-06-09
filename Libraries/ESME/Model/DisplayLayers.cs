using System.Collections.Generic;
using System.ComponentModel;
using HRC.Utility;

namespace ESME.Model
{
    public class DisplayLayer : PropertyChangedBase
    {
        #region Properties
        public List<DisplayLayer> Children { get; private set; }
        public bool IsInitiallySelected { get; private set; }
        public string Name { get; private set; }
        public bool IsSelected { get; set; }
        public bool IsExpanded { get; set; }

        #region IsChecked

        /// <summary>
        /// Gets/sets the state of the associated UI toggle (ex. CheckBox).
        /// The return value is calculated based on the check state of all
        /// child FooViewModels.  Setting this property to true or false
        /// will set all children to the same check state, and setting it 
        /// to any value will cause the parent to verify its check state.
        /// </summary>
        public bool? IsChecked
        {
            get { return _isChecked; }
            set { SetIsChecked(value, true, true); }
        }

        void SetIsChecked(bool? value, bool updateChildren, bool updateParent)
        {
            if (value == _isChecked)
                return;

            _isChecked = value;

            if (updateChildren && _isChecked.HasValue)
                Children.ForEach(c => c.SetIsChecked(_isChecked, true, false));

            if (updateParent && _parent != null)
                _parent.VerifyCheckState();

            NotifyPropertyChanged(new PropertyChangedEventArgs("IsChecked"));
        }

        void VerifyCheckState()
        {
            bool? state = null;
            for (int i = 0; i < Children.Count; ++i)
            {
                bool? current = Children[i].IsChecked;
                if (i == 0)
                {
                    state = current;
                }
                else if (state != current)
                {
                    state = null;
                    break;
                }
            }
            SetIsChecked(state, false, true);
        }

        #endregion // IsChecked

        #endregion // Properties

        DisplayLayer(string name)
        {
            Name = name;
            Children = new List<DisplayLayer>();
        }

        void Initialize()
        {
            foreach (DisplayLayer child in Children)
            {
                child._parent = this;
                child.Initialize();
            }
        }

        private static int _count;

        public static List<DisplayLayer> CreateDisplayLayers()
        {
            var root = new DisplayLayer("Display Layers " + _count++)
            {
                IsInitiallySelected = true,
                IsExpanded = true,
                Children =
                {
                    new DisplayLayer("Simulation Scenario")
                    {
                        IsSelected = true,
                        IsExpanded = true,
                        IsChecked = true,
                        Children = 
                        {
                            new DisplayLayer("Analysis Points"),
                            new DisplayLayer("Nemo Scenario File")
                            {
                                Children = 
                                {
                                    new DisplayLayer("Operational Area(s)")
                                    {
                                        Children =
                                        {
                                            new DisplayLayer("OpArea 1")
                                            {
                                                IsChecked = true,
                                            },
                                            new DisplayLayer("OpArea 2"),
                                        },
                                    },
                                    new DisplayLayer("Platform(s)")
                                    {
                                        Children =
                                        {
                                            new DisplayLayer("Platform 1"),
                                            new DisplayLayer("Platform 2"),
                                        }
                                    },
                                },
                            },
                        },
                    },
                    new DisplayLayer("Animals")
                    {
                        Children =
                        {
                            new DisplayLayer("generic_mysticete"),
                            new DisplayLayer("generic_odontocete"),
                        },
                    },
                    new DisplayLayer("Sound Speed Field"),
                    new DisplayLayer("Bathymetry"),
                    new DisplayLayer("Sediment"),
                },
            };
            root.Initialize();
            return new List<DisplayLayer> { root };
        }

        #region Data
        bool? _isChecked = false;
        DisplayLayer _parent;
        #endregion

    }
}
