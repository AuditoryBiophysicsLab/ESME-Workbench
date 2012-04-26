using System;
using HRC.ViewModels;

namespace ESMEWorkbench.ViewModels.Tree
{
    public class WorldMapFeatureNode : ViewModelBase
    {
        public WorldMapFeatureNode(string featureName, Func<bool> getter, Action<bool> setter)
        {
            FeatureName = featureName;
            _getter = getter;
            _setter = setter;
        }

        readonly Func<bool> _getter;
        readonly Action<bool> _setter;
        public bool IsChecked
        {
            get { return _getter(); }
            set { _setter(value); }
        }

        public string FeatureName { get; private set; }
    }
}