using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.IO;
using System.Linq;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Locations;
using ESME.Plugins;
using ESME.Views.Locations;
using HRC.Navigation;
using HRC.Utility;
using Microsoft.Win32;
using NAVODatabaseAdapter;

namespace InstallableNAVOPlugin
{
    [PartCreationPolicy(CreationPolicy.Shared)]
    [EnvironmentDataSource(EnvironmentDataType = EnvironmentDataType.Sediment,
                           Name = "BST 2.0 for ESME Workbench",
                           Description = "Bottom Sediments Type Database Version 2.0 Repacked from US Navy/NAVOCEANO, packaged for ESME Workbench")]
    public sealed class BST20ForESME : EnvironmentalDataSourcePluginBase<Sediment>
    {
        const string RequiredBSTFilename = "hfevav2.h5";

        public BST20ForESME()
        {
            SetPropertiesFromAttributes(GetType());
            AvailableResolutions = new[] { 5f, 0.1f };
            IsTimeVariantData = false;
            AvailableTimePeriods = new[] { TimePeriod.Invalid };

            var regKey = Registry.LocalMachine.OpenSubKey(@"Software\Boston University\ESME Workbench\Data Sources\BST 2.0");
            if (regKey != null) _dataDirectory = (string)regKey.GetValue("");

            IsSelectable = _dataDirectory != null;
            IsConfigured = _dataDirectory != null &&
                           Directory.Exists(_dataDirectory) &&
                           File.Exists(Path.Combine(_dataDirectory, RequiredBSTFilename));
            SelectionControlViewModel = new MultipleSelectionsViewModel<float>
            {
                UnitName = " min",
                AvailableSelections = AvailableResolutions,
            };
            SelectionControl = new MultipleSelectionsView { DataContext = SelectionControlViewModel };
        }

        readonly string _dataDirectory;

        public override Sediment Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, PercentProgress progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            return BST.Extract(Path.Combine(_dataDirectory, RequiredBSTFilename), geoRect, resolution, progress);
        }

        public override IEnumerable<EnvironmentalDataSet> SelectedDataSets
        {
            get
            {
                return from simpleSelection in ((MultipleSelectionsViewModel<float>)SelectionControlViewModel).SimpleSelectionViewModels where simpleSelection.IsSelected select new EnvironmentalDataSet
                {
                    SourcePlugin = PluginIdentifier,
                    Resolution = simpleSelection.Value,
                    TimePeriod = TimePeriod.Invalid,
                };
            }
        }
    }
}