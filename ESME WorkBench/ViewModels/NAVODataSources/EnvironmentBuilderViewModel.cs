﻿using System.IO;
using Cinch;
using ESME.Environment.NAVO;
using ESMEWorkBench.Data;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.NAVODataSources
{
    [ExportViewModel("EnvironmentBuilderViewModel")]
    public class EnvironmentBuilderViewModel
    {
        public Experiment Experiment { get; set; }

        #region LaunchEnvironmentConfigurationViewCommand

        SimpleCommand<object, object> _launchEnvironmentConfigurationView;

        public SimpleCommand<object, object> LaunchEnvironmentConfigurationViewCommand
        {
            get { return _launchEnvironmentConfigurationView ?? (_launchEnvironmentConfigurationView = new SimpleCommand<object, object>(delegate { Mediator.Instance.NotifyColleagues("LaunchEnvironmentConfigurationViewCommandMessage"); })); }
        }

        #endregion

        #region ExtractAllCommand

        SimpleCommand<object, object> _extractAll;

        public SimpleCommand<object, object> ExtractAllCommand
        {
            get { return _extractAll ?? (_extractAll = new SimpleCommand<object, object>(delegate
                                                                                         {
                                                                                             //(if the 4 seasons are defined or if the 2 seasons are defined)
                                                                                             //and if BST and DBDB have resolutions selected
                                                                                             //and if the configuration view exited successfully and all paths were set
                                                                                             //then this button should be usable.
                                                                                             
                                                                                             return true;
                                                                                         }, 
                                                                                         delegate
                                                                                         {
                                                                                             //set the times
                                                                                             SetTimes(); 
                                                                                             //extract data from all data sources.
                                                                                             ExtractArea();
                                                                                         })); }
        }

        #endregion

        void SetTimes() { MediatorMessage.Send(MediatorMessage.SeasonsDefined); }

        //new mediator message sink for actually extracitng area -- will wind up also getting called when some idiot changes the boundaries.
        void ExtractArea()
        {
            var area = new NAVOExtractionPacket
                       {
                           Filename = Path.Combine(Experiment.LocalStorageRoot, "temp.xml"),
                           North = Experiment.Bathymetry.BoundingBox.North,
                           South = Experiment.Bathymetry.BoundingBox.South,
                           East = Experiment.Bathymetry.BoundingBox.East,
                           West = Experiment.Bathymetry.BoundingBox.West,
                       };
            MediatorMessage.Send(MediatorMessage.ExtractBST, area);
            MediatorMessage.Send(MediatorMessage.ExtractDBDB, area);
            MediatorMessage.Send(MediatorMessage.ExtractGDEM, area);
            MediatorMessage.Send(MediatorMessage.ExtractSMGC, area);
        }
    }
}