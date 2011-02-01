using System;
using System.Diagnostics;
using Cinch;
using ESME.Environment.NAVO;
using ESMEWorkBench.Properties;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.NAVODataSources
{
    [ExportViewModel("SMGCViewModel")]
    internal class SMGCViewModel : NAVODataSourceViewModel
    {
        readonly SMGC _smgc = new SMGC();

        public SMGCViewModel()
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nSMGCfViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
            _smgc.GridSpacing = 1;
        }

        [MediatorMessageSink(MediatorMessage.EnvironmentBuilderDatabasesSpecified)]
        public void SetDatabasePaths()
        {
            _smgc.DatabasePath = Globals.AppSettings.NAVOConfiguration.SMGCDirectory;
            _smgc.ExtractionProgramPath = Globals.AppSettings.NAVOConfiguration.SMGCEXEPath;
        }

        [MediatorMessageSink(MediatorMessage.ExtractSMGC)]
        void ExtractData(NAVOExtractionPacket packet)
        {
            _smgc.ExtractArea(packet);
            ExtractedArea = _smgc.ExtractedArea;
            MediatorMessage.Send(MediatorMessage.SMGCExtracted);
        }
    }
}