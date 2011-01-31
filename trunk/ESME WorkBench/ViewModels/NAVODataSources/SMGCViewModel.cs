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

        public SMGCViewModel() { _smgc.GridSpacing = 1; }

        [MediatorMessageSink(MediatorMessage.EnvironmentBuilderDatabasesSpecified)]
        public void SetDatabasePaths()
        {
            _smgc.DatabasePath = Settings.Default.SMGCDirectory;
            _smgc.ExtractionProgramPath = Settings.Default.SMGCEXEDirectory;
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