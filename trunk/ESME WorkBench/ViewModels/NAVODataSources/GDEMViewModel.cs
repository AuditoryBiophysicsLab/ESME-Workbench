using Cinch;
using ESME.Environment.NAVO;
using ESMEWorkBench.Properties;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.NAVODataSources
{
    [ExportViewModel("GDEMViewModel")]
    internal class GDEMViewModel : NAVODataSourceViewModel
    {
        readonly GDEM _gdem = new GDEM();
        public GDEMViewModel() { _gdem.GridSpacing = 0.25f; }

        [MediatorMessageSink(MediatorMessage.EnvironmentBuilderDatabasesSpecified)]
        public void SetDatabasePaths()
        {
            _gdem.DatabasePath = Settings.Default.GDEMDirectory;
            _gdem.ExtractionProgramPath = Settings.Default.GDEMEXEDirectory;
        }


        [MediatorMessageSink(MediatorMessage.ExtractGDEM)]
        void ExtractData(NAVOExtractionPacket packet)
        {
            _gdem.ExtractArea(packet);
            ExtractedArea = _gdem.ExtractedArea;
            MediatorMessage.Send(MediatorMessage.GDEMExtracted);
        }
    }
}