using System;
using System.Diagnostics;
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
        public GDEMViewModel()
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nGDEMViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            } 
            _gdem.GridSpacing = 0.25f;
        }


        [MediatorMessageSink(MediatorMessage.EnvironmentBuilderDatabasesSpecified)]
        public void SetDatabasePaths()
        {
            _gdem.DatabasePath = Globals.AppSettings.NAVOConfiguration.GDEMDirectory;
            _gdem.ExtractionProgramPath = Globals.AppSettings.NAVOConfiguration.GDEMEXEPath;
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