using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Cinch;
using ESME.Environment.NAVO;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.NAVODataSources
{
    [ExportViewModel("GDEMViewModel")]
    class GDEMViewModel : NAVODataSourceViewModel
    {
        GDEM _gdem = new GDEM();
        public GDEMViewModel()
        {
            SetDatabasePaths(_gdem);
            _gdem.GridSpacing = 0.25f;
            
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
