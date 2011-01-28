using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Cinch;
using ESME.Environment.NAVO;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.NAVODataSources
{
    [ExportViewModel("SMGCViewModel")]
    class SMGCViewModel: NAVODataSourceViewModel
    {
        SMGC _smgc = new SMGC();

        public SMGCViewModel()
        {
            SetDatabasePaths(_smgc);
            _smgc.GridSpacing = 1;
            
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
