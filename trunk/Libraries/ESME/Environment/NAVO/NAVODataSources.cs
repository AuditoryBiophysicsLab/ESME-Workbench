using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using Cinch;

namespace ESME.Environment.NAVO
{
    public class NAVODataSources:ViewModelBase
    {
        public BST BST { get; private set; }
        public DBDB DBDB { get; private set; }
        public GDEM GDEM { get; private set; }
        public SMGC SMGC { get; private set; }
        internal NAVOExtractionPacket ExtractionPacket { get; set; }

        public NAVODataSources( NAVOConfiguration configurations, NAVOExtractionPacket extractionPacket)
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nNAVODataSources: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }

            ExtractionPacket = extractionPacket;
            BST = new BST()
                  {
                      DatabasePath = configurations.BSTDirectory,
                      ExtractionProgramPath = configurations.BSTEXEPath,
                      //TimePeriod = extractionPacket.TimePeriod,
                  };
            DBDB = new DBDB()
            {
                DatabasePath = configurations.DBDBDirectory,
                ExtractionProgramPath = configurations.DBDBEXEPath,
                //TimePeriod = extractionPacket.TimePeriod,
            };
            GDEM = new GDEM()
            {
                DatabasePath = configurations.GDEMDirectory,
                ExtractionProgramPath = @"C:\Projects\ESME Deliverables\trunk\Utilities\NetCDFExtractor\bin\x86\Debug\ImportNetCDF.exe", //todo
               // TimePeriod = extractionPacket.TimePeriod,
            };
            SMGC = new SMGC()
            {
                DatabasePath = configurations.SMGCDirectory,
                ExtractionProgramPath = configurations.SMGCEXEPath,
               // TimePeriod = extractionPacket.TimePeriod,
            };

            BST.GetAllResolutions();
            DBDB.GetAllResolutions();

        }


        #region ExtractAreasCommand

        public SimpleCommand<object, object> ExtractAreasCommand
        {
            get { return _extractAreas ?? (_extractAreas = new SimpleCommand<object, object>(delegate
                                                                                             {
                                                                                                 BST.ExtractArea(ExtractionPacket);
                                                                                                 DBDB.ExtractArea(ExtractionPacket);
                                                                                                 GDEM.ExtractArea(ExtractionPacket);
                                                                                                 SMGC.ExtractArea(ExtractionPacket);

                                                                                             })); }
        }

        SimpleCommand<object, object> _extractAreas;

        #endregion



    }
}
