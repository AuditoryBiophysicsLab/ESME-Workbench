using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using HRC.Utility;
using ESME.Environment;
using HRC.Navigation;
using ESME.NEMO;
using System.IO;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;
using System.Windows.Forms;
using ESME.TransmissionLoss.Bellhop;
//todo: deprecated.
namespace ESME.Model
{
#if false
   
    public static class Tester
    {
        public static void BellhopRunFileTest()
        {
            EnvironmentInformation Environment = new EnvironmentInformation
            {
                LocationName = "Bahamas",
                WindSpeed_knots = 0,
                Sediment = SedimentTypes.Find("Muddy Sand"),
                Basement = SedimentTypes.Find("Muddy Sand"),
                SoundSpeedField = new SoundSpeedField(@"C:\Users\Dave Anderson\Documents\ESME Workbench\Bahamas\Bahamas.eeb"),
                Bathymetry = new Bathymetry(@"C:\Users\Dave Anderson\Documents\ESME Workbench\Bahamas\Bahamas.eeb"),
                SoundSpeedFieldName = "",
            };
            OldTransmissionLossField tlf = new OldTransmissionLossField
            {
                Filename = "",
                MaxTLDepth_meters = 2000,
                OldAnalysisPoint = new OldAnalysisPoint
                {
                    AcousticProperties = new AcousticProperties
                    {
                        DepressionElevationAngle = 0,
                        HighFrequency = 3500,
                        LowFrequency = 3500,
                        SourceDepth = 10,
                        VerticalBeamWidth = 90,
                    },
                    FieldRadius_meters = 10000,
                    RangeCellSize_meters = 50,
                    DepthCellSize_meters = 10,
                    Location = new EarthCoordinate
                    {
                        Latitude_degrees = Environment.Bathymetry.Latitudes[Environment.Bathymetry.Latitudes.Length / 2],
                        Longitude_degrees = Environment.Bathymetry.Longitudes[Environment.Bathymetry.Longitudes.Length / 2],
                    },
                },
                RadialCount = 4,
            };
            Globals.CurrentLocationName = "Test";
            //BellhopRunFile RunFile = Bellhop.CreateRunFile(tlf, Environment); 
            string Filename = Globals.RandomRunFileFilename;
            //RunFile.Save(Filename);
            var TestFile = BellhopRunFile.Load(Filename);
        }

        public static void ExperimentTest()
        {
            ESME_Experiment experiment = new ESME_Experiment
            {
                Information = new ExperimentInformation
                {
                    Name = "Test Experiment",
                    Created = DateTime.Now,
                    Modified = DateTime.Now,
                    Author = "Dave Anderson",
                    Description = "Test Description",
                    NemoFileName = "Nemo.nemo"
                },
                Environment = new EnvironmentInformation
                {
                    LocationName = "SOCAL",
                    SoundSpeedFieldName = null,
                    WindSpeed_knots = 0,
                    Sediment = new SedimentType
                    {
                        CompressionWaveCoefficient = 1,
                        CompressionWaveSpeed_metersSec = 2,
                        Density_gramsCC = 3,
                        Name = "SedimentTest",
                        ShearWaveCoefficient = 4,
                        ShearWaveSpeed_metersSec = 5,
                    },
                    Basement = new SedimentType
                    {
                        CompressionWaveCoefficient = 6,
                        CompressionWaveSpeed_metersSec = 7,
                        Density_gramsCC = 8,
                        Name = "BasementTest",
                        ShearWaveCoefficient = 9,
                        ShearWaveSpeed_metersSec = 10,
                    },
                },
                FixedSources = new FixedSourceList(),
                NewAnalysisPoints = new OldAnalysisPointList(),
                SpeciesList = new SpeciesList(@"C:\Projects\ESME\Workbench\Species"),
                TransmissionLossFields = new OldTransmissionLossFieldList(),
            };

            experiment.SpeciesList.ItemDeleted += new EventHandler<ItemDeletedEventArgs<Species>>(SpeciesList_ItemDeleted);

            experiment.Animats = new AnimatList(experiment.SpeciesList);

            OldAnalysisPoint oldAnalysisPoint1 = new OldAnalysisPoint
            {
                FieldRadius_meters = 20000,
                Location = new EarthCoordinate(0, 0),
                AcousticProperties = new AcousticProperties
                {
                    DepressionElevationAngle = 0,
                    SourceDepth = 10,
                    HighFrequency = 3500,
                    LowFrequency = 3500,
                    VerticalBeamWidth = 90,
                },
            };
            OldAnalysisPoint oldAnalysisPoint2 = new OldAnalysisPoint(oldAnalysisPoint1)
            {
                Location = new EarthCoordinate(1, 1),
            };
            experiment.NewAnalysisPoints.Add(oldAnalysisPoint1);
            experiment.NewAnalysisPoints.Add(oldAnalysisPoint2);

            experiment.FixedSources.Add(new FixedSource(oldAnalysisPoint1)
            {
                Name = "Source 1",
                Bearing_degrees = 0,
                HorizontalHalfAngle_degrees = 90,
                PingDuration_seconds = 1,
                PingInterval_seconds = 30,
            });
            experiment.FixedSources.Add(new FixedSource(oldAnalysisPoint2)
            {
                Name = "Source 2",
                Bearing_degrees = 0,
                HorizontalHalfAngle_degrees = 90,
                PingDuration_seconds = 1,
                PingInterval_seconds = 30,
            });
            experiment.TransmissionLossFields.Add(new OldTransmissionLossField(oldAnalysisPoint1)
            {
                Filename = "",
                MaxTLDepth_meters = 2000,
                RadialCount = 8,
            });
            experiment.TransmissionLossFields.Add(new OldTransmissionLossField(oldAnalysisPoint2)
            {
                Filename = "",
                MaxTLDepth_meters = 2000,
                RadialCount = 8,
            });
            experiment.Save("test.esme");
            ESME_Experiment newExperiment = ESME_Experiment.Load("test.esme");
        }

        static void SpeciesList_ItemDeleted(object sender, ItemDeletedEventArgs<Species> e)
        {
            throw new NotImplementedException();
        }

        //ValidationEventHandler Call-back Method
        private static void ValidationError(object sender, ValidationEventArgs arguments)
        {
            MessageBox.Show(arguments.Message, "XML Validation Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
        }
    } 
#endif
}
