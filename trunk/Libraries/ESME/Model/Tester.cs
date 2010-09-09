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

namespace ESME.Model
{
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
            TransmissionLossField tlf = new TransmissionLossField
            {
                Filename = "",
                MaxTLDepth_meters = 2000,
                AnalysisPoint = new AnalysisPoint
                {
                    AcousticProperties = new AcousticProperties
                    {
                        DepressionElevationAngle_degrees = 0,
                        HighFrequency_Hz = 3500,
                        LowFrequency_Hz = 3500,
                        SourceDepth_meters = 10,
                        VerticalBeamWidth_degrees = 90,
                    },
                    FieldRadius_meters = 10000,
                    RangeCellSize_meters = 50,
                    DepthCellSize_meters = 10,
                    Location = new EarthCoordinate
                    {
                        Latitude_degrees = Environment.Bathymetry.Latitudes_degrees[Environment.Bathymetry.Latitudes_degrees.Length / 2],
                        Longitude_degrees = Environment.Bathymetry.Longitudes_degrees[Environment.Bathymetry.Longitudes_degrees.Length / 2],
                    },
                },
                RadialCount = 4,
            };
            Globals.CurrentLocationName = "Test";
            BellhopRunFile RunFile = Bellhop.CreateRunFile(tlf, Environment);
            string Filename = Globals.RandomRunFileFilename;
            RunFile.Save(Filename);
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
                AnalysisPoints = new AnalysisPointList(),
                SpeciesList = new SpeciesList(@"C:\Projects\ESME\Workbench\Species"),
                TransmissionLossFields = new TransmissionLossFieldList(),
            };

            experiment.SpeciesList.ItemDeleted += new EventHandler<ItemDeletedEventArgs<Species>>(SpeciesList_ItemDeleted);

            experiment.Animats = new AnimatList(experiment.SpeciesList);

            AnalysisPoint analysisPoint1 = new AnalysisPoint
            {
                FieldRadius_meters = 20000,
                Location = new EarthCoordinate(0, 0),
                AcousticProperties = new AcousticProperties
                {
                    DepressionElevationAngle_degrees = 0,
                    SourceDepth_meters = 10,
                    HighFrequency_Hz = 3500,
                    LowFrequency_Hz = 3500,
                    VerticalBeamWidth_degrees = 90,
                },
            };
            AnalysisPoint analysisPoint2 = new AnalysisPoint(analysisPoint1)
            {
                Location = new EarthCoordinate(1, 1),
            };
            experiment.AnalysisPoints.Add(analysisPoint1);
            experiment.AnalysisPoints.Add(analysisPoint2);

            experiment.FixedSources.Add(new FixedSource(analysisPoint1)
            {
                Name = "Source 1",
                Bearing_degrees = 0,
                HorizontalHalfAngle_degrees = 90,
                PingDuration_seconds = 1,
                PingInterval_seconds = 30,
            });
            experiment.FixedSources.Add(new FixedSource(analysisPoint2)
            {
                Name = "Source 2",
                Bearing_degrees = 0,
                HorizontalHalfAngle_degrees = 90,
                PingDuration_seconds = 1,
                PingInterval_seconds = 30,
            });
            experiment.TransmissionLossFields.Add(new TransmissionLossField(analysisPoint1)
            {
                Filename = "",
                MaxTLDepth_meters = 2000,
                RadialCount = 8,
            });
            experiment.TransmissionLossFields.Add(new TransmissionLossField(analysisPoint2)
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
}
