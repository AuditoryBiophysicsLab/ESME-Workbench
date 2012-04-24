using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Xml.Serialization;
using ESME.Environment;
using ESME.Model;
using HRC.Aspects;
using HRC.Navigation;
using HRC.ViewModels;

namespace ESME.TransmissionLoss.CASS
{
    public class NAEMOEnvironmentFile : ViewModelBase
    {
        public string OverlayFileName { get; set; }
        public string BathymetryFilename { get; set; }
        public TimePeriod TimePeriod { get; set; }
        [Initialize] public List<NAEMOEnvironmentLocation> Locations { get; set; }

        #region public Dictionary<string, List<Geo>> SedimentTypes { get; set; }
        [XmlIgnore]
        public Dictionary<string, List<Geo>> SedimentTypes
        {
            get { return _sedimentTypes ?? (_sedimentTypes = CreateSedimentDictionary()); }
        }

        Dictionary<string, List<Geo>> _sedimentTypes;

        Dictionary<string, List<Geo>> CreateSedimentDictionary()
        {
            var result = new Dictionary<string, List<Geo>>();
            foreach (var location in Locations)
            {
                var curSedimentType = location.BottomType;
                if (!result.ContainsKey(curSedimentType)) result.Add(curSedimentType, new List<Geo>());
                result[curSedimentType].Add(new Geo(location));
            }
            return result;
        }

        #endregion

        public EnvironmentInformation EnvironmentInformation { get; set; }

        #region Parser

        static KeyValuePair<string, string> ParseKeyValuePair(string line)
        {
            var result = line.Split('=');
            if (result.Length != 2) throw new FormatException("Invalid key/value pair");
            return new KeyValuePair<string, string>(result[0], result[1]);
        }

        public static NAEMOEnvironmentFile Load(string environmentFileName)
        {
            var result = new NAEMOEnvironmentFile();
            var resarray = File.ReadAllLines(environmentFileName).ToList();
            var rawvalues = new List<List<string>>();
            var curLineIndex = 0;
            var space = new[] { ' ' };
            var equals = new[] { '=' };
            //make packets
            while (curLineIndex < resarray.Count)
            {
                var thisline = resarray[curLineIndex++].Trim();
                if (curLineIndex >= resarray.Count) break;
                if (thisline.StartsWith("COMMENT TABLE"))
                {
                    while (!thisline.StartsWith("EOT"))
                    {
                        if (thisline.StartsWith("limitName") && thisline.Contains("=")) result.OverlayFileName = Path.GetFileName(ParseKeyValuePair(thisline).Value);
                        if (thisline.StartsWith("bathName") && thisline.Contains("=")) result.BathymetryFilename = Path.GetFileName(ParseKeyValuePair(thisline).Value);
                        if (thisline.StartsWith("timeFrame") && thisline.Contains("=")) result.TimePeriod = (TimePeriod)Enum.Parse(typeof(TimePeriod), Path.GetFileName(ParseKeyValuePair(thisline).Value), true);
                        thisline = resarray[curLineIndex++].Trim();
                    }
                }
                //if the line starts with 'ENVIRONMENT LATITUDE', add it plus everything up to the next blank line to rawvalues[i].
                if (!thisline.StartsWith("ENVIRONMENT LATITUDE")) continue;
                var curGroup = new List<string> { thisline.Trim() };
                while (!string.IsNullOrEmpty(thisline = resarray[curLineIndex++].Trim()))
                {
                    if (curLineIndex >= resarray.Count) break;
                    curGroup.Add(thisline);
                }
                rawvalues.Add(curGroup);
            }

            foreach (var packet in rawvalues)
            {
                double lat;
                double lon;
                if (!double.TryParse(packet[0].Split(space, StringSplitOptions.RemoveEmptyEntries)[3], out lat)) throw new DataMisalignedException("");
                if (!double.TryParse(packet[1].Split(space, StringSplitOptions.RemoveEmptyEntries)[3], out lon)) throw new DataMisalignedException("");
                var retpacket = new NAEMOEnvironmentLocation(new Geo(lat, lon))
                {
                    Filename = environmentFileName
                };
                var curGroupLineIndex = 0;
                while (curGroupLineIndex < packet.Count)
                {
                    var curLine = packet[curGroupLineIndex++].Trim();
                    if (curLine.StartsWith("M"))
                    {
                        var index = curGroupLineIndex;
                        var depths = new List<double>();
                        var speeds = new List<double>();
                        while (!packet[index].Contains("EOT"))
                        {
                            double depth;
                            double speed;
                            if (!double.TryParse(packet[index].Split(space, StringSplitOptions.RemoveEmptyEntries)[0], out depth) ||
                                !double.TryParse(packet[index].Split(space, StringSplitOptions.RemoveEmptyEntries)[1], out speed)) 
                                throw new DataException("");
                            depths.Add(depth);
                            speeds.Add(speed);
                            index++;
                        }
                        retpacket.Depths = depths;
                        retpacket.Soundspeeds = speeds;
                    }
                    if (packet[curGroupLineIndex].StartsWith("BOTTOM REFLECTION")) break;
                }
                var model = packet[curGroupLineIndex++].Split(equals, StringSplitOptions.RemoveEmptyEntries);
                retpacket.BottomTypeModel = model[1].Trim().ToUpper();
                switch (model[1].Trim().ToUpper())
                {
                    case "HFEVA":
                        retpacket.BottomType = packet[curGroupLineIndex++];
                        break;
                    case "HFBL":
                        curGroupLineIndex++;    // Skip one line
                        break;
                    case "LFBL_HFB":
                        curGroupLineIndex += 12;
                        break;
                    case "LFBL_PE":
                        curGroupLineIndex += 11;
                        break;
                }
                double wind;
                if (!double.TryParse(packet[curGroupLineIndex].Split(space, StringSplitOptions.RemoveEmptyEntries)[3], out wind)) throw new DataException("");
                retpacket.WindSpeed = wind;
                result.Locations.Add(retpacket);
            }
            result.EnvironmentInformation = new EnvironmentInformation
            {
                Wind = new Wind(),
                SoundSpeedField = new SoundSpeedField {TimePeriod = result.TimePeriod},
                Sediment = new Sediment(),
            };
            var windData = new TimePeriodEnvironmentData<WindSample> { TimePeriod = result.TimePeriod };
            foreach (var location in result.Locations)
            {
                windData.EnvironmentData.Add(new WindSample(location, (float)location.WindSpeed));
                if (location.BottomTypeModel == "HFEVA")
                {
                    var bottomType = location.BottomType == "SAND" ? "Medium Sand or Sand" : location.BottomType;
                    result.EnvironmentInformation.Sediment.Samples.Add(new SedimentSample(location,
                                                                   new SedimentSampleBase
                                                                   {
                                                                       SampleValue = (short)Model.SedimentTypes.Find(bottomType).HFEVACategory
                                                                   }));
                }
                var profileData = location.Depths.Select((t, depthIndex) => new SoundSpeedSample((float)t, (float)location.Soundspeeds[depthIndex])).ToList();
                result.EnvironmentInformation.SoundSpeedField.EnvironmentData.Add(new SoundSpeedProfile(location){ Data = profileData});
            }
            result.EnvironmentInformation.Wind.TimePeriods.Add(windData); 
            return result;
        }
        #endregion
    }
}
