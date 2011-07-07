﻿using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.IO;
using System.Linq;
using Cinch;
using ESME.Environment.NAVO;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.TransmissionLoss.CASS
{
    public class NAEMOEnvironmentFile : PropertyChangedBase 
    {
        #region public string OverlayFileName { get; set; }

        public string OverlayFileName
        {
            get { return _overlayFileName; }
            set
            {
                if (_overlayFileName == value) return;
                _overlayFileName = value;
                NotifyPropertyChanged(OverlayFileNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OverlayFileNameChangedEventArgs = ObservableHelper.CreateArgs<NAEMOEnvironmentFile>(x => x.OverlayFileName);
        string _overlayFileName;

        #endregion

        #region public string BathymetryFilename { get; set; }

        public string BathymetryFilename
        {
            get { return _bathymetryFilename; }
            set
            {
                if (_bathymetryFilename == value) return;
                _bathymetryFilename = value;
                NotifyPropertyChanged(BathymetryFilenameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BathymetryFilenameChangedEventArgs = ObservableHelper.CreateArgs<NAEMOEnvironmentFile>(x => x.BathymetryFilename);
        string _bathymetryFilename;

        #endregion

        #region public NAVOTimePeriod TimePeriod { get; set; }

        public NAVOTimePeriod TimePeriod
        {
            get { return _timePeriod; }
            set
            {
                if (_timePeriod == value) return;
                _timePeriod = value;
                NotifyPropertyChanged(TimePeriodChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TimePeriodChangedEventArgs = ObservableHelper.CreateArgs<NAEMOEnvironmentFile>(x => x.TimePeriod);
        NAVOTimePeriod _timePeriod;

        #endregion

        #region public List<NAEMOEnvironmentLocation> Locations { get; set; }

        public List<NAEMOEnvironmentLocation> Locations
        {
            get { return _locations ?? (_locations = new List<NAEMOEnvironmentLocation>()); }
            set
            {
                if (_locations == value) return;
                _locations = value;
                NotifyPropertyChanged(LocationsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LocationsChangedEventArgs = ObservableHelper.CreateArgs<NAEMOEnvironmentFile>(x => x.Locations);
        List<NAEMOEnvironmentLocation> _locations;

        #endregion

        #region Parser

        static System.Collections.Generic.KeyValuePair<string, string> ParseKeyValuePair(string line)
        {
            var result = line.Split('=');
            if (result.Length != 2) throw new FormatException("Invalid key/value pair");
            return new System.Collections.Generic.KeyValuePair<string, string>(result[0], result[1]);
        }

        public static NAEMOEnvironmentFile Load(string environmentFileName)
        {
            var result = new NAEMOEnvironmentFile();
            var resarray = File.ReadAllLines(environmentFileName).ToList();
            var rawvalues = new List<List<string>>();
            var curLineIndex = 0;
            var space = new[] {' '};
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
                        if (thisline.StartsWith("timeFrame") && thisline.Contains("=")) result.TimePeriod = (NAVOTimePeriod)Enum.Parse(typeof(NAVOTimePeriod), Path.GetFileName(ParseKeyValuePair(thisline).Value), true);
                        thisline = resarray[curLineIndex++].Trim();
                    }
                }
                //if the line starts with 'ENVIRONMENT LATITUDE', add it plus everything up to the next blank line to rawvalues[i].
                if (thisline.StartsWith("ENVIRONMENT LATITUDE"))
                {
                    var curGroup = new List<string>
                                   {
                                       thisline.Trim()
                                   };
                    while (!string.IsNullOrEmpty(thisline = resarray[curLineIndex++].Trim()))
                    {
                        if (curLineIndex >= resarray.Count) break;
                        curGroup.Add(thisline);
                    }
                    rawvalues.Add(curGroup);
                }
            }

            foreach (var packet in rawvalues)
            {
                double lat;
                double lon;
                if (!double.TryParse(packet[0].Split(space, StringSplitOptions.RemoveEmptyEntries)[3], out lat)) throw new DataMisalignedException("");
                if (!double.TryParse(packet[1].Split(space, StringSplitOptions.RemoveEmptyEntries)[3], out lon)) throw new DataMisalignedException("");
                var retpacket = new NAEMOEnvironmentLocation(new EarthCoordinate(lat, lon))
                {
                    Latitude = lat,
                    Longitude = lon,
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
                                !double.TryParse(packet[index].Split(space, StringSplitOptions.RemoveEmptyEntries)[1], out speed)) throw new DataException("");
                            depths.Add(depth);
                            speeds.Add(speed);
                            index++;
                        }
                        retpacket.Depths = depths;
                        retpacket.Soundspeeds = speeds;
                    }
                    if (curLine.StartsWith("BOTTOM REFLECTION"))
                    {
                        var bottom = packet[curGroupLineIndex++];
                        if (bottom.Contains("WIND SPEED"))
                        {
                            double wind;
                            if (!double.TryParse(bottom.Split(space, StringSplitOptions.RemoveEmptyEntries)[3], out wind)) throw new DataException("");
                            retpacket.WindSpeed = wind;
                        }
                        else
                        {
                            retpacket.BottomType = bottom;
                            var speed = packet[curGroupLineIndex];
                            double wind;
                            if (!double.TryParse(speed.Split(space, StringSplitOptions.RemoveEmptyEntries)[3], out wind)) throw new DataException("");
                            retpacket.WindSpeed = wind;
                        }
                    }
                }
                result.Locations.Add(retpacket);
            }
            return result;
        }
        #endregion
    }
}