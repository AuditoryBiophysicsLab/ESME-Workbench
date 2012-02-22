﻿using System;
using HRC.Navigation;

namespace ESME.Environment
{
    public interface IEnvironmentalDataSource<out T> : IESMEPlugin
    {
        float[] AvailableResolutions { get; }
        string DataLocation { get; set; }
        string DataLocationHelp { get; }
        bool IsTimeVariantData { get; }
        TimePeriod[] AvailableTimePeriods { get; }
        T Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod, IProgress<float> progress = null);
    }
}
