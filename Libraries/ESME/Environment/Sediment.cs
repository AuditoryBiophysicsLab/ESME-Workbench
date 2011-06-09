using System;
using System.Collections.Generic;
using HRC.Navigation;

namespace ESME.Environment
{
    public class Sediment
    {
        private static readonly List<Type> ReferencedTypes = new List<Type>
                                                                 {
                                                                     typeof (SedimentSample),
                                                                     typeof (SedimentSampleBase),
                                                                 };

        public EnvironmentData<SedimentSample> Samples { get; private set; }

        public Sediment()
        {
            Samples = new EnvironmentData<SedimentSample>();
        }

        public static Sediment Load(string filename)
        {
            return new Sediment {Samples = EnvironmentData<SedimentSample>.Load(filename, ReferencedTypes)};
        }

        public void Save(string filename)
        {
            Samples.Save(filename, ReferencedTypes);
        }
    }
}
