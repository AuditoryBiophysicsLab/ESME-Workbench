using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using HRC.Navigation;

namespace ESME.NEMO
{
    public abstract class NemoBase: IHaveProperties
    {
        protected XmlNode ParentNode;

        static NemoBase() { SimulationStepTime = new TimeSpan(0, 0, 1); }

        protected NemoBase() { }

        protected NemoBase(XmlNode parentNode) { ParentNode = parentNode; }

        #region public TimeSpan SimulationStepTime { get; set; }

        public static TimeSpan SimulationStepTime
        {
            get { return _simulationStepTime; }
            set
            {
                if (_simulationStepTime == value) return;
                if (value.TotalSeconds <= 0.0) throw new ParameterOutOfRangeException(string.Format("SimulationStepTime must be a positive TimeSpan. {0} is an invalid value", _simulationStepTime));
                if (value.TotalSeconds != Math.Floor(value.TotalSeconds)) throw new ParameterOutOfRangeException(string.Format("SimulationStepTime must specify an integer number of seconds.  {0} is an invalid value", _simulationStepTime));
                _simulationStepTime = value;
            }
        }

        static TimeSpan _simulationStepTime;

        #endregion

        public abstract IEnumerable<KeyValuePair<string, string>> Properties { get; }

        protected bool GetBool(string childElementName)
        {
            bool tmpval;
            if (bool.TryParse(GetInnerText(childElementName), out tmpval)) return tmpval;
            throw new FormatException(FormatError(childElementName));
        }

        protected int GetInt(string childElementName)
        {
            int tmpval;
            if (int.TryParse(GetInnerText(childElementName), out tmpval)) return tmpval;
            throw new FormatException(FormatError(childElementName));
        }

        protected float GetFloat(string childElementName)
        {
            float tmpval;
            if (float.TryParse(GetInnerText(childElementName), out tmpval)) return tmpval;
            throw new FormatException(FormatError(childElementName));
        }

        protected DateTime GetDateTime(string childElementName)
        {
            DateTime tmpval;
            if (DateTime.TryParse(GetInnerText(childElementName), out tmpval)) return tmpval;
            throw new FormatException(FormatError(childElementName));
        }

        protected TimeSpan GetTimeSpan(string childElementName)
        {
            TimeSpan tmpval;
            if (TimeSpan.TryParse(GetInnerText(childElementName).Trim().Replace(' ', '.'), out tmpval)) return tmpval;
            throw new FormatException(FormatError(childElementName));
        }

        protected EarthCoordinate GetEarthCoordinate(string latitudeElementName, string longitudeElementName) { return new EarthCoordinate(GetFloat(latitudeElementName), GetFloat(longitudeElementName)); }

        protected EarthCoordinate3D GetEarthCoordinate3D(string latitudeElementName, string longitudeElementName, string elevationElementName) { return new EarthCoordinate3D(GetFloat(latitudeElementName), GetFloat(longitudeElementName), GetFloat(elevationElementName)); }

        protected string GetString(string childElementName) { return GetInnerText(childElementName); }

        string GetInnerText(string childElementName)
        {
            try
            {
                // ReSharper disable PossibleNullReferenceException
                return ParentNode[childElementName].InnerText;
                // ReSharper restore PossibleNullReferenceException
            }
            catch (NullReferenceException)
            {
                throw new FormatException(MissingNode(childElementName));
            }
        }

        string XmlPath(string childElementName)
        {
            var curNode = ParentNode;
            var path = new StringBuilder(childElementName);
            while (curNode.ParentNode != null)
            {
                path.Insert(0, curNode.Name + ".");
                curNode = curNode.ParentNode;
            }
            return path.ToString();
        }

        string FormatError(string childElementName) { return string.Format("{0} format error", XmlPath(childElementName)); }

        string MissingNode(string childElementName) { return string.Format("{0} is missing", XmlPath(childElementName)); }
    }
}