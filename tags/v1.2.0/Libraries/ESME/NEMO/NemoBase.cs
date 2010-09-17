using System;
using System.Text;
using System.Xml;
using HRC.Navigation;

namespace ESME.NEMO
{
    public abstract class NemoBase
    {
        protected XmlNode ParentNode;

        static NemoBase()
        {
            SimulationStepTime = new TimeSpan(0, 0, 1);
        }

        protected NemoBase()
        {
        }

        protected NemoBase(XmlNode parentNode)
        {
            ParentNode = parentNode;
        }

        public static TimeSpan SimulationStepTime { get; set; }

        protected bool GetBool(string childElementName)
        {
            bool tmpval;
            if (bool.TryParse(GetInnerText(childElementName), out tmpval))
                return tmpval;
            throw new FormatException(FormatError(childElementName));
        }

        protected int GetInt(string childElementName)
        {
            int tmpval;
            if (int.TryParse(GetInnerText(childElementName), out tmpval))
                return tmpval;
            throw new FormatException(FormatError(childElementName));
        }

        protected float GetFloat(string childElementName)
        {
            float tmpval;
            if (float.TryParse(GetInnerText(childElementName), out tmpval))
                return tmpval;
            throw new FormatException(FormatError(childElementName));
        }

        protected DateTime GetDateTime(string childElementName)
        {
            DateTime tmpval;
            if (DateTime.TryParse(GetInnerText(childElementName), out tmpval))
                return tmpval;
            throw new FormatException(FormatError(childElementName));
        }

        protected TimeSpan GetTimeSpan(string childElementName)
        {
            TimeSpan tmpval;
            if (TimeSpan.TryParse(GetInnerText(childElementName).Trim().Replace(' ', '.'), out tmpval))
                return tmpval;
            throw new FormatException(FormatError(childElementName));
        }

        protected EarthCoordinate GetEarthCoordinate(string latitudeElementName, string longitudeElementName)
        {
            return new EarthCoordinate(GetFloat(latitudeElementName), GetFloat(longitudeElementName));
        }

        protected EarthCoordinate3D GetEarthCoordinate3D(string latitudeElementName, string longitudeElementName,
                                                         string elevationElementName)
        {
            return new EarthCoordinate3D(GetFloat(latitudeElementName), GetFloat(longitudeElementName),
                                         GetFloat(elevationElementName));
        }

        protected string GetString(string childElementName)
        {
            return GetInnerText(childElementName);
        }

        private string GetInnerText(string childElementName)
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

        private string XmlPath(string childElementName)
        {
            XmlNode curNode = ParentNode;
            var path = new StringBuilder(childElementName);
            while (curNode.ParentNode != null)
            {
                path.Insert(0, curNode.Name + ".");
                curNode = curNode.ParentNode;
            }
            return path.ToString();
        }

        private string FormatError(string childElementName)
        {
            return string.Format("{0} format error", XmlPath(childElementName));
        }

        private string MissingNode(string childElementName)
        {
            return string.Format("{0} is missing", XmlPath(childElementName));
        }
    }
}