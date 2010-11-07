using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using HRC.Navigation;

namespace ESME.Environment
{
    public class Transect
    {
        private EarthCoordinate mStartPoint, mEndPoint, mMidPoint;
        private string mName;
        private double mLength_meters;
        private double mBearing_degrees;

        public Transect(string Name, EarthCoordinate StartPoint, EarthCoordinate EndPoint)
        {
            mName = Name;
            mStartPoint = new EarthCoordinate(StartPoint);
            mEndPoint = EndPoint;
            mLength_meters = mStartPoint.GetDistanceTo_Meters(mEndPoint);
            mBearing_degrees = mStartPoint.GetBearingTo_Degrees(mEndPoint);
            mMidPoint = new EarthCoordinate(StartPoint);
            mMidPoint.Move(mBearing_degrees, mLength_meters / 2);
        }

        public Transect(string Name, EarthCoordinate StartPoint, double BearingAngle_degrees, double Length_meters)
        {
            mName = Name;
            mStartPoint = new EarthCoordinate(StartPoint);
            mEndPoint = new EarthCoordinate(mStartPoint, BearingAngle_degrees, Length_meters);
            mLength_meters = Length_meters;
            mBearing_degrees = BearingAngle_degrees;
            mMidPoint = new EarthCoordinate(StartPoint);
            mMidPoint.Move(mBearing_degrees, mLength_meters / 2);
        }

        public double Bearing_degrees { get { return mBearing_degrees; } }
        public double Length_meters { get { return mLength_meters; } }
        public string Name { get { return mName; } }
        public EarthCoordinate StartPoint { get { return new EarthCoordinate(mStartPoint); } }
        public EarthCoordinate EndPoint { get { return new EarthCoordinate(mEndPoint); } }
        public EarthCoordinate MidPoint { get { return new EarthCoordinate(mMidPoint); } }
    }
}
