using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace HRC.Navigation
{
    public class Course
    {
        private double mCourse_degrees, mReciprocalCourse_degrees;
        private const double DEGREES_TO_RADIANS = Math.PI / 180.0;
        private const double RADIANS_TO_DEGREES = 180.0 / Math.PI;

        public Course()
        {
            mCourse_degrees = 0;
            Normalize();
        }

        public Course(double InitialBearing_Degrees)
        {
            mCourse_degrees = InitialBearing_Degrees;
            Normalize();
        }

        public Course(EarthCoordinate InitialPoint, EarthCoordinate FinalPoint)
        {
            mCourse_degrees = InitialPoint.GetBearingTo_Degrees(FinalPoint);
            Normalize();
        }

        public double Degrees 
        { 
            get { return mCourse_degrees; } 
            set
            {
                mCourse_degrees = value;
                Normalize();
            }
        }
        public double Radians 
        { 
            get { return mCourse_degrees * DEGREES_TO_RADIANS; }
            set
            {
                mCourse_degrees = value * RADIANS_TO_DEGREES;
                Normalize();
            }
        }

        public double ReciprocalDegrees { get { return mReciprocalCourse_degrees; } }
        public double ReciprocalRadians { get { return mReciprocalCourse_degrees * DEGREES_TO_RADIANS; } }

        // Reflects a bearing given the normal vector (bearing) of the reflecting surface.
        // For correct results, please ensure the normal vector is pointing towards the inbound
        // bearing vector.
        public void Reflect(Course NormalToReflector)
        {
            double myX, myY, normX, normY, dot, newX, newY;

            myX = Math.Sin(Radians);
            myY = Math.Cos(Radians);
            normX = Math.Sin(NormalToReflector.Radians);
            normY = Math.Cos(NormalToReflector.Radians);

            // Compute the dot product of the current bearing and the normal vector;
            dot = (myX * normX) + (myY * normY);
            newX = myX - (2 * dot * normX);
            newY = myY - (2 * dot * normY);

            mCourse_degrees = Math.Atan2(newX, newY) * RADIANS_TO_DEGREES;
            Normalize();
        }

        // Normalize the bearing to +/- 180 degrees
        private void Normalize()
        {
            while (mCourse_degrees > 180)
                mCourse_degrees -= 360;
            while (mCourse_degrees < -180)
                mCourse_degrees += 360;
            mReciprocalCourse_degrees = mCourse_degrees + 180;
            while (mReciprocalCourse_degrees > 180)
                mReciprocalCourse_degrees -= 360;
            while (mReciprocalCourse_degrees < -180)
                mReciprocalCourse_degrees += 360;
        }
    }
}
