using System;
using System.Collections.Generic;
using System.Collections;
using System.Text;
using System.Diagnostics;
using mbs;


namespace MMMBSLib
{
    public class Geography
    {

        // Determine the bearing of 2 relative to 1
        public static DISTANGL DetermineBearing(double lat1, double lon1, double lat2, double lon2)
        {
            // The sign convention used in the following equations is that latitude is
            // negative in the South, positive in the North, and that longitude is positive in 
            // the West and negative in the East.  This is opposite of normal convention and 
            // requires some temporary sign manipulation.

            // The following labeling conventions are used: lat1 and lon1 are the latitude and
            // longitude of the sound source, lat2 and lon2 are the latitude and longitude of the 
            // animat.

            double bearing; 	// bearing from animat to sound source
            double d_rads;			// distance from animat to sound source in radians
            double rad_lon1, rad_lon2, rad_lat1, rad_lat2;	// radian conversions of latitude and longitude
            DISTANGL distBear;

            // Signs are temporarily changed for longitude to be used in the following equations which 
            // follow the convention of W being a positive value and E being a negative value
            rad_lon1 = (Math.PI / 180) * (-lon1);
            rad_lon2 = (Math.PI / 180) * (-lon2);
            rad_lat1 = (Math.PI / 180) * (lat1);
            rad_lat2 = (Math.PI / 180) * (lat2);

            // Calculate distance between points
            d_rads =
                2 * Math.Asin(
                        Math.Sqrt(Math.Pow((Math.Sin((rad_lat1 - rad_lat2) / 2)), 2)
                        +
                        Math.Cos(rad_lat1) * Math.Cos(rad_lat2) * Math.Pow((Math.Sin((rad_lon1 - rad_lon2) / 2)), 2)
                        )
                    );

            // Determine the bearing in radians
            if(Math.Sin(rad_lon2 - rad_lon1) == 0)
            {
                if(rad_lat1 == rad_lat2 || rad_lat1 < rad_lat2)
                {
                    distBear.angle = 0;
                    distBear.distance = d_rads * 1852 * (180 * 60) / Math.PI; // convert to meters.
                    return distBear;
                }
                else
                {
                    distBear.angle = 180;
                    distBear.distance = d_rads * 1852 * (180 * 60) / Math.PI; // convert to meters.
                    return distBear;
                }
            }
            else if(Math.Sin(rad_lon2 - rad_lon1) < 0)
                bearing = Math.Acos((Math.Sin(rad_lat2) - Math.Sin(rad_lat1) * Math.Cos(d_rads)) / (Math.Sin(d_rads) * Math.Cos(rad_lat1)));
            else
                bearing = 2 * Math.PI - Math.Acos((Math.Sin(rad_lat2) - Math.Sin(rad_lat1) * Math.Cos(d_rads)) / (Math.Sin(d_rads) * Math.Cos(rad_lat1)));

            // Convert bearing back to degrees
            bearing = (180 / Math.PI) * bearing;

            distBear.angle = bearing;
            distBear.distance = d_rads * 1852 * (180 * 60) / Math.PI; // convert to meters.

            return distBear;
        }

        //-----------------------------------------------------------------------------------//
        // Not currently being used.  Put into a separate library.  Accuracy is doubted too..
        //-----------------------------------------------------------------------------------//
        public static double MetersBetweenCoordinates(double Lat1, double Lon1, double Lat2, double Lon2)
        {
            double rad_lon1, rad_lon2, rad_lat1, rad_lat2;	// radian conversions of latitude and longitude
            double d_rads; // distance in radians.
            double d_meters; // distance in meters;
            // Signs are temporarily changed for longitude to be used in the following equations which 
            // follow the convention of W being a positive value and E being a negative value
            rad_lon1 = (Math.PI / 180) * (-Lon1);
            rad_lon2 = (Math.PI / 180) * (-Lon2);
            rad_lat1 = (Math.PI / 180) * (Lat1);
            rad_lat2 = (Math.PI / 180) * (Lat2);


            // Calculate distance between points
            d_rads =
                2 * Math.Asin(
                        Math.Sqrt(
                            Math.Pow((Math.Sin((rad_lat1 - rad_lat2) / 2)), 2)
                            +
                            Math.Cos(rad_lat1) * Math.Cos(rad_lat2) * Math.Pow(
                                                                        (Math.Sin((rad_lon1 - rad_lon2) / 2)),
                                                                        2)
                        )
                    );

            d_meters = d_rads * 1852 * (180 * 60) / Math.PI; // convert to meters.

            return d_meters;
        }

    }
}