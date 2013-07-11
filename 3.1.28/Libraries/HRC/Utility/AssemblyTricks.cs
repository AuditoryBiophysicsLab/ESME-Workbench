using System;

namespace HRC.Utility
{
    public static class AssemblyTricks
    {
        // Lightly adapted from http://stackoverflow.com/questions/1600962/c-displaying-the-build-date
        // For alternative methods, see http://www.codinghorror.com/blog/archives/000264.html
        // Documentation on the PE Header format: http://support.microsoft.com/kb/65122
        public static DateTime RetrieveLinkerTimestamp()
        {
            var filePath = System.Reflection.Assembly.GetCallingAssembly().Location;
            const int cPeHeaderOffset = 60;
            const int cLinkerTimestampOffset = 8;
            var b = new byte[2048];
            System.IO.Stream s = null;

            try
            {
                if (filePath != null)
                {
                    s = new System.IO.FileStream(filePath, System.IO.FileMode.Open, System.IO.FileAccess.Read);
                    s.Read(b, 0, 2048);
                }
            }
            finally
            {
                if (s != null)
                {
                    s.Close();
                }
            }

            var i = BitConverter.ToInt32(b, cPeHeaderOffset);
            int secondsSince1970 = BitConverter.ToInt32(b, i + cLinkerTimestampOffset);
            var dt = new DateTime(1970, 1, 1, 0, 0, 0);
            dt = dt.AddSeconds(secondsSince1970);
            dt = dt.AddHours(TimeZone.CurrentTimeZone.GetUtcOffset(dt).Hours);
            return dt;
        }
    }
}
