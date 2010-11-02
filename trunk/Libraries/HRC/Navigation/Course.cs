using System;

namespace HRC.Navigation
{
    public class Course
    {
        double _course;

        const double DegreesToRadians = Math.PI/180.0;
        const double RadiansToDegrees = 180.0/Math.PI;

        public Course()
        {
            _course = 0;
            Normalize();
        }

        /// <summary>
        /// Create a new Course object
        /// </summary>
        /// <param name="initialBearing">Initial course, in degrees</param>
        public Course(double initialBearing)
        {
            _course = initialBearing;
            Normalize();
        }

        /// <summary>
        /// Create a new Course object, which will be initialized with the course required to get from initialPoint to finalPoint
        /// </summary>
        /// <param name="initialPoint">Starting point of the course</param>
        /// <param name="finalPoint">Ending point of the course</param>
        public Course(EarthCoordinate initialPoint, EarthCoordinate finalPoint)
        {
            _course = initialPoint.GetBearingTo_Degrees(finalPoint);
            Normalize();
        }

        public double Degrees
        {
            get { return _course; }
            set
            {
                _course = value;
                Normalize();
            }
        }

        public double Radians
        {
            get { return _course*DegreesToRadians; }
            set
            {
                _course = value*RadiansToDegrees;
                Normalize();
            }
        }

        public static Course operator +(Course c1, Course c2) { return new Course(c1.Degrees + c2.Degrees); }
        public static Course operator +(Course c1, double c2) { return new Course(c1.Degrees + c2); }
        public static Course operator -(Course c1, Course c2) { return new Course(c1.Degrees - c2.Degrees); }
        public static Course operator -(Course c1, double c2) { return new Course(c1.Degrees - c2); }

        public double Reciprocal { get; private set; }

        public double ReciprocalRadians
        {
            get { return Reciprocal*DegreesToRadians; }
        }

        // Reflects a bearing given the normal vector (bearing) of the reflecting surface.
        // For correct results, please ensure the normal vector is pointing towards the inbound
        // bearing vector.
        public void Reflect(Course normalToReflector)
        {
            var myX = Math.Sin(Radians);
            var myY = Math.Cos(Radians);
            var normX = Math.Sin(normalToReflector.Radians);
            var normY = Math.Cos(normalToReflector.Radians);

            // Compute the dot product of the current bearing and the normal vector;
            var dot = (myX*normX) + (myY*normY);
            var newX = myX - (2*dot*normX);
            var newY = myY - (2*dot*normY);

            _course = Math.Atan2(newX, newY)*RadiansToDegrees;
            Normalize();
        }

        // Normalize the bearing to +/- 180 degrees
        void Normalize()
        {
            while (_course > 180) _course -= 360;
            while (_course < -180) _course += 360;
            Reciprocal = _course + 180;
            while (Reciprocal > 180) Reciprocal -= 360;
            while (Reciprocal < -180) Reciprocal += 360;
        }
    }

    public class PieSlice
    {
        readonly double _left;
        readonly double _right;
        #region public constructor

        /// <summary>
        /// Construct a pie slice which runs clockwise from the left hand course to the right hand course
        /// </summary>
        /// <param name="leftHandCourse">Left hand course</param>
        /// <param name="rightHandCourse">Right hand course</param>
        public PieSlice(Course leftHandCourse, Course rightHandCourse) : this(leftHandCourse.Degrees, rightHandCourse.Degrees) { }

        /// <summary>
        /// Construct a pie slice which runs clockwise from the left hand course to the right hand course
        /// </summary>
        /// <param name="leftHandCourse">Left hand course, in degrees</param>
        /// <param name="rightHandCourse">Right hand course, in degrees</param>
        public PieSlice(double leftHandCourse, double rightHandCourse)
        {
            _left = Normalize(leftHandCourse);
            _right = Normalize(rightHandCourse);
            _right -= _left;
        }

        #endregion

        public bool Contains(double course)
        {
            return (Normalize(course) - _left) <= _right;
        }

        public bool IsLeftCloserTo(double course)
        {
            var normalizedCourse = (Normalize(course) - _left);
            return normalizedCourse < (_right - normalizedCourse);
        }

        public bool IsRightCloserTo(double course)
        {
            var normalizedCourse = (Normalize(course) - _left);
            return normalizedCourse > (_right - normalizedCourse);
        }

        // Normalize the value to fall into the range of 0 <= value <= 360 degrees
        static double Normalize(double value)
        {
            while (value < 0.0) value += 360.0;
            return value % 360.0;
        }

    }
}