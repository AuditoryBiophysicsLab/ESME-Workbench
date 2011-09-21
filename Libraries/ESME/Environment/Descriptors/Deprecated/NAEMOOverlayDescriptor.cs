using System;
using System.Collections.Generic;
using ESME.Metadata;
using ESME.NEMO.Overlay;

namespace ESME.Environment.Descriptors
{
#if false
    public class NAEMOOverlayDescriptor : NAEMODescriptor<OverlayFile, NAEMOOverlayMetadata>, IEqualityComparer<NAEMOOverlayDescriptor>, IComparable<NAEMOOverlayDescriptor>
    {
        #region public OverlayFile Data { get; set; }

        public override OverlayFile Data
        {
            get { return _data ?? (_data = new OverlayFile(DataFilename)); }
            internal set { _data = value; }
        }
        OverlayFile _data;

        #endregion

        #region Implementation of IEqualityComparer<in NAEMOOverlayDescriptor>
        /// <summary>
        /// Determines whether the specified objects are equal.
        /// </summary>
        /// <returns>
        /// true if the specified objects are equal; otherwise, false.
        /// </returns>
        /// <param name="x">The first object of type <paramref name="T"/> to compare.</param><param name="y">The second object of type <paramref name="T"/> to compare.</param>
        public bool Equals(NAEMOOverlayDescriptor x, NAEMOOverlayDescriptor y) { return x.DataFilename == y.DataFilename; }

        /// <summary>
        /// Returns a hash code for the specified object.
        /// </summary>
        /// <returns>
        /// A hash code for the specified object.
        /// </returns>
        /// <param name="obj">The <see cref="T:System.Object"/> for which a hash code is to be returned.</param><exception cref="T:System.ArgumentNullException">The type of <paramref name="obj"/> is a reference type and <paramref name="obj"/> is null.</exception>
        public int GetHashCode(NAEMOOverlayDescriptor obj) { return obj.DataFilename.GetHashCode(); }
        #endregion

        #region Implementation of IComparable<in NAEMOOverlayDescriptor>
        /// <summary>
        /// Compares the current object with another object of the same type.
        /// </summary>
        /// <returns>
        /// A value that indicates the relative order of the objects being compared. The return value has the following meanings: Value Meaning Less than zero This object is less than the <paramref name="other"/> parameter.Zero This object is equal to <paramref name="other"/>. Greater than zero This object is greater than <paramref name="other"/>. 
        /// </returns>
        /// <param name="other">An object to compare with this object.</param>
        public int CompareTo(NAEMOOverlayDescriptor other) { return DataFilename.CompareTo(other.DataFilename); }
        #endregion
    }
#endif
}