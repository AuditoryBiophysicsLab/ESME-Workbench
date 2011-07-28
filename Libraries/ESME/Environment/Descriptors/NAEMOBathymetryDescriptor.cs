using System;
using System.Collections.Generic;
using ESME.Metadata;

namespace ESME.Environment.Descriptors
{
    public class NAEMOBathymetryDescriptor : NAEMODescriptor<Bathymetry, NAEMOBathymetryMetadata>, IEqualityComparer<NAEMOBathymetryDescriptor>, IComparable<NAEMOBathymetryDescriptor>
    {
        #region public OverlayFile Data { get; set; }

        public override Bathymetry Data
        {
            get { return _data ?? (_data = Bathymetry.FromYXZ(DataFilename,-1)); }
            internal set { _data = value; }
        }
        Bathymetry _data;

        #endregion

        #region Implementation of IEqualityComparer<in NAEMOBathymetryDescriptor>
        /// <summary>
        /// Determines whether the specified objects are equal.
        /// </summary>
        /// <returns>
        /// true if the specified objects are equal; otherwise, false.
        /// </returns>
        /// <param name="x">The first object of type <paramref name="T"/> to compare.</param><param name="y">The second object of type <paramref name="T"/> to compare.</param>
        public bool Equals(NAEMOBathymetryDescriptor x, NAEMOBathymetryDescriptor y) { return x.DataFilename == y.DataFilename; }

        /// <summary>
        /// Returns a hash code for the specified object.
        /// </summary>
        /// <returns>
        /// A hash code for the specified object.
        /// </returns>
        /// <param name="obj">The <see cref="T:System.Object"/> for which a hash code is to be returned.</param><exception cref="T:System.ArgumentNullException">The type of <paramref name="obj"/> is a reference type and <paramref name="obj"/> is null.</exception>
        public int GetHashCode(NAEMOBathymetryDescriptor obj) { return obj.DataFilename.GetHashCode(); }
        #endregion

        #region Implementation of IComparable<in NAEMOBathymetryDescriptor>
        /// <summary>
        /// Compares the current object with another object of the same type.
        /// </summary>
        /// <returns>
        /// A value that indicates the relative order of the objects being compared. The return value has the following meanings: Value Meaning Less than zero This object is less than the <paramref name="other"/> parameter.Zero This object is equal to <paramref name="other"/>. Greater than zero This object is greater than <paramref name="other"/>. 
        /// </returns>
        /// <param name="other">An object to compare with this object.</param>
        public int CompareTo(NAEMOBathymetryDescriptor other) { return DataFilename.CompareTo(other.DataFilename); }
        #endregion
    }
}