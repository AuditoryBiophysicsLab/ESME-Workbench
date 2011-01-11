using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading;

namespace ESME.Environment
{
    public class DataAxis
    {
        const uint Magic = 0x1df0f868;
        //private float[] values;
        readonly List<AxisMap> _axis;

        #region Constructors

        /// <summary>
        ///   Construct a DataAxis from an array of axis values.
        /// </summary>
        /// <param name = "name">Name for the new DataAxis</param>
        /// <param name = "values">An array of values to be contained in the new DataAxis.   These values must be in ascending order or an exception will be thrown.</param>
        public DataAxis(string name, IList<float> values)
        {
            Name = name;
            _axis = new List<AxisMap>(values.Count);
            for (var i = 0; i < values.Count; i++) _axis.Add(new AxisMap(values[i], i));
        }

        /// <summary>
        ///   Constructs a DataAxis from a stream.  The source of the stream must have previously been created by calling DataAxis.Save()
        /// </summary>
        /// <param name = "stream"></param>
        public DataAxis(BinaryReader stream)
        {
            int i;

            var actualMagic = stream.ReadUInt32();
            if (Magic != actualMagic) throw new FormatException(string.Format("DataAxis: Invalid input file format.  Magic number {0:x} not found (read {1:x} instead)", Magic, actualMagic));
            Name = stream.ReadString();
            var axisLength = stream.ReadUInt16();
            _axis = new List<AxisMap>(axisLength);
            for (i = 0; i < axisLength; i++) _axis.Add(new AxisMap(stream));
            CheckAxis();
        }

        /// <summary>
        ///   Write the DataAxis to a stream
        /// </summary>
        /// <param name = "stream"></param>
        public void Save(BinaryWriter stream)
        {
            stream.Write(Magic);
            stream.Write(Name);
            stream.Write((ushort) _axis.Count);
            foreach (var axisMap in _axis) axisMap.Save(stream);
        }

        /// <summary>
        ///   Construct a DataAxis from a subset (StartValue to EndValue) of another DataAxis
        /// </summary>
        /// <param name = "source">Existing DataAxis</param>
        /// <param name = "startValue"></param>
        /// <param name = "endValue"></param>
        public DataAxis(DataAxis source, float startValue, float endValue)
        {
            if (!source.ContainsRange(startValue, endValue)) throw new ArgumentOutOfRangeException("DataAxis: Source axis does not contain the requested range");
            var lower = Math.Min(startValue, endValue);
            var higher = Math.Max(startValue, endValue);

            var result = from map in source._axis
                         where (lower <= map.Value) && (map.Value <= higher)
                         select new AxisMap(map);
            _axis = result.ToList();
            for (var i = 0; i < _axis.Count; i++) _axis[i].Index = i;
            _axis.Sort();
            Name = source.Name;
        }

        public void Normalize(float minValue, float maxValue)
        {
            if (maxValue <= minValue) throw new ArgumentOutOfRangeException("Normalize: MaxValue must be greater than MinValue");
            var range = maxValue - minValue;
            foreach (var cur in _axis)
            {
                while (cur.Value < minValue) cur.Value += range;
                while (cur.Value > maxValue) cur.Value -= range;
            }
        }

        /// <summary>
        ///   Copy constructor.  Copies an existing DataAxis to a new one.
        /// </summary>
        public DataAxis(DataAxis source)
        {
            _axis = new List<AxisMap>(source.Map.Length);
            foreach (var axisMap in source.Map) _axis.Add(new AxisMap(axisMap));
            Name = source.Name;
        }

        #endregion

        void CheckAxis()
        {
            int i;

            for (i = 0; i < _axis.Count - 2; i++) if (_axis[i].Value >= _axis[i + 1].Value) throw new ApplicationException("Axis: Axis values must be in ascending order");
        }

        /// <summary>
        ///   Check if the DataAxis contains the requested range
        /// </summary>
        /// <param name = "value1">One end of the range to be tested for</param>
        /// <param name = "value2">The other end of the range to be tested for</param>
        /// <returns>true if this DataAxis contains the requested range</returns>
        public bool ContainsRange(float value1, float value2)
        {
            var lower = Math.Min(value1, value2);
            var higher = Math.Max(value1, value2);
            if ((_axis.Find(x => x.Value <= lower) != null) && (_axis.Find(x => x.Value >= higher) != null)) return true;
            return false;
        }

        /// <summary>
        ///   Get the axis value at the specified index
        /// </summary>
        /// <param name = "index"></param>
        /// <returns></returns>
        public float this[int index]
        {
            get { return _axis[index].Value; }
        }

        public int this[float value]
        {
            get
            {
                if (value < -360 || value > 360) throw new IndexOutOfRangeException("The requested value is out of range.");
                return _axis.Find(x => x.Value >= value).Index;
            }
        }

        public AxisMap[] Map
        {
            get { return _axis.ToArray(); }
        }

        public List<AxisMap> MapList
        {
            get { return _axis; }
        }

        /// <summary>
        ///   Return the values in the axis
        /// </summary>
        public float[] Values
        {
            get
            {
                var result = from m in _axis
                             select m.Value;
                return result.ToArray();
            }
        }

        public float[] ValuesBetween(float startValue, float endValue)
        {
            if (startValue >= endValue) throw new IndexOutOfRangeException("DataAxis: StartValue must be less than EndValue");
            var result = from m in _axis
                         where ((startValue <= m.Value) && (m.Value <= endValue))
                         select m.Value;
            return result.ToArray();
        }

        public double[] DoubleValuesBetween(float startValue, float endValue)
        {
            if (startValue >= endValue) throw new IndexOutOfRangeException("DataAxis: StartValue must be less than EndValue");
            var result = from m in _axis
                         where ((startValue <= m.Value) && (m.Value <= endValue))
                         select (double)m.Value;
            return result.ToArray();
        }

        public int[] IndicesBetween(float startValue, float endValue)
        {
            if (startValue >= endValue) throw new IndexOutOfRangeException("DataAxis: StartValue must be less than EndValue");
            var result = from m in _axis
                         where ((startValue <= m.Value) && (m.Value <= endValue))
                         select m.Index;
            return result.ToArray();
        }

        /// <summary>
        ///   Return the values in the axis as an array of doubles
        /// </summary>
        public double[] DoubleValues
        {
            get
            {
                var result = from m in _axis
                             select (double) m.Value;
                return result.ToArray();
            }
        }

        public double[] UnwrappedValues
        {
            get
            {
                var found = from m in _axis
                            where ((m.Value >= -180.0) && (m.Value < 180.0))
                            orderby m.Index ascending
                            select new
                                   {
                                       m.Index,
                                       m.Value
                                   };
                return found.Select(f => f.Value).Select(dummy => (double) dummy).ToArray();
            }
        }

        /// <summary>
        ///   Get the number of wrapped elements in the DataAxis (may be more than the number of actual elements)
        /// </summary>
        public int Length
        {
            get
            {
                if (_axis == null) return 0;
                return _axis.Count;
            }
        }

        /// <summary>
        ///   Get the name of the DataAxis
        /// </summary>
        public string Name { get; set; }

        public override string ToString() { return string.Format("Name             : {0}\n" + "                                 Length : {1}\n" + "                       Min Actual Value : {2}\n" + "                       Min Actual Index : {3}\n" + "                       Max Actual Value : {4}\n" + "                       Max Actual Index : {5}", Name, Length, _axis[0].Value, _axis[0].Index, _axis.Last().Value, _axis.Last().Index); }
    }

    [DebuggerDisplay("(Value = {Value}, Index = {Index})")]
    public class AxisMap : IComparable<AxisMap>, IEquatable<AxisMap>, IEquatable<float>, IEqualityComparer<AxisMap>
    {
        public float Value { get; set; }
        public int Index { get; set; }

        public AxisMap(float value, int index)
        {
            Value = value;
            Index = index;
        }

        public AxisMap(AxisMap toCopy)
        {
            Value = toCopy.Value;
            Index = toCopy.Index;
        }

        public AxisMap(BinaryReader stream)
        {
            Value = stream.ReadSingle();
            Index = stream.ReadInt32();
        }

        public void Save(BinaryWriter stream)
        {
            stream.Write(Value);
            stream.Write(Index);
        }

        public int CompareTo(AxisMap other) { return Value.CompareTo(other.Value); }

        public override string ToString() { return string.Format("({0}, {1})", Value, Index); }

        public bool Equals(AxisMap theOne, AxisMap theOther) { return theOne.Equals(theOther); }

        public int GetHashCode(AxisMap thing) { return thing.Value.GetHashCode(); }

        public bool Equals(AxisMap other)
        {
            var product = Value/(double) (other.Value);
            if ((0.99999 <= product) && (product < 1.00001)) return true;
            return false;
        }

        public bool Equals(float other)
        {
            var product = Value/(double) other;
            if ((0.99999 <= product) && (product < 1.00001)) return true;
            return false;
        }
    }
}