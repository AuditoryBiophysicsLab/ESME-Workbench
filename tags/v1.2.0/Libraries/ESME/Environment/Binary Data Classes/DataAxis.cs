using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Diagnostics;
using System.Linq;

namespace ESME.Environment
{
    public class DataAxis
    {
        private const uint _magic = 0x1df0f868;
        //private float[] values;
        private List<AxisMap> _axis;

        #region Constructors
        /// <summary>
        /// Construct a DataAxis from an array of axis values.
        /// </summary>
        /// <param name="Name">Name for the new DataAxis</param>
        /// <param name="Values">An array of values to be contained in the new DataAxis.   These values must be in ascending order or an exception will be thrown.</param>
        /// <param name="WrapModulus">On wrappable axes (like longitude), this contains the maximum legal value for the current axis</param>
        public DataAxis(string Name, float[] Values)
        {
            this.Name = Name;
            _axis = new List<AxisMap>(Values.Length);
            for (int i = 0; i < Values.Length; i++)
                _axis.Add(new AxisMap(Values[i], i));
        }

        /// <summary>
        /// Constructs a DataAxis from a stream.  The source of the stream must have previously been created by calling DataAxis.Save()
        /// </summary>
        /// <param name="stream"></param>
        public DataAxis(BinaryReader stream)
        {
            int i;
            
            UInt32 actualMagic = stream.ReadUInt32();
            if (_magic != actualMagic)
                throw new FormatException(string.Format("DataAxis: Invalid input file format.  Magic number {0:x} not found (read {1:x} instead)", _magic, actualMagic));
            Name = stream.ReadString();
            UInt16 axisLength = stream.ReadUInt16();
            _axis = new List<AxisMap>(axisLength);
            for (i = 0; i < axisLength; i++)
                _axis.Add(new AxisMap(stream));
            CheckAxis();
        }

        /// <summary>
        /// Write the DataAxis to a stream
        /// </summary>
        /// <param name="stream"></param>
        public void Save(BinaryWriter stream)
        {
            stream.Write(_magic);
            stream.Write(Name);
            stream.Write((ushort)_axis.Count);
            for (int i = 0; i < _axis.Count; i++)
                _axis[i].Save(stream);
        }

        /// <summary>
        /// Construct a DataAxis from a subset (StartValue to EndValue) of another DataAxis
        /// </summary>
        /// <param name="Source">Existing DataAxis</param>
        /// <param name="StartValue"></param>
        /// <param name="EndValue"></param>
        /// <param name="IncludeEndpoints"></param>
        public DataAxis(DataAxis Source, float StartValue, float EndValue)
        {
            float lower, higher;

            if (!Source.ContainsRange(StartValue, EndValue))
                throw new ArgumentOutOfRangeException("DataAxis: Source axis does not contain the requested range");
            lower = Math.Min(StartValue, EndValue);
            higher = Math.Max(StartValue, EndValue);

            var result = from map in Source._axis
                         where (lower <= map.Value) && (map.Value <= higher)
                         select new AxisMap(map);
            _axis = result.ToList();
            for (int i = 0; i < _axis.Count; i++)
                _axis[i].Index = i;
            _axis.Sort();
            Name = Source.Name;
        }

        public void Normalize()
        {
            float NormalizeFactor = 0;
            foreach (AxisMap cur in _axis)
                if (cur.Value > 180)
                {
                    NormalizeFactor = -360;
                    break;
                }
                else if (cur.Value < -180)
                {
                    NormalizeFactor = 360;
                    break;
                }
            if (NormalizeFactor != 0)
                foreach (AxisMap cur in _axis)
                    cur.Value += NormalizeFactor;
        }

        /// <summary>
        /// Copy constructor.  Copies an existing DataAxis to a new one.
        /// </summary>
        public DataAxis(DataAxis Source)
        {
            _axis = new List<AxisMap>(Source.Map.Length);
            for (int i = 0; i < Source.Map.Length; i++)
                _axis.Add(new AxisMap(Source.Map[i]));
            Name = Source.Name;
        }
        #endregion

        private void CheckAxis()
        {
            int i;

            for (i = 0; i < _axis.Count - 2; i++)
                if (_axis[i].Value >= _axis[i + 1].Value)
                    throw new ApplicationException("Axis: Axis values must be in ascending order");
        }

        /// <summary>
        /// Check if the DataAxis contains the requested range
        /// </summary>
        /// <param name="Value1">One end of the range to be tested for</param>
        /// <param name="Value2">The other end of the range to be tested for</param>
        /// <returns>true if this DataAxis contains the requested range</returns>
        public bool ContainsRange(float Value1, float Value2)
        {
            float lower, higher;

            lower = Math.Min(Value1, Value2);
            higher = Math.Max(Value1, Value2);
            if ((_axis.Find(x => x.Value <= lower) != null) &&
                (_axis.Find(x => x.Value >= higher) != null))
                return true;
            return false;
        }

        /// <summary>
        /// Get the axis value at the specified index
        /// </summary>
        /// <param name="index"></param>
        /// <returns></returns>
        public float this[int index] { get { return _axis[index].Value; } }
        public int this[float value] { get { return _axis.Find(x => x.Value >= value).Index; } }

        public AxisMap[] Map
        {
            get
            {
                return _axis.ToArray();
            }
        }

        public List<AxisMap> MapList
        {
            get
            {
                return _axis;
            }
        }
        /// <summary>
        /// Return the values in the axis
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

        
        /// <summary>
        /// Return the values in the axis as an array of doubles
        /// </summary>
        public double[] DoubleValues
        {
            get
            {
                var result = from m in _axis
                             select (double)m.Value;
                return result.ToArray();
            }
        }

        public double[] UnwrappedValues
        {
            get
            {
                List<double> results = new List<double>();
                var Found = from m in _axis 
                            where ((m.Value >= -180.0) && (m.Value < 180.0)) 
                            orderby m.Index ascending 
                            select new { m.Index, m.Value };
                foreach (var f in Found)
                    results.Add(f.Value);
                return results.ToArray();
            }
        }

        /// <summary>
        /// Get the number of wrapped elements in the DataAxis (may be more than the number of actual elements)
        /// </summary>
        public int Length
        {
            get
            {
                if (_axis == null)
                    return 0;
                return _axis.Count;
            }
        }

        /// <summary>
        /// Get the name of the DataAxis
        /// </summary>
        public string Name { get; set; }

        public override string ToString()
        {
            return string.Format("Name             : {0}\n" +
                                 "                                 Length : {1}\n" +
                                 "                       Min Actual Value : {2}\n" +
                                 "                       Min Actual Index : {3}\n" +
                                 "                       Max Actual Value : {4}\n" +
                                 "                       Max Actual Index : {5}",
                                 Name.ToString(), Length, _axis[0].Value, _axis[0].Index, _axis.Last().Value, _axis.Last().Index);
        }
    }

    [DebuggerDisplay("(Value = {Value}, Index = {Index})")]
    public class AxisMap : IComparable<AxisMap>, IEquatable<AxisMap>, IEquatable<float>, IEqualityComparer<AxisMap>
    {
        public float Value { get; set; }
        public int Index { get; set; }

        public AxisMap(float Value, int Index)
        {
            this.Value = Value;
            this.Index = Index;
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
        
        public int CompareTo(AxisMap Other)
        {
            return Value.CompareTo(Other.Value);
        }

        public override string ToString()
        {
            return string.Format("({0}, {1})", Value, Index);
        }

        public bool Equals(AxisMap the_one, AxisMap the_other)
        {
            return the_one.Equals(the_other);
        }

        public int GetHashCode(AxisMap thing)
        {
            return thing.Value.GetHashCode();
        }

        public bool Equals(AxisMap other)
        {
            double product = (double)Value / (double)(other.Value);
            if ((0.99999 <= product) && (product < 1.00001))
                return true;
            return false;
        }

        public bool Equals(float other)
        {
            double product = (double)Value / (double)other;
            if ((0.99999 <= product) && (product < 1.00001))
                return true;
            return false;
        }
    }
}
