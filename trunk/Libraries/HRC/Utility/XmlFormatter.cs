using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.Serialization;
using System.Text;
using System.Xml;

namespace HRC.Utility
{
    /// <summary>
    ///   The <see cref = "XmlFormatter" /> class implements a custom XmlFormatter
    ///   which uses the <see cref = "ISerializable" /> interace. 
    ///   The class implements the <see cref = "IFormatter" /> interface to serialize
    ///   and deserialize the object to an XML representation.
    /// </summary>
    /// <remarks>
    ///   The class calls the methods of ISerializable on the object if the object
    ///   supports this interface. If not, the class will use Reflection to examine the public
    ///   fields and properties of the object.<br />
    ///   When adding objects that inherit or implement IList, ICollection, the 
    ///   elements of the list should be passed as an array to <see cref = "SerializationInfo" />.
    /// </remarks>
    public sealed class XmlFormatter : IFormatter
    {
        #region [-- Class members --]

        /// <summary>
        ///   The context in which this formatter performs his work.
        /// </summary>
        StreamingContext _context = new StreamingContext(StreamingContextStates.Persistence);

        /// <summary>
        ///   The serialization binder used to map object types to names.
        /// </summary>
        SerializationBinder _binder;

        /// <summary>
        ///   The System.Type to serialize or deserialize.
        /// </summary>
        Type _type;

        /// <summary>
        ///   Contains a list with objects that implement the IDeserializationCallback interface.
        /// </summary>
        List<object> _deserializationCallbackList;

        #endregion

        #region [-- Properties --]

        #region IFormatter Members

        /// <summary>
        ///   Gets or sets the type binder.
        /// </summary>
        public SerializationBinder Binder
        {
            get { return _binder ?? (_binder = new CustomBinder()); }
            set { _binder = value; }
        }

        /// <summary>
        ///   Gets or sets the StreamingContext.
        /// </summary>
        public StreamingContext Context
        {
            get { return _context; }
            set { _context = value; }
        }

        /// <summary>
        ///   Gets or sets the SurrogateSelector.
        /// </summary>
        public ISurrogateSelector SurrogateSelector { get; set; }

        #endregion

        List<object> DeserializationCallBackList
        {
            get { return _deserializationCallbackList ?? (_deserializationCallbackList = new List<object>()); }
        }

        #endregion

        #region [-- Constructors --]

        public XmlFormatter(Type objectType) { _type = objectType; }

        #endregion

        #region [-- Public Methods --]

        #region Serialization

        /// <summary>
        ///   Serializes the passed object to the passed stream.
        /// </summary>
        /// <param name = "serializationStream">The stream to serialize to.</param>
        /// <param name = "objectToSerialize">The object to serialize.</param>
        public void Serialize(Stream serializationStream, object objectToSerialize)
        {
            if (objectToSerialize == null) return;

            if (serializationStream == null) throw new ArgumentException("Empty serializationStream!");

            var writer = new XmlTextWriter(serializationStream, Encoding.UTF8);

            Serialize(writer, new FormatterConverter(), objectToSerialize.GetType().Name, objectToSerialize, objectToSerialize.GetType());
        }

        #endregion

        #region Deserialization

        /// <summary>
        ///   Deserializes an object from the passed stream.
        /// </summary>
        /// <param name = "serializationStream">The stream to deserialize the object from.</param>
        /// <returns>The deserialized object.</returns>
        public object Deserialize(Stream serializationStream)
        {
            if (_type == null) throw new InvalidOperationException("Type property not initialized");

            return Deserialize(serializationStream, _type);
        }

        #endregion

        #endregion

        #region [-- Private Methods --]

        #region Serialization

        /// <summary>
        ///   Serializes the object using the passed XmlWriter.
        /// </summary>
        /// <param name = "writer">The XmlWriter to write to.</param>
        /// <param name = "converter">The converter to use when converting simple types.</param>
        /// <param name = "elementName">The name of the element in the Xml.</param>
        /// <param name = "objectToSerialize">The object to serialize.</param>
        /// <param name = "objectType">The type of the object.</param>
        void Serialize(XmlWriter writer, IFormatterConverter converter, string elementName, object objectToSerialize, Type objectType)
        {
            // Include type information when using ISerializable.
            var includeTypeInfo = (objectToSerialize is ISerializable);

            // write the opening tag
            writer.WriteStartElement(elementName);

            // If the name of the tag differs from the type name, include type information.
            if (elementName != objectType.Name || includeTypeInfo)
            {
                WriteAttributes(writer, objectType, false);
            }

            // for each serializable item in this object
            foreach (var entry in GetMemberInfo(objectToSerialize, objectType, converter))
            {
                if (entry.ObjectType.IsPrimitive || entry.ObjectType == typeof (string) || entry.ObjectType.IsEnum || entry.ObjectType == typeof (DateTime))
                {
                    // simple type, directly write the value.
                    WriteValueElement(writer, entry);
                }
                else if (entry.ObjectType == typeof (Array) || entry.ObjectType.IsSubclassOf(typeof (Array)))
                {
                    // the type is an array type. iterate through the members
                    // get the type of the elements in the array
                    var enumeratedType = entry.ObjectType.GetElementType();
                    // write the opening tag.
                    writer.WriteStartElement(entry.Name);
                    // write the attributes.
                    WriteAttributes(writer, enumeratedType, true);

                    // if the array is an null value, skip loop
                    if (entry.Value != null)
                    {
                        foreach (var item in ((Array) entry.Value))
                        {
                            if (enumeratedType.IsPrimitive || enumeratedType == typeof (string))
                            {
                                // write the element
                                WriteValueElement("Value", writer, entry);
                            }
                            else
                            {
                                // serialize the object (recursive call)
                                Serialize(writer, converter, enumeratedType.Name, item, item.GetType());
                            }
                        }
                    }
                    // write closing tag
                    writer.WriteEndElement();
                }
                else if (entry.IsList)
                {
                    // write the opening tag
                    writer.WriteStartElement(entry.Name);

                    if (includeTypeInfo)
                    {
                        // write the attributes
                        WriteAttributes(writer, entry.ObjectType, false);
                    }

                    var items = (IList) entry.Value;

                    // loop through the list
                    foreach (var item in items)
                    {
                        // serialize the object (recursive call)
                        Serialize(writer, converter, entry.ObjectType.GetElementType().Name, item, entry.ObjectType);
                    }
                    // write the closing tag
                    writer.WriteEndElement();
                }
                else
                {
                    // serialize the object (recursive call)
                    Serialize(writer, converter, entry.Name, entry.Value, entry.ObjectType);
                }
            }
            // write the closing tag
            writer.WriteEndElement();
            // flush the contents of the writer to the stream
            writer.Flush();
        }

        /// <summary>
        ///   Writes the Type and includeArrayAttribute attributes to the element
        /// </summary>
        /// <param name = "writer">The XmlWriter to write to.</param>
        /// <param name = "objectType">The type of the object.</param>
        /// <param name = "includeArrayAttribute">Indicates whether to write the isArray attribute.</param>
        static void WriteAttributes(XmlWriter writer, Type objectType, bool includeArrayAttribute)
        {
            writer.WriteStartAttribute("type", "http://www.w3.org/2001/XMLSchema-instance");

            if (objectType.IsPrimitive || objectType == typeof (string)) writer.WriteString(objectType.FullName);
            else writer.WriteString(objectType.Name);

            writer.WriteEndAttribute();

            if (includeArrayAttribute)
            {
                writer.WriteStartAttribute("includeArrayAttribute", "http://www.w3.org/2001/XMLSchema-instance");
                writer.WriteString("true");
                writer.WriteEndAttribute();
            }
        }

        /// <summary>
        ///   Writes a simple element to the writer. The name of the element is the name of the object type.
        /// </summary>
        /// <param name = "writer">The XmlWriter to write to.</param>
        /// <param name = "entry">The entry to write to the element.</param>
        static void WriteValueElement(XmlWriter writer, CustomSerializationEntry entry) { WriteValueElement(entry.Name, writer, entry); }

        /// <summary>
        ///   Writes a simple element to the writer.
        /// </summary>
        /// <param name = "tagName">The name of the tag to write.</param>
        /// <param name = "writer">The XmlWriter to write to.</param>
        /// <param name = "entry">The entry to write to the element.</param>
        static void WriteValueElement(string tagName, XmlWriter writer, CustomSerializationEntry entry)
        {
            // write opening tag
            writer.WriteStartElement(tagName);

            if (entry.ObjectType.IsEnum)
            {
                writer.WriteValue(((int) entry.Value).ToString());
            }
            else
            {
                writer.WriteValue(entry.Value);
            }

            // write closing tag
            writer.WriteEndElement();
        }

        /// <summary>
        ///   Gets all the serializable members of an object and return an enumarable collection.
        /// </summary>
        /// <param name = "objectToSerialize">The object to get the members from.</param>
        /// <param name = "objectToSerializeType">The type of the object.</param>
        /// <param name = "converter">The converter to use when converting simple types.</param>
        /// <returns>An IEnumerable list of <see cref = "CustomSerializationEntry" /> entries.</returns>
        IEnumerable<CustomSerializationEntry> GetMemberInfo(object objectToSerialize, Type objectToSerializeType, IFormatterConverter converter)
        {
            ISurrogateSelector selector1;
            ISerializationSurrogate serializationSurrogate;
            SerializationInfo info = null;
            var objectType = objectToSerializeType;

            // if the passed object is null, break the iteration.
            if (objectToSerialize == null) yield break;

            if ((SurrogateSelector != null) && ((serializationSurrogate = SurrogateSelector.GetSurrogate(objectType, Context, out selector1)) != null))
            {
                // use a surrogate to get the members.
                info = new SerializationInfo(objectType, converter);

                if (!objectType.IsPrimitive)
                {
                    // get the data from the surrogate.
                    serializationSurrogate.GetObjectData(objectToSerialize, info, Context);
                }
            }
            else if (objectToSerialize is ISerializable)
            {
                // object implements ISerializable
                if (!objectType.IsSerializable)
                {
                    throw new SerializationException(string.Format("Type is not serializable : {0}.", objectType.FullName));
                }

                info = new SerializationInfo(objectType, converter);
                // get the data using ISerializable.
                ((ISerializable) objectToSerialize).GetObjectData(info, Context);
            }

            if (info != null)
            {
                // either the surrogate provided the members, or the members were retrieved 
                // using ISerializable.
                // create the custom entries collection by copying all the members
                foreach (var member in info)
                {
                    var entry = new CustomSerializationEntry(member.Name, member.ObjectType, false, member.Value);
                    // yield return will return the entry now and return to this point when
                    // the enclosing loop (the one that contains the call to this method)
                    // request the next item.
                    yield return entry;
                }
            }
            else
            {
                // The item does not hava surrogate, not does it implement ISerializable.
                // We use reflection to get the objects state.
                if (!objectType.IsSerializable)
                {
                    throw new SerializationException(string.Format("Type is not serializable : {0}.", objectType.FullName));
                }
                // Get all serializable members
                var members = FormatterServices.GetSerializableMembers(objectType, Context);

                foreach (var member in members)
                {
                    if (CanSerialize(member))
                    {
                        // create the entry
                        var entry = new CustomSerializationEntry(member.Name, member.ReflectedType, false);

                        if (typeof (IList).IsAssignableFrom(entry.ObjectType))
                        {
                            entry.IsList = true;
                        }
                        // get the value of the member
                        entry.Value = GetMemberValue(objectToSerialize, member);
                        // yield return will return the entry now and return to this point when
                        // the enclosing loop (the one that contains the call to this method)
                        // request the next item.
                        yield return entry;
                    }
                }
            }
        }

        /// <summary>
        ///   Determines if the passed member is public and writable.
        /// </summary>
        /// <param name = "member">The member to investigate.</param>
        /// <returns>True if public and writable, False if not.</returns>
        static bool CanSerialize(MemberInfo member)
        {
            if (member.MemberType == MemberTypes.Field)
            {
                // if the member is a field, the member is writable when public.
                var field = member as FieldInfo;

                    if (field != null && field.IsPublic && !field.IsStatic)
                    {
                        return true;
                    }
            }
            else if (member.MemberType == MemberTypes.Property)
            {
                // if the member is a property, the member is writable when public set methods exist.
                var property = member as PropertyInfo;

                if (property != null && property.CanRead && property.CanWrite && property.GetGetMethod().IsPublic && !property.GetGetMethod().IsStatic && property.GetSetMethod().IsPublic && !property.GetSetMethod().IsStatic)
                {
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        ///   Get the value of a member.
        /// </summary>
        /// <param name = "item">The item to get the member from.</param>
        /// <param name = "member">The member to get the value from.</param>
        /// <returns>The value of the member.</returns>
        static object GetMemberValue(object item, MemberInfo member)
        {
            switch (member.MemberType)
            {
                case MemberTypes.Property:
                {
                    return item.GetType().GetProperty(member.Name).GetValue(item, null);
                }
                case MemberTypes.Field:
                {
                    return item.GetType().GetField(member.Name).GetValue(item);
                }

                default:
                {
                    throw new NotSupportedException(string.Format("Member cannot be serialized. {0}", member.MemberType));
                }
            }
        }

        #endregion

        #region Deserialization

        /// <summary>
        ///   Deserializes an object from the given stream for the given type.
        /// </summary>
        /// <param name = "serializationStream">The stream to read the object from.</param>
        /// <param name = "objectType">The type of object to create.</param>
        /// <returns>The deserialized object.</returns>
        object Deserialize(Stream serializationStream, Type objectType)
        {
            if (_type == null) _type = objectType;

            object deserialized;

            // create xml reader from stream
            using (var reader = new XmlTextReader(serializationStream))
            {
                DeserializationCallBackList.Clear();
                deserialized = InitializeObject(reader, new FormatterConverter(), objectType);
            }

            foreach (IDeserializationCallback callBack in DeserializationCallBackList)
            {
                callBack.OnDeserialization(null);
            }

            return deserialized;
        }

        /// <summary>
        ///   Reads an object from the XML and initializes the object.
        /// </summary>
        /// <param name = "reader">The XmlReader to read from.</param>
        /// <param name = "converter">The converter used to parse the values from the XML.</param>
        /// <param name = "objectType">The type of the object to create.</param>
        /// <returns>The recreated object.</returns>
        object InitializeObject(XmlTextReader reader, FormatterConverter converter, Type objectType)
        {
            Type actualType = null;
            ISurrogateSelector selector1;
            ISerializationSurrogate serializationSurrogate;
            SerializationInfo info;
            object initializedObject = null;

            // check if a type attribute is present
            if (reader.HasAttributes)
            {
                // if so, get the type
                var actualTypeName = reader.GetAttribute("type", "http://www.w3.org/2001/XMLSchema-instance");
                if (actualTypeName != null) actualType = Binder.BindToType("", actualTypeName);
            }
            else
            {
                // passed type is actual type.
                actualType = objectType;
            }

            if (actualType == null) throw new NullReferenceException("actualType cannot be null!");

            // check whether a surrogate should be used, iserializable is implemented or reflection is needed.
            if ((SurrogateSelector != null) && ((serializationSurrogate = SurrogateSelector.GetSurrogate(actualType, Context, out selector1)) != null))
            {
                // use surrogate
                info = new SerializationInfo(actualType, converter);

                if (!actualType.IsPrimitive)
                {
                    // create a instance of the type.
                    initializedObject = FormatterServices.GetUninitializedObject(actualType);

                    // read the first element
                    reader.ReadStartElement();

                    while (reader.IsStartElement())
                    {
                        // determine type
                        var typeName = reader.GetAttribute("type", "http://www.w3.org/2001/XMLSchema-instance");
                        if (typeName != null) 
                        {
                            var type = Binder.BindToType("", typeName);
                            // using ISerializable
                            info.AddValue(reader.Name, DetermineValue(reader, converter, type));
                        }
                        reader.ReadEndElement();
                    }
                    // use the surrogate to populate the instance
                    initializedObject = serializationSurrogate.SetObjectData(initializedObject, info, Context, SurrogateSelector);
                }
            }
            else if (typeof (ISerializable).IsAssignableFrom(actualType))
            {
                // The item implements ISerializable. Create a SerializationInfo object
                info = new SerializationInfo(actualType, converter);
                // Populate the collection
                PopulateSerializationInfo(reader, converter, info);
                // Get the specialized Serialization Constructor
                var ctor = actualType.GetConstructor(new[]
                                                     {
                                                         typeof (SerializationInfo), typeof (StreamingContext)
                                                     });
                // Create the object
                initializedObject = ctor.Invoke(new object[]
                                                {
                                                    info, Context
                                                });
            }
            else
            {
                // The item does not implement ISerializable. Use  reflection to get public 
                // fields and properties.
                initializedObject = FormatterServices.GetUninitializedObject(actualType);

                var memberList = new List<MemberInfo>();
                var valuesList = new List<object>();
                // read the first element.
                reader.ReadStartElement();

                while (reader.IsStartElement())
                {
                    // Get public fields and members of this type.
                    var possibleMembers = actualType.GetMember(reader.Name, MemberTypes.Property | MemberTypes.Field, BindingFlags.Public | BindingFlags.DeclaredOnly | BindingFlags.Instance);

                    if (possibleMembers.Length > 1) throw new SerializationException(string.Format("More than one member found for tag {0}", reader.Name));

                    if (typeof (IList).IsAssignableFrom(possibleMembers[0].ReflectedType))
                    {
                        // the type is a list, get the list from the initialized object.
                        var list = GetMemberValue(initializedObject, possibleMembers[0]) as IList;

                        if (list == null) throw new SerializationException(string.Format("List in object is null. {0}, member {1}", possibleMembers[0].DeclaringType.FullName, possibleMembers[0].Name));

                        // read the next element
                        reader.ReadStartElement();

                        while (reader.IsStartElement())
                        {
                            if (!reader.IsEmptyElement)
                            {
                                // Initialize the object (recursive call)
                                var listItem = InitializeObject(reader, converter, possibleMembers[0].ReflectedType.GetElementType());
                                list.Add(listItem);
                                reader.ReadEndElement();
                            }
                            else
                            {
                                reader.ReadStartElement();
                            }
                        }
                    }
                    else
                    {
                        // determine the value.
                        var value = DetermineValue(reader, converter, possibleMembers[0].ReflectedType);
                        memberList.Add(possibleMembers[0]);
                        valuesList.Add(value);
                    }
                }
                if (memberList.Count > 0)
                {
                    initializedObject = FormatterServices.PopulateObjectMembers(initializedObject, memberList.ToArray(), valuesList.ToArray());
                }
                reader.ReadEndElement();
            }

            if ((initializedObject as IDeserializationCallback) != null)
            {
                DeserializationCallBackList.Add(initializedObject);
            }
            return initializedObject;
        }

        /// <summary>
        ///   Populates the serialized members in the SerializationInfo.
        /// </summary>
        /// <param name = "reader">The XmlReader to read from.</param>
        /// <param name = "converter">The converter used to parse the values from the XML.</param>
        /// <param name = "info">The object to populate.</param>
        void PopulateSerializationInfo(XmlTextReader reader, FormatterConverter converter, SerializationInfo info)
        {
            // read the next element.
            reader.ReadStartElement();

            while (reader.IsStartElement())
            {
                // determine type
                var typeName = reader.GetAttribute("type", "http://www.w3.org/2001/XMLSchema-instance");
                var isArray = (reader.GetAttribute("includeArrayAttribute", "http://www.w3.org/2001/XMLSchema-instance") == "true") ? true : false;

                // If type is found in attribute, get the System.Type by using the Binder.
                var type = typeName != null ? Binder.BindToType("", typeName) : typeof (object);
                if (type == null) throw new NullReferenceException("type cannot be null");

                if (reader.IsEmptyElement)
                {
                    // if the type is an array type, place a empty array in the collection.
                    if (isArray)
                        info.AddValue(reader.Name, type.MakeArrayType().GetConstructor(new[]
                                                                                       {
                                                                                           typeof (Int32)
                                                                                       }).Invoke(new object[]
                                                                                                 {
                                                                                                     0
                                                                                                 }));
                    else info.AddValue(reader.Name, null);

                    reader.ReadStartElement();
                }
                else
                {
                    if (isArray)
                    {
                        // Item found is an array type.
                        var name = reader.Name;
                        // create a list of the type.
                        var list = (IList) typeof (List<>).MakeGenericType(new[]
                                                                           {
                                                                               type
                                                                           }).GetConstructor(Type.EmptyTypes).Invoke(null);
                        // read the next element.
                        reader.ReadStartElement();

                        while (reader.IsStartElement())
                        {
                            if (!reader.IsEmptyElement)
                            {
                                // determine value
                                list.Add(DetermineValue(reader, converter, type));
                                reader.ReadEndElement();
                            }
                            else
                            {
                                reader.ReadStartElement();
                            }
                        }
                        // create an array of the element type and copy the list to the array.
                        var array = (Array) type.MakeArrayType().GetConstructor(new[]
                                                                                {
                                                                                    typeof (Int32)
                                                                                }).Invoke(new object[]
                                                                                          {
                                                                                              list.Count
                                                                                          });
                        list.CopyTo(array, 0);
                        // add the array to the collection.
                        info.AddValue(name, array, type.MakeArrayType());

                        if (!reader.IsEmptyElement) reader.ReadEndElement();
                        else reader.ReadStartElement();
                    }
                    else
                    {
                        // type if not an array type.
                        // determine value and add it to the collection
                        info.AddValue(reader.Name, DetermineValue(reader, converter, type));
                        reader.ReadEndElement();
                    }
                }
            }
        }

        /// <summary>
        ///   Determines the value of an object.
        /// </summary>
        /// <param name = "reader">The XML reader the read from.</param>
        /// <param name = "converter">The converter used to parse the values from the XML.</param>
        /// <param name = "objectType">The type of the object to create.</param>
        /// <returns>The value of the object.</returns>
        object DetermineValue(XmlTextReader reader, FormatterConverter converter, Type objectType)
        {
            object parsedObject;

            // check if the value can be directly determined or that the type is a complex type.
            if (objectType.IsPrimitive || objectType == typeof (string) || objectType.IsEnum || objectType == typeof (DateTime) || objectType == typeof (object))
            {
                // directly parse
                parsedObject = converter.Convert(reader.ReadString(), objectType);
            }
            else
            {
                // Initialize the object (recursive call)
                parsedObject = InitializeObject(reader, converter, objectType);
            }

            return parsedObject;
        }

        #endregion

        #endregion

        #region CustomSerializationEntry Struct

        /// <summary>
        ///   The <see cref = "CustomSerializationEntry" /> mimics the <see cref = "System.Runtime.Serialization.SerializationEntry" /> class
        ///   to make it possible to create are own entries. The class acts as a placeholder for a type,
        ///   it's name and it's value. This class is used in the <see cref = "XmlFormatter" /> class to 
        ///   serialize objects.
        /// </summary>
        public struct CustomSerializationEntry
        {
            #region [-- Class members --]

            /// <summary>
            ///   The name of the object.
            /// </summary>
            readonly string _name;

            /// <summary>
            ///   The type of the object.
            /// </summary>
            readonly Type _objectType;

            /// <summary>
            ///   The value of the object.
            /// </summary>
            object _value;

            /// <summary>
            ///   Indicates whether the object is a list.
            /// </summary>
            bool _isList;

            #endregion

            #region [-- Properties --]

            /// <summary>
            ///   Gets the name of the object.
            /// </summary>
            public string Name
            {
                get { return _name; }
            }

            /// <summary>
            ///   Gets the System.Type of the object.
            /// </summary>
            public Type ObjectType
            {
                get { return _objectType; }
            }

            /// <summary>
            ///   Gets or sets the value contained in the object.
            /// </summary>
            public object Value
            {
                get { return _value; }
                set { _value = value; }
            }

            /// <summary>
            ///   Gets or sets whether the object is a list type.
            /// </summary>
            public bool IsList
            {
                get { return _isList; }
                set { _isList = value; }
            }

            #endregion

            #region [-- Constructors --]

            /// <summary>
            ///   Constructor to create a <see cref = "CustomSerializationEntry" /> without a value. 
            ///   Value is set to null.
            /// </summary>
            /// <param name = "name">The name of the object.</param>
            /// <param name = "objectType">The System.Type of the object.</param>
            /// <param name = "isList">Indicates whether the object is a list type.</param>
            public CustomSerializationEntry(string name, Type objectType, bool isList) : this(name, objectType, isList, null) { }

            /// <summary>
            ///   Constructor to create a <see cref = "CustomSerializationEntry" />.
            /// </summary>
            /// <param name = "name">The name of the object.</param>
            /// <param name = "objectType">The System.Type of the object.</param>
            /// <param name = "isList">Indicates whether the object is a list type.</param>
            /// <param name = "value">The value of the object.</param>
            public CustomSerializationEntry(string name, Type objectType, bool isList, object value)
            {
                _name = name;
                _objectType = objectType;
                _isList = isList;
                _value = value;
            }

            #endregion
        }

        #endregion

        #region Binder class

        /// <summary>
        ///   The <see cref = "CustomBinder" /> class performs the mapping to types
        ///   declared in this assembly. It accumulates all types defined in the assembly
        ///   this class is defined in. Optionally, an assembly can be passed as an argument.
        /// </summary>
        public sealed class CustomBinder : SerializationBinder
        {
            #region [-- Class members --]

            /// <summary>
            ///   The list that holds the types and type names contained in the assembly.
            /// </summary>
            static readonly Dictionary<string, Type> TypeList;

            #endregion

            #region [-- Constructors --]

            /// <summary>
            ///   Static constructor to load a list of types contained in the assembly
            ///   only once (during the first call).
            /// </summary>
            static CustomBinder() { TypeList = LoadTypes(typeof (CustomBinder).Assembly); }

            #endregion

            #region [-- Methods --]

            /// <summary>
            ///   Loads the types from the passed assembly in the list. The key of the list
            ///   is the simple name of the type.
            /// </summary>
            /// <param name = "assembly"></param>
            /// <remarks>
            ///   Because the list uses the simple name of the type, it should be unique within the 
            ///   assembly. Otherwise, a <see cref = "System.ArgumentException" /> is thrown.
            /// </remarks>
            /// <returns>A Dictionary object.</returns>
            static Dictionary<string, Type> LoadTypes(Assembly assembly)
            {
                return assembly.GetTypes().ToDictionary(type => type.Name);
            }

            /// <summary>
            ///   Binds the passed typename to the type contained in the dictionary.
            /// </summary>
            /// <param name = "assemblyName">The assembly to load the type from.</param>
            /// <param name = "typeName">The simple name of the type.</param>
            /// <remarks>
            ///   When the passed type is not found in the assembly, the method will try 
            ///   to get the Type using System.GetType. If still not found, the method will
            ///   return typeof (Object).
            /// </remarks>
            /// <returns>The Type reference of the type name.</returns>
            public override Type BindToType(string assemblyName, string typeName)
            {
                Dictionary<string, Type> typeList;

                if (assemblyName.Length > 0)
                {
                    var assembly = Assembly.Load(assemblyName);
                    typeList = LoadTypes(assembly);
                }
                else
                {
                    typeList = TypeList;
                }

                if (typeList.ContainsKey(typeName)) return typeList[typeName];
                var type = Type.GetType(typeName);
                return type ?? typeof (object);
            }

            #endregion
        }

        #endregion
    }
}