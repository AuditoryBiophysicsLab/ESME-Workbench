using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Xml;

namespace ESMEWorkbench.ViewModels.RecentFiles
{
    class XmlPersister : IPersist
    {
        public XmlPersister()
        {
            Filepath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), ApplicationAttributes.CompanyName + "\\" + ApplicationAttributes.ProductName + "\\" + "RecentFileList.xml");
        }

        public XmlPersister(string filepath)
        {
            Filepath = filepath;
        }

        public XmlPersister(Stream stream)
        {
            Stream = stream;
        }

        string Filepath { get; set; }
        Stream Stream { get; set; }

        #region IPersist Members

        public List<string> RecentFiles(int max)
        {
            return Load(max);
        }

        public void InsertFile(string filepath, int max)
        {
            Update(filepath, true, max);
        }

        public void RemoveFile(string filepath, int max)
        {
            Update(filepath, false, max);
        }

        #endregion

        void Update(string filepath, bool insert, int max)
        {
            var old = Load(max);

            var list = new List<string>(old.Count + 1);

            if (insert) list.Add(filepath);

            CopyExcluding(old, filepath, list, max);

            Save(list);
        }

        static void CopyExcluding(IEnumerable<string> source, string exclude, ICollection<string> target, int max)
        {
            foreach (var s in from s in source where !String.IsNullOrEmpty(s) where !s.Equals(exclude, StringComparison.OrdinalIgnoreCase) where target.Count < max select s)
                target.Add(s);
        }

        SmartStream OpenStream(FileMode mode)
        {
            return !String.IsNullOrEmpty(Filepath) ? new SmartStream(Filepath, mode) : new SmartStream(Stream);
        }

        List<string> Load(int max)
        {
            var list = new List<string>(max);

            using (var ms = new MemoryStream())
            {
                using (var ss = OpenStream(FileMode.OpenOrCreate))
                {
                    if (ss.Stream.Length == 0) return list;

                    ss.Stream.Position = 0;

                    var buffer = new byte[1 << 20];
                    for (;;)
                    {
                        var bytes = ss.Stream.Read(buffer, 0, buffer.Length);
                        if (bytes == 0) break;
                        ms.Write(buffer, 0, bytes);
                    }

                    ms.Position = 0;
                }

                XmlTextReader x = null;

                try
                {
                    x = new XmlTextReader(ms);

                    while (x.Read())
                    {
                        switch (x.NodeType)
                        {
                            case XmlNodeType.XmlDeclaration:
                            case XmlNodeType.Whitespace:
                                break;

                            case XmlNodeType.Element:
                                switch (x.Name)
                                {
                                    case "RecentFiles":
                                        break;

                                    case "RecentFile":
                                        if (list.Count < max) list.Add(x.GetAttribute(0));
                                        break;

                                    default:
                                        Debug.Assert(false);
                                        break;
                                }
                                break;

                            case XmlNodeType.EndElement:
                                switch (x.Name)
                                {
                                    case "RecentFiles":
                                        return list;
                                    default:
                                        Debug.Assert(false);
                                        break;
                                }
                                break;

                            default:
                                Debug.Assert(false);
                                break;
                        }
                    }
                }
                finally
                {
                    if (x != null) x.Close();
                }
            }
            return list;
        }

        void Save(IEnumerable<string> list)
        {
            using (var ms = new MemoryStream())
            {
                XmlTextWriter x = null;

                try
                {
                    x = new XmlTextWriter(ms, Encoding.UTF8) {Formatting = Formatting.Indented};

                    x.WriteStartDocument();

                    x.WriteStartElement("RecentFiles");

                    foreach (string filepath in list)
                    {
                        x.WriteStartElement("RecentFile");
                        x.WriteAttributeString("Filepath", filepath);
                        x.WriteEndElement();
                    }

                    x.WriteEndElement();

                    x.WriteEndDocument();

                    x.Flush();

                    using (SmartStream ss = OpenStream(FileMode.Create))
                    {
                        ss.Stream.SetLength(0);

                        ms.Position = 0;

                        var buffer = new byte[1 << 20];
                        for (;;)
                        {
                            int bytes = ms.Read(buffer, 0, buffer.Length);
                            if (bytes == 0) break;
                            ss.Stream.Write(buffer, 0, bytes);
                        }
                    }
                }
                finally
                {
                    if (x != null) x.Close();
                }
            }
        }

        #region Nested type: SmartStream

        class SmartStream : IDisposable
        {
            readonly bool _isStreamOwned = true;
            Stream _stream;

            public SmartStream(string filepath, FileMode mode)
            {
                _isStreamOwned = true;

                Directory.CreateDirectory(Path.GetDirectoryName(filepath));

                _stream = File.Open(filepath, mode);
            }

            public SmartStream(Stream stream)
            {
                _isStreamOwned = false;
                _stream = stream;
            }

            public Stream Stream
            {
                get { return _stream; }
            }

            #region IDisposable Members

            public void Dispose()
            {
                if (_isStreamOwned && _stream != null) _stream.Dispose();

                _stream = null;
            }

            #endregion

            public static implicit operator Stream(SmartStream me)
            {
                return me.Stream;
            }
        }

        #endregion
    }
}