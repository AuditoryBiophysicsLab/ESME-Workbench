using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace ESME.Environment
{
    public class DataFile
    {
        private const uint _magic = 0x8ef22a9d;
        private DataLayerList layers;

        /// <summary>
        /// Open an existing DataFile
        /// </summary>
        /// <param name="FileName">The filename of the DataFile</param>
        private DataFile(string FileName)
        {
            int layerCount;
            this.FileName = FileName;
            if (!File.Exists(FileName))
                throw new ApplicationException("DataFile: Specified file \"" + FileName + "\" does not exist");

            OpenStreams(FileMode.Open);

            if (_magic != ReadStream.ReadUInt32())
                throw new FormatException("DataFile: Invalid input file format.  Magic number not found.");
            layerCount = ReadStream.ReadUInt16();
            layers = new DataLayerList(this);
            for (int i = 0; i < layerCount; i++)
            {
                DataLayer newLayer = new DataLayer(ReadStream);
                //newLayer.DataArray.DataFile = this;
                layers.Add(newLayer);
            }
            // This goes AFTER we read in the existing layers from the file, so we're notified if the caller should add a new layer
            // to this file
            layers.ListChanged += LayerListChanged;
        }

        /// <summary>
        /// Create a new DataFile with the specified list of DataLayers
        /// </summary>
        /// <param name="Filename">The filename of the DataFile</param>
        /// <param name="Layers">DataLayerList containing a list of layers, or null if no layers are to be added</param>
        private DataFile(string Filename, DataLayerList Layers)
        {
            Create(Filename, Layers);
        }

        private DataFile(string Filename, DataLayer Layer)
        {
            if (Layer == null)
                Create(Filename, null);
            else
                Create(Filename, new DataLayerList { Layer });
        }

        private void Create(string FileName, DataLayerList Layers)
        {
            this.FileName = FileName;
            OpenStreams(FileMode.Create);

            if (Layers == null)
                layers = new DataLayerList(this);
            else
                layers = Layers;
            Save();
            layers.ListChanged += LayerListChanged;
        }

        void LayerListChanged(object sender, EventArgs e)
        {
            if (CanWrite)
                Save();
        }

        private void OpenStreams(FileMode FileMode)
        {
            BaseStream = File.Open(FileName, FileMode, FileAccess.ReadWrite, FileShare.ReadWrite);
            ReadStream = new BinaryReader(BaseStream);
            WriteStream = new BinaryWriter(BaseStream);
        }

        public void Close()
        {
            if (ReadStream != null)
                ReadStream.Close();
            ReadStream = null;

            if (WriteStream != null)
                WriteStream.Close();
            WriteStream = null;

            if (BaseStream != null)
                BaseStream.Close();
            BaseStream = null;
        }

        public static DataFile Create(string Filename)
        {
            return new DataFile(Filename, (DataLayerList)null);
        }

        public static DataFile Open(string Filename)
        {
            return new DataFile(Filename);
        }

        public void Save()
        {
            WriteStream.Seek(0, SeekOrigin.Begin);
            WriteStream.Write(_magic);
            if (layers != null)
            {
                WriteStream.Write((UInt16)layers.Count);
                for (int i = 0; i < layers.Count; i++)
                    layers[i].Save(WriteStream);
            }
            else
            {
                WriteStream.Write((UInt16)0);
            }
        }

        public string FileName { get; private set; }
        internal BinaryWriter WriteStream { get; private set; }
        internal BinaryReader ReadStream { get; private set; }
        internal Stream BaseStream { get; private set; }

        internal bool CanRead { get { return BaseStream.CanRead; } }
        internal bool CanWrite { get { return BaseStream.CanWrite; } }

        public DataLayerList Layers { get { return layers; } }
        public DataLayer this[string name] { get { return layers[name]; } }

        public override string ToString()
        {
            string retval;
            retval = string.Format("DataFile information.\n" +
                                   "       Name: {0}\n" +
                                   "Layer Count: {1}\n", this.FileName, this.Layers.Count);
            foreach (DataLayer layer in Layers)
                retval += layer.ToString();
            return retval;
        }
    }
}
