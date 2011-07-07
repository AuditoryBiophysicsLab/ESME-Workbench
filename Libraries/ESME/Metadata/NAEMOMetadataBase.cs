﻿using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using Cinch;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Metadata
{
    public class NAEMOMetadataBase : PropertyChangedBase
    {
        protected static readonly List<Type> ReferencedTypes = new List<Type>{typeof(string), typeof(DateTime)};

        public NAEMOMetadataBase()
        {
            Creator = System.Environment.UserName;
            CreationDateTime = DateTime.Now;
        }

        protected static string MetadataFilename(string sourceFilename)
        {
            var metadataPath = Path.GetDirectoryName(sourceFilename);
            var metadataFile = Path.GetFileNameWithoutExtension(sourceFilename);
            return Path.Combine(metadataPath, metadataFile + ".xml");
        }

        public static T Load<T>(string sourceFilename) where T: NAEMOMetadataBase, new()
        {
            var metaDataFilename = MetadataFilename(sourceFilename);

            var result = XmlSerializer<T>.Load(metaDataFilename, ReferencedTypes);
            result.Filename = metaDataFilename;
            return result;
        }

        public virtual void Save<T>(T data, string filename = null) where T : NAEMOMetadataBase, new()
        {
            if (string.IsNullOrEmpty(filename)) filename = Filename;
            var serializer = new XmlSerializer<T> { Data = data };
            serializer.Save(filename, ReferencedTypes);
        }

        #region public string Creator { get; set; }

        public string Creator
        {
            get { return _creator; }
            set
            {
                if (_creator == value) return;
                _creator = value;
                NotifyPropertyChanged(CreatorChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CreatorChangedEventArgs = ObservableHelper.CreateArgs<NAEMOMetadataBase>(x => x.Creator);
        string _creator;

        #endregion

        #region public DateTime CreationDateTime { get; set; }

        public DateTime CreationDateTime
        {
            get { return _creationDateTime; }
            set
            {
                if (_creationDateTime == value) return;
                _creationDateTime = value;
                NotifyPropertyChanged(CreationDateChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs CreationDateChangedEventArgs = ObservableHelper.CreateArgs<NAEMOMetadataBase>(x => x.CreationDateTime);
        DateTime _creationDateTime;

        #endregion

        #region public GeoRect Bounds { get; set; }

        public GeoRect Bounds
        {
            get { return _bounds; }
            set
            {
                if (_bounds == value) return;
                _bounds = value;
                NotifyPropertyChanged(BoundsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BoundsChangedEventArgs = ObservableHelper.CreateArgs<NAEMOMetadataBase>(x => x.Bounds);
        GeoRect _bounds;

        #endregion

        #region public string Filename { get; set; }

        public string Filename
        {
            get { return _filename; }
            set
            {
                if (_filename == value) return;
                _filename = value;
                NotifyPropertyChanged(FilenameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs FilenameChangedEventArgs = ObservableHelper.CreateArgs<NAEMOMetadataBase>(x => x.Filename);
        string _filename;

        #endregion
    }
}