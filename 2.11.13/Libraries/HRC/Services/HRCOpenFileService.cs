using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.IO;
using System.Windows;
using HRC.Collections;
using MEFedMVVM.ViewModelLocator;
using Microsoft.Win32;

namespace HRC.Services
{
    /// <summary>
    ///   This class implements the IOpenFileService for WPF purposes.
    /// </summary>
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(IHRCOpenFileService))]
    public class HRCOpenFileService : IHRCOpenFileService
    {
        #region Data

        /// <summary>
        ///   Embedded OpenFileDialog to pass back correctly selected
        ///   values to ViewModel
        /// </summary>
        readonly OpenFileDialog _ofd = new OpenFileDialog();

        /// <summary>
        /// Used if set, to associate filter strings with last-used directories
        /// Must be externally managed
        /// </summary>
        public static SerializableDictionary<string, string> InitialDirectories { get; set; }

        #endregion

        #region IOpenFileService Members

        /// <summary>
        ///   This method should show a window that allows a file to be selected
        /// </summary>
        /// <param name = "owner">The owner window of the dialog</param>
        /// <returns>A bool from the ShowDialog call</returns>
        public bool? ShowDialog(Window owner)
        {
            //Set embedded OpenFileDialog.Filter
            if (!String.IsNullOrEmpty(Filter)) _ofd.Filter = Filter;

            //Set embedded OpenFileDialog.InitialDirectory
            if (!String.IsNullOrEmpty(InitialDirectory)) _ofd.InitialDirectory = InitialDirectory;

            //return results
            return _ofd.ShowDialog(owner);
        }

        /// <summary>
        ///   FileName : Simply use embedded OpenFileDialog.FileName
        ///   But DO NOT allow a Set as it will ONLY come from user
        ///   picking a file
        /// </summary>
        public string FileName
        {
            get
            {
                if (InitialDirectories != null)
                {
                    if (InitialDirectories.ContainsKey(Filter)) InitialDirectories[Filter] = Path.GetDirectoryName(_ofd.FileName);
                    else InitialDirectories.Add(Filter, Path.GetDirectoryName(_ofd.FileName));
                }
                return _ofd.FileName;
            }
            set { _ofd.FileName = value; }
        }

        /// <summary>
        ///   Filter : Simply use embedded OpenFileDialog.Filter
        /// </summary>
        public string Filter
        {
            get { return _ofd.Filter; }
            set
            {
                _ofd.Filter = value;
                if (InitialDirectories != null && InitialDirectories.ContainsKey(value)) InitialDirectory = InitialDirectories[value];
            }
        }

        /// <summary>
        ///   Filter : Simply use embedded OpenFileDialog.InitialDirectory
        /// </summary>
        public string InitialDirectory
        {
            get { return _ofd.InitialDirectory; }
            set { _ofd.InitialDirectory = value; }
        }

        public string Title
        {
            get { return _ofd.Title; }
            set { _ofd.Title = value; }
        }
        #endregion
    }

    public interface IHRCOpenFileService
    {
        string FileName { get; set; }
        string Filter { get; set; }
        string InitialDirectory { get; set; }
        bool? ShowDialog(Window owner);
        string Title { get; set; }
    }
}