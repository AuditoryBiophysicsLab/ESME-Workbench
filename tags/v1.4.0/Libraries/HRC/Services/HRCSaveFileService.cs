using System;
using System.ComponentModel.Composition;
using System.Windows;
using Cinch;
using MEFedMVVM.ViewModelLocator;
using Microsoft.Win32;

namespace HRC.Services
{
    /// <summary>
    ///   This class implements the ISaveFileService for WPF purposes.
    /// </summary>
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(IHRCSaveFileService))]
    public class HRCSaveFileService : IHRCSaveFileService
    {
        #region Data

        /// <summary>
        ///   Embedded SaveFileDialog to pass back correctly selected
        ///   values to ViewModel
        /// </summary>
        readonly SaveFileDialog _sfd = new SaveFileDialog();

        #endregion

        #region ISaveFileService Members

        /// <summary>
        ///   This method should show a window that allows a file to be selected
        /// </summary>
        /// <param name = "owner">The owner window of the dialog</param>
        /// <returns>A bool from the ShowDialog call</returns>
        public bool? ShowDialog(Window owner)
        {
            //Set embedded SaveFileDialog.Filter
            if (!String.IsNullOrEmpty(Filter)) _sfd.Filter = Filter;

            //Set embedded SaveFileDialog.InitialDirectory
            if (!String.IsNullOrEmpty(InitialDirectory)) _sfd.InitialDirectory = InitialDirectory;

            //Set embedded SaveFileDialog.OverwritePrompt
            _sfd.OverwritePrompt = OverwritePrompt;

            //return results
            return _sfd.ShowDialog(owner);
        }

        /// <summary>
        ///   FileName : Simply use embedded SaveFileDialog.FileName
        ///   But DO NOT allow a Set as it will ONLY come from user
        ///   picking a file
        /// </summary>
        public string FileName
        {
            get { return _sfd.FileName; }
            set { _sfd.FileName = value; }
        }

        /// <summary>
        ///   Filter : Simply use embedded SaveFileDialog.Filter
        /// </summary>
        public string Filter
        {
            get { return _sfd.Filter; }
            set { _sfd.Filter = value; }
        }

        /// <summary>
        ///   Filter : Simply use embedded SaveFileDialog.InitialDirectory
        /// </summary>
        public string InitialDirectory
        {
            get { return _sfd.InitialDirectory; }
            set { _sfd.InitialDirectory = value; }
        }

        /// <summary>
        ///   OverwritePrompt : Simply use embedded SaveFileDialog.OverwritePrompt
        /// </summary>
        public bool OverwritePrompt
        {
            get { return _sfd.OverwritePrompt; }
            set { _sfd.OverwritePrompt = value; }
        }

        #endregion
    }
    public interface IHRCSaveFileService
    {
        bool OverwritePrompt { get; set; }
        string FileName { get; set; }
        string Filter { get; set; }
        string InitialDirectory { get; set; }
        bool? ShowDialog(Window owner);
    }

}