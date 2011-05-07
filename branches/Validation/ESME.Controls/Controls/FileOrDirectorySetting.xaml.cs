using System.IO;
using System.Windows;
using Microsoft.Win32;

namespace ESME.Views.Controls
{
    public partial class FileOrDirectorySetting
    {
        public FileOrDirectorySetting()
        {
            InitializeComponent();
        }

        public static DependencyProperty CaptionProperty = DependencyProperty.Register("Caption", typeof(string), typeof(FileOrDirectorySetting), new FrameworkPropertyMetadata("Caption", FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public string Caption
        {
            get { return (string)GetValue(CaptionProperty); }
            set { SetValue(CaptionProperty, value); }
        }

        public static DependencyProperty DialogTitleProperty = DependencyProperty.Register("DialogTitle", typeof(string), typeof(FileOrDirectorySetting), new FrameworkPropertyMetadata("", FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public string DialogTitle
        {
            get { return (string)GetValue(DialogTitleProperty); }
            set { SetValue(DialogTitleProperty, value); }
        }

        public static DependencyProperty FileNameProperty = DependencyProperty.Register("FileName", typeof(string), typeof(FileOrDirectorySetting), new FrameworkPropertyMetadata("FileName", FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public string FileName
        {
            get { return (string) GetValue(FileNameProperty); }
            set { SetValue(FileNameProperty, value); }
        }

        public static DependencyProperty FileNameFilterProperty = DependencyProperty.Register("FileNameFilter", typeof(string), typeof(FileOrDirectorySetting), new FrameworkPropertyMetadata("", FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public string FileNameFilter
        {
            get { return (string) GetValue(FileNameFilterProperty); }
            set { SetValue(FileNameFilterProperty, value); }
        }

        public static DependencyProperty IsDirectoryBrowserProperty = DependencyProperty.Register("IsDirectoryBrowser", typeof(bool), typeof(FileOrDirectorySetting), new FrameworkPropertyMetadata(false, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public bool IsDirectoryBrowser
        {
            get { return (bool)GetValue(IsDirectoryBrowserProperty); }
            set { SetValue(IsDirectoryBrowserProperty, value); }
        }

        public static DependencyProperty ShowNewFolderButtonProperty = DependencyProperty.Register("ShowNewFolderButton", typeof(bool), typeof(FileOrDirectorySetting), new FrameworkPropertyMetadata(false, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public bool ShowNewFolderButton
        {
            get { return (bool)GetValue(ShowNewFolderButtonProperty); }
            set { SetValue(ShowNewFolderButtonProperty, value); }
        }

        public static DependencyProperty UseSaveFileDialogProperty = DependencyProperty.Register("UseSaveFileDialog", typeof(bool), typeof(FileOrDirectorySetting), new FrameworkPropertyMetadata(false, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public bool UseSaveFileDialog
        {
            get { return (bool)GetValue(UseSaveFileDialogProperty); }
            set { SetValue(UseSaveFileDialogProperty, value); }
        }

        void OpenFile_Click(object sender, RoutedEventArgs e)
        {
            if (IsDirectoryBrowser)
            {
                var folderDialog = new System.Windows.Forms.FolderBrowserDialog
                                   {
                                       SelectedPath = FileName,
                                       ShowNewFolderButton = ShowNewFolderButton,
                                   };
                var folderResult = folderDialog.ShowDialog();
                if (folderResult == System.Windows.Forms.DialogResult.OK) FileName = folderDialog.SelectedPath;
            }
            else
            {
                if (UseSaveFileDialog)
                {
                    // Create OpenFileDialog, and set filter for file extension and default file extension
                    var fileDialog = new SaveFileDialog
                                     {
                                         InitialDirectory = string.IsNullOrEmpty(FileName) ? null : Path.GetDirectoryName(FileName),
                                         FileName = string.IsNullOrEmpty(FileName) ? null : Path.GetFileName(FileName),
                                         Filter = FileNameFilter,
                                         Title = string.IsNullOrEmpty(DialogTitle) ? "Save" : DialogTitle,
                                     };

                    // Display OpenFileDialog by calling ShowDialog method
                    var fileResult = fileDialog.ShowDialog();

                    // Set FileName to the selected FileName
                    if ((bool)fileResult)
                    {
                        // Open document
                        FileName = fileDialog.FileName;
                    }
                }
                else
                {
                    // Create OpenFileDialog, and set filter for file extension and default file extension
                    var fileDialog = new OpenFileDialog
                                     {
                                         InitialDirectory = string.IsNullOrEmpty(FileName) ? null : Path.GetDirectoryName(FileName),
                                         FileName = string.IsNullOrEmpty(FileName) ? null : Path.GetFileName(FileName),
                                         Filter = FileNameFilter,
                                         Title = string.IsNullOrEmpty(DialogTitle) ? "Open" : DialogTitle,
                                     };

                    // Display OpenFileDialog by calling ShowDialog method
                    var fileResult = fileDialog.ShowDialog();

                    // Set FileName to the selected FileName
                    if ((bool) fileResult)
                    {
                        // Open document
                        FileName = fileDialog.FileName;
                    }
                }
            }
        }
    }
}