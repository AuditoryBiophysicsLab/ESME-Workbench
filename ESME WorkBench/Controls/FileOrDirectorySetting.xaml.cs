using System.Windows;
using Microsoft.Win32;

namespace ESMEWorkBench.Controls
{
    public partial class FileOrDirectorySetting
    {
        public static DependencyProperty CaptionProperty = DependencyProperty.Register("Caption", typeof (string), typeof (FileOrDirectorySetting), new FrameworkPropertyMetadata("Caption", FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public static DependencyProperty FileNameProperty = DependencyProperty.Register("FileName", typeof (string), typeof (FileOrDirectorySetting), new FrameworkPropertyMetadata("FileName", FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public static DependencyProperty FileNameFilterProperty = DependencyProperty.Register("FileNameFilter", typeof (string), typeof (FileOrDirectorySetting), new FrameworkPropertyMetadata("", FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public static DependencyProperty IsDirectoryBrowserProperty = DependencyProperty.Register("IsDirectoryBrowser", typeof (bool), typeof (FileOrDirectorySetting), new FrameworkPropertyMetadata(false, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public FileOrDirectorySetting()
        {
            InitializeComponent();
        }

        public string Caption
        {
            get { return (string) GetValue(CaptionProperty); }
            set { SetValue(CaptionProperty, value); }
        }

        public string FileName
        {
            get { return (string) GetValue(FileNameProperty); }
            set { SetValue(FileNameProperty, value); }
        }

        public string FileNameFilter
        {
            get { return (string) GetValue(FileNameFilterProperty); }
            set { SetValue(FileNameFilterProperty, value); }
        }

        public bool IsDirectoryBrowser
        {
            get { return (bool) GetValue(IsDirectoryBrowserProperty); }
            set { SetValue(IsDirectoryBrowserProperty, value); }
        }

        void OpenFile_Click(object sender, RoutedEventArgs e)
        {
            if (IsDirectoryBrowser)
            {
                var folderDialog = new System.Windows.Forms.FolderBrowserDialog();
                System.Windows.Forms.DialogResult folderResult = folderDialog.ShowDialog();
                if (folderResult == System.Windows.Forms.DialogResult.OK) FileName = folderDialog.SelectedPath;
            }
            else
            {
                // Create OpenFileDialog
                var fileDialog = new OpenFileDialog
                                 {
                                     Filter = FileNameFilter
                                 };

                // Set filter for file extension and default file extension

                // Display OpenFileDialog by calling ShowDialog method
                var fileResult = fileDialog.ShowDialog();

                // Get the selected file name and display in a TextBox
                if ((bool) fileResult)
                {
                    // Open document
                    FileName = fileDialog.FileName;
                }
            }
        }
    }
}