using System.Windows;
using System.Windows.Controls;
using Microsoft.Win32;
using Path = System.IO.Path;

namespace ESME.Views.Controls
{
    /// <summary>
    /// Follow steps 1a or 1b and then 2 to use this custom control in a XAML file.
    ///
    /// Step 1a) Using this custom control in a XAML file that exists in the current project.
    /// Add this XmlNamespace attribute to the root element of the markup file where it is 
    /// to be used:
    ///
    ///     xmlns:MyNamespace="clr-namespace:ESME.Views.Controls"
    ///
    ///
    /// Step 1b) Using this custom control in a XAML file that exists in a different project.
    /// Add this XmlNamespace attribute to the root element of the markup file where it is 
    /// to be used:
    ///
    ///     xmlns:MyNamespace="clr-namespace:ESME.Views.Controls;assembly=ESME.Views.Controls"
    ///
    /// You will also need to add a project reference from the project where the XAML file lives
    /// to this project and Rebuild to avoid compilation errors:
    ///
    ///     Right click on the target project in the Solution Explorer and
    ///     "Add Reference"->"Projects"->[Browse to and select this project]
    ///
    ///
    /// Step 2)
    /// Go ahead and use your control in the XAML file.
    ///
    ///     <MyNamespace:FileOrDirectorySetting/>
    ///
    /// </summary>
    public class FileOrDirectorySetting : Control
    {
        static FileOrDirectorySetting()
        {
            DefaultStyleKeyProperty.OverrideMetadata(typeof(FileOrDirectorySetting), new FrameworkPropertyMetadata(typeof(FileOrDirectorySetting)));
        }

        public FileOrDirectorySetting()
        {
            BrowserButton = new Button { Content = "..." };
            BrowserButton.Click += OpenFileClick;
        }

        #region dependency property Button BrowserButton

        public static DependencyProperty BrowserButtonProperty = DependencyProperty.Register("BrowserButton",
                                                                                 typeof(Button),
                                                                                 typeof(FileOrDirectorySetting));

        public Button BrowserButton
        {
            get { return (Button)GetValue(BrowserButtonProperty); }
            set { SetCurrentValue(BrowserButtonProperty, value); }
        }

        #endregion

        #region dependency property Visibility CaptionVisibility

        public static DependencyProperty CaptionVisibilityProperty = DependencyProperty.Register("CaptionVisibility",
                                                                                 typeof(Visibility),
                                                                                 typeof(FileOrDirectorySetting),
                                                                                 new FrameworkPropertyMetadata(Visibility.Visible, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, CaptionVisibilityPropertyChanged));

        public Visibility CaptionVisibility { get { return (Visibility)GetValue(CaptionVisibilityProperty); } set { SetValue(CaptionVisibilityProperty, value); } }

        static void CaptionVisibilityPropertyChanged(DependencyObject obj, DependencyPropertyChangedEventArgs args) { ((FileOrDirectorySetting)obj).CaptionVisibilityPropertyChanged(); }
        void CaptionVisibilityPropertyChanged() { }
        #endregion

        public static DependencyProperty CaptionProperty = DependencyProperty.Register("Caption", typeof(string), typeof(FileOrDirectorySetting), new FrameworkPropertyMetadata("Caption", FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public string Caption
        {
            get { return (string)GetValue(CaptionProperty); }
            set { SetCurrentValue(CaptionProperty, value); }
        }

        public static DependencyProperty DialogTitleProperty = DependencyProperty.Register("DialogTitle", typeof(string), typeof(FileOrDirectorySetting), new FrameworkPropertyMetadata("", FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public string DialogTitle
        {
            get { return (string)GetValue(DialogTitleProperty); }
            set { SetCurrentValue(DialogTitleProperty, value); }
        }

        public static DependencyProperty FileNameProperty = DependencyProperty.Register("FileName", typeof(string), typeof(FileOrDirectorySetting), new FrameworkPropertyMetadata("FileOrDirectorySetting", FrameworkPropertyMetadataOptions.BindsTwoWayByDefault, OnFileNameChanged));

        public string FileName
        {
            get { return (string)GetValue(FileNameProperty); }
            set { SetCurrentValue(FileNameProperty, value); }
        }

        public static void OnFileNameChanged(DependencyObject dependencyObject, DependencyPropertyChangedEventArgs e)
        {
            var control = (FileOrDirectorySetting)dependencyObject;
            var fileName = control.FileName;
        }

        public static DependencyProperty FileNameFilterProperty = DependencyProperty.Register("FileNameFilter", typeof(string), typeof(FileOrDirectorySetting), new FrameworkPropertyMetadata("", FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public string FileNameFilter
        {
            get { return (string)GetValue(FileNameFilterProperty); }
            set { SetCurrentValue(FileNameFilterProperty, value); }
        }

        public static DependencyProperty IsDirectoryBrowserProperty = DependencyProperty.Register("IsDirectoryBrowser", typeof(bool), typeof(FileOrDirectorySetting), new FrameworkPropertyMetadata(false, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public bool IsDirectoryBrowser
        {
            get { return (bool)GetValue(IsDirectoryBrowserProperty); }
            set
            {
                SetCurrentValue(IsDirectoryBrowserProperty, value);
                FileNameProperty.DefaultMetadata.DefaultValue = IsDirectoryBrowser ? "DirectoryName" : "FileName";
            }
        }

        public static DependencyProperty ShowNewFolderButtonProperty = DependencyProperty.Register("ShowNewFolderButton", typeof(bool), typeof(FileOrDirectorySetting), new FrameworkPropertyMetadata(false, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public bool ShowNewFolderButton
        {
            get { return (bool)GetValue(ShowNewFolderButtonProperty); }
            set { SetCurrentValue(ShowNewFolderButtonProperty, value); }
        }

        public static DependencyProperty UseSaveFileDialogProperty = DependencyProperty.Register("UseSaveFileDialog", typeof(bool), typeof(FileOrDirectorySetting), new FrameworkPropertyMetadata(false, FrameworkPropertyMetadataOptions.BindsTwoWayByDefault));

        public bool UseSaveFileDialog
        {
            get { return (bool)GetValue(UseSaveFileDialogProperty); }
            set { SetCurrentValue(UseSaveFileDialogProperty, value); }
        }

        void OpenFileClick(object sender, RoutedEventArgs e)
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
                    if ((bool)fileResult)
                    {
                        // Open document
                        FileName = fileDialog.FileName;
                    }
                }
            }
        }
    }
}
