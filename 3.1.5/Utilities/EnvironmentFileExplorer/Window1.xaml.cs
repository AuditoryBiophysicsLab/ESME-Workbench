using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using ESME.Environment;
using HRC.Navigation;
using System.IO;

namespace EnvironmentFileExplorer
{
    /// <summary>
    /// Interaction logic for Window1.xaml
    /// </summary>
    public partial class Window1 : Window
    {
        private Queue<string> WorkQueue = new Queue<string>();
        System.Windows.Threading.DispatcherTimer Timer;

        public Window1()
        {
            InitializeComponent();
            Timer = new System.Windows.Threading.DispatcherTimer(new TimeSpan(0, 0, 1), 
                System.Windows.Threading.DispatcherPriority.ApplicationIdle, 
                new EventHandler(Timer_Tick), 
                System.Windows.Threading.Dispatcher.CurrentDispatcher);
            Timer.IsEnabled = false;
        }

        void Timer_Tick(object sender, EventArgs e)
        {
            if (WorkQueue.Count == 0)
            {
                Timer.IsEnabled = false;
                return;
            }
            OpenFile(WorkQueue.Dequeue());
        }

        /// <summary>
        /// Opens a DataFile, creates a new tab and TreeView, and displays the DataFile's metadata in the TreeView
        /// </summary>
        /// <param name="FileName">The DataFile to open</param>
        private void OpenFile(string FileName)
        {
            tabControl1.Visibility = Visibility.Visible;
            TabItem newItem = new TabItem { Header = System.IO.Path.GetFileName(FileName) };
            tabControl1.Items.Add(newItem);
            TreeView treeView = new TreeView();
            newItem.Content = treeView;
            tabControl1.SelectedIndex = tabControl1.Items.Count - 1;

            TreeViewItem curFile = new TreeViewItem();
            curFile.Header = "File: " + FileName;

            treeView.Items.Clear();
            treeView.Items.Add(curFile);
            curFile.IsExpanded = true;

            try
            {
                DataFile file = DataFile.Open(FileName);
                foreach (DataLayer layer in file.Layers)
                {
                    TreeViewItem curLayer, latAxis, lonAxis, depAxis;
                    curLayer = AddSubItem(curFile, "Layer: " + layer.Name);
                    curLayer.IsExpanded = true;
                    AddSubItem(curLayer, "Time period: " + layer.TimePeriod);
                    AddSubItem(curLayer, "Metadata: " + layer.Metadata);
                    AddSubItem(curLayer, "Original Filename(s): " + layer.OriginalFilename);
                    lonAxis = AddAxisDetail(curLayer, layer.LongitudeAxis, "deg");
                    latAxis = AddAxisDetail(curLayer, layer.LatitudeAxis, "deg");
                    if (layer.DepthAxis != null)
                    {
                        depAxis = AddAxisDetail(curLayer, layer.DepthAxis, "m");
                        AddSubItem(curLayer, "Data: [" + layer.RowCount + ", " + layer.ColumnCount + ", " + layer.DepthCount + "]").ToolTip = new ToolTip { Content = "Tool Tippy" };
                    }
                    else
                        AddSubItem(curLayer, "Data: [" + layer.RowCount + ", " + layer.ColumnCount + "]");
                }
            }
            catch (Exception e)
            {
                treeView.Items.Add(new TreeViewItem { Header = "Error opening requested file.  Format may be invalid." });
                treeView.Items.Add(new TreeViewItem { Header = "Error detail follows." });
                treeView.Items.Add(new TreeViewItem { Header = e.ToString() });
            }
        }

        /// <summary>
        /// Adds detailed information about a supplied axis to the supplied TreeViewItem
        /// </summary>
        /// <param name="parent">Item TreeViewItem that will be the parent item to get the detail for the selected axis.</param>
        /// <param name="curAxis">The DataAxis we're going to be providing detail about</param>
        /// <param name="units">The units (if any) the current axis uses, like meters or degrees or whatnot</param>
        /// <returns></returns>
        private TreeViewItem AddAxisDetail(TreeViewItem parent, DataAxis curAxis, string units)
        {
            TreeViewItem newItem, axisDetail;
            int lowIndex, highIndex;

            newItem = AddSubItem(parent, curAxis.Name + " Axis [" + curAxis.Length + "]");
            newItem.IsExpanded = true;
            lowIndex = highIndex = -1;
            lowIndex = 0;
            highIndex = curAxis.Length - 1;
            AddSubItem(newItem, "Minimum value: " + curAxis.Map[lowIndex].Value + " " + units);
            AddSubItem(newItem, "Maximum value: " + curAxis.Map[highIndex].Value + " " + units);
            axisDetail = AddSubItem(newItem, "Detail");
            for (int j = lowIndex; j <= highIndex; j++)
                AddSubItem(axisDetail, "[" + curAxis.Map[j].Index + "] = " + curAxis.Map[j].Value.ToString() + " " + units); 
            return newItem;
        }

        /// <summary>
        /// Wrapper to add a sub item to a specified item, without setting a Tag
        /// </summary>
        /// <param name="parent">Parent item</param>
        /// <param name="Header">Header text for the new item</param>
        /// <returns>New item, already added as a child of the provided parent item</returns>
        private TreeViewItem AddSubItem(TreeViewItem parent, string Header)
        {
            return AddSubItem(parent, Header, null);
        }

        /// <summary>
        /// Wrapper to add a sub item to a specified item, and allows the caller to set a Tag
        /// </summary>
        /// <param name="parent">Parent item</param>
        /// <param name="Header">Header text for the new item</param>
        /// <param name="Tag">Object to set as a tag for the new item, or null if no tag is desired</param>
        /// <returns>New item, already added as a child of the provided parent item</returns>
        private TreeViewItem AddSubItem(TreeViewItem parent, string Header, object Tag)
        {
            TreeViewItem newItem = new TreeViewItem();
            newItem.Header = Header;
            newItem.Tag = Tag;
            parent.Items.Add(newItem);
            return newItem;
        }

        #region Command CanExecute and Executed handlers
        #region Open
        /// <summary>
        /// We can always open a new document
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void Open_CanExecute(object sender, CanExecuteRoutedEventArgs e)
        {
            e.CanExecute = true;
        }

        /// <summary>
        /// Open a new document, prompts the user with an OpenFileDialog.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void Open_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            // Configure open file dialog box
            Microsoft.Win32.OpenFileDialog dlg = new Microsoft.Win32.OpenFileDialog();
            dlg.DefaultExt = ".eeb"; // Default file extension
            dlg.CheckFileExists = true;
            dlg.CheckPathExists = true;
            dlg.Multiselect = true;
            dlg.Filter = "ESME Environment Binary File (*.eeb)|*.eeb"; // Filter files by extension

            // Show open file dialog box
            Nullable<bool> result = dlg.ShowDialog();

            // Process open file dialog box results
            if (result == true)
            {
                // Open document
                if (!File.Exists(dlg.FileName))
                {
                    MessageBox.Show("Specified file does not exist!", "Error opening specified file", MessageBoxButton.OK, MessageBoxImage.Error);
                    return;
                }
                foreach (string cur in dlg.FileNames)
                    WorkQueue.Enqueue(cur);
                Timer.IsEnabled = true;
            }
        }
        #endregion
        #region Exit
        /// <summary>
        /// We can always exit the application
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void Exit_CanExecute(object sender, CanExecuteRoutedEventArgs e)
        {
            e.CanExecute = true;
        }

        /// <summary>
        /// User requested the application to exit, so close the form
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void Exit_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            this.Close();
        }
        #endregion
        #region Close
        /// <summary>
        /// Close is allowed if there are any open files, disallowed otherwise
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void Close_CanExecute(object sender, CanExecuteRoutedEventArgs e)
        {
            if (tabControl1.Items.Count > 0)
                e.CanExecute = true;
            else
                e.CanExecute = false;
        }

        /// <summary>
        /// The user requested to close a file.  Process that request by closing the current tab.
        /// If there are then no open tabs, hide the tab control
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void Close_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            tabControl1.Items.Remove(tabControl1.Items[tabControl1.SelectedIndex]);
            if (tabControl1.Items.Count == 0)
                tabControl1.Visibility = Visibility.Hidden;
        }
        #endregion
        #endregion

        private void treeViewEEBInfo_Expanded(object sender, RoutedEventArgs e)
        {
            TreeViewItem item = (TreeViewItem)e.OriginalSource;
            if (item.Tag is DataLayer)
            {
            }
            else if (item.Tag is DataAxis)
            {
            }
        }

        private void Window_Closing(object sender, System.ComponentModel.CancelEventArgs e)
        {
            Properties.Settings.Default.Save();
        }

        private void Window_SizeChanged(object sender, SizeChangedEventArgs e)
        {
            Properties.Settings.Default.Width = this.Width;
            Properties.Settings.Default.Height = this.Height;
        }

        private void FilesDropped(object sender, DragEventArgs e)
        {
            if (e.Data.GetDataPresent(DataFormats.FileDrop))
            {
                e.Handled = true;
                string[] files = e.Data.GetData(DataFormats.FileDrop, true) as string[];
                foreach (string cur in files)
                    WorkQueue.Enqueue(cur);
                Timer.IsEnabled = true;
            }
        }

        private void FilesDragged(object sender, DragEventArgs e)
        {
            if (e.Data.GetDataPresent(DataFormats.FileDrop))
            {
                string[] files = e.Data.GetData(DataFormats.FileDrop, true) as string[];
                e.Effects = DragDropEffects.Copy;
                e.Handled = true;
            }
        }

        private void Window_Loaded(object sender, RoutedEventArgs e)
        {
            if (App.Args.Length > 0)
            {
                foreach (string cur in App.Args)
                    WorkQueue.Enqueue(cur);
                Timer.IsEnabled = true;
            }
        }
    }
}
