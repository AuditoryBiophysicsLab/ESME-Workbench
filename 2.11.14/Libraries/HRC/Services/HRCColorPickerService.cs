using System.ComponentModel.Composition;
using System.Windows;
using System.Windows.Forms;
using System.Windows.Media;
using MEFedMVVM.ViewModelLocator;

namespace HRC.Services
{
    /// <summary>
    ///   This class implements the IHRCColorPickerService for WPF purposes.
    /// </summary>
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof (IHRCColorPickerService))]
    public class HRCColorPickerService : IHRCColorPickerService
    {
        #region Data

        /// <summary>
        ///   Embedded OpenFileDialog to pass back correctly selected
        ///   values to ViewModel
        /// </summary>
        readonly ColorDialog _colorDialog = new ColorDialog
                                            {
                                                AllowFullOpen = true,
                                                AnyColor = true
                                            };

        #endregion

        #region IHRCColorPickerService Members

        /// <summary>
        /// This method displays the color picker dialog
        /// </summary>
        /// <param name = "owner">The owner window of the dialog</param>
        /// <returns>A bool? from the ShowDialog call</returns>
        public bool? ShowDialog(Window owner)
        {
            var result = _colorDialog.ShowDialog((IWin32Window)owner);
            return result == DialogResult.OK;
        }

        /// <summary>
        /// This method displays the color picker dialog
        /// </summary>
        /// <returns>A bool? from the ShowDialog call</returns>
        public bool? ShowDialog()
        {
            var result = _colorDialog.ShowDialog();
            return result == DialogResult.OK;
        }

        /// <summary>
        ///   Gets or sets a value indicating whether the user can use the dialog box to define custom colors
        /// </summary>
        public bool AllowFullOpen
        {
            get { return _colorDialog.AllowFullOpen; }
            set { _colorDialog.AllowFullOpen = value; }
        }

        /// <summary>
        ///   Gets or sets a value indicating whether the dialog box displays all available colors in the set of basic colors.
        /// </summary>
        public bool AnyColor
        {
            get { return _colorDialog.AnyColor; }
            set { _colorDialog.AnyColor = value; }
        }

        /// <summary>
        ///   Represents an ARGB (alpha, red, green, blue) color.
        /// </summary>
        public Color Color
        {
            get { return Color.FromArgb(_colorDialog.Color.A, _colorDialog.Color.R, _colorDialog.Color.G, _colorDialog.Color.B); }
            set { _colorDialog.Color = System.Drawing.Color.FromArgb(value.A, value.R, value.G, value.B); }
        }

        #endregion
    }

    public interface IHRCColorPickerService
    {
        Color Color { get; set; }
        bool AllowFullOpen { get; set; }
        bool AnyColor { get; set; }
        bool? ShowDialog(Window owner);
        bool? ShowDialog();
    }
}