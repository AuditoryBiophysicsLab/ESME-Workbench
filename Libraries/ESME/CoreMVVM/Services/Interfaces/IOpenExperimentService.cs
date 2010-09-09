using System;
using System.Windows;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ESME.CoreMVVM.Services
{
    /// <summary>
    /// This interface defines a interface that will allow 
    /// a ViewModel to open an ESME Experiment
    /// </summary>
    public interface IOpenExperimentService
    {
        /// <summary>
        /// ExperimentName
        /// </summary>
        String ExperimentName { get; }

        /// <summary>
        /// Filter
        /// </summary>
        String InitialDirectory { get; set; }

        /// <summary>
        /// This method should show a window that allows a file to be selected
        /// </summary>
        /// <param name="owner">The owner window of the dialog</param>
        /// <returns>A bool from the ShowDialog call</returns>
        bool? ShowDialog(Window owner);
    }
}
