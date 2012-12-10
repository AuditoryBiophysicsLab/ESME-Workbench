using System.ComponentModel.Composition;
using System.Windows;
using MEFedMVVM.ViewModelLocator;

namespace HRC.Services
{
    /// <summary>
    /// This class implements the IMessageBoxService for WPF purposes.
    /// </summary>
    [PartCreationPolicy(CreationPolicy.Shared)]
    [ExportService(ServiceType.Both, typeof(IMessageBoxService))]
    public class WPFMessageBoxService : IMessageBoxService
    {
        #region IMessageBoxService Members

        /// <summary>
        /// Displays an error dialog with a given message.
        /// </summary>
        /// <param name="message">The message to be displayed.</param>
        public void ShowError(string message)
        {
            ShowMessage(message, "Error", MessageBoxImage.Stop);
        }

        /// <summary>
        /// Displays an error dialog with a given message.
        /// </summary>
        /// <param name="message">The message to be displayed.</param>
        public void ShowInformation(string message)
        {
            ShowMessage(message, "Information", MessageBoxImage.Information);
        }

        /// <summary>
        /// Displays an error dialog with a given message.
        /// </summary>
        /// <param name="message">The message to be displayed.</param>
        public void ShowWarning(string message)
        {
            ShowMessage(message, "Warning", MessageBoxImage.Warning);
        }

        /// <summary>
        /// Displays a Yes/No dialog and returns the user input.
        /// </summary>
        /// <param name="message">The message to be displayed.</param>
        /// <param name="icon">The icon to be displayed.</param>
        /// <returns>User selection.</returns>
        public MessageBoxResult ShowYesNo(string message, MessageBoxImage icon)
        {
            return ShowQuestionWithButton(message, icon, MessageBoxButton.YesNo);
        }

        /// <summary>
        /// Displays a Yes/No/Cancel dialog and returns the user input.
        /// </summary>
        /// <param name="message">The message to be displayed.</param>
        /// <param name="icon">The icon to be displayed.</param>
        /// <returns>User selection.</returns>
        public MessageBoxResult ShowYesNoCancel(string message, MessageBoxImage icon)
        {
            return ShowQuestionWithButton(message, icon, MessageBoxButton.YesNoCancel);
        }

        /// <summary>
        /// Displays a OK/Cancel dialog and returns the user input.
        /// </summary>
        /// <param name="message">The message to be displayed.</param>
        /// <param name="icon">The icon to be displayed.</param>
        /// <returns>User selection.</returns>
        public MessageBoxResult ShowOkCancel(string message, MessageBoxImage icon)
        {
            return ShowQuestionWithButton(message, icon, MessageBoxButton.OKCancel);
        }
        #endregion

        #region Private Methods
        /// <summary>
        /// Shows a standard System.Windows.MessageBox using the parameters requested
        /// </summary>
        /// <param name="message">The message to be displayed.</param>
        /// <param name="heading">The heading to be displayed</param>
        /// <param name="icon">The icon to be displayed.</param>
        private static void ShowMessage(string message, string heading, MessageBoxImage icon)
        {
            MessageBox.Show(message, heading, MessageBoxButton.OK, icon);
        }

        /// <summary>
        /// Shows a standard System.Windows.MessageBox using the parameters requested
        /// but will return a translated result to enable adhere to the IMessageBoxService
        /// implementation required. 
        /// 
        /// This abstraction allows for different frameworks to use the same ViewModels but supply
        /// alternative implementations of core service interfaces
        /// </summary>
        /// <param name="message">The message to be displayed.</param>
        /// <param name="icon">The icon to be displayed.</param>
        /// <param name="button"></param>
        /// <returns>MessageBoxResult results to use</returns>
        private static MessageBoxResult ShowQuestionWithButton(string message, MessageBoxImage icon, MessageBoxButton button)
        {
            return MessageBox.Show(message, "Please confirm...", button, icon);
        }
        #endregion
    }
    /// <summary>
    /// This interface defines a interface that will allow 
    /// a ViewModel to show a messagebox
    /// </summary>
    public interface IMessageBoxService
    {
        /// <summary>
        /// Shows an error message
        /// </summary>
        /// <param name="message">The error message</param>
        void ShowError(string message);

        /// <summary>
        /// Shows an information message
        /// </summary>
        /// <param name="message">The information message</param>
        void ShowInformation(string message);

        /// <summary>
        /// Shows an warning message
        /// </summary>
        /// <param name="message">The warning message</param>
        void ShowWarning(string message);

        /// <summary>
        /// Displays a Yes/No dialog and returns the user input.
        /// </summary>
        /// <param name="message">The message to be displayed.</param>
        /// <param name="icon">The icon to be displayed.</param>
        /// <returns>User selection.</returns>
        MessageBoxResult ShowYesNo(string message, MessageBoxImage icon);

        /// <summary>
        /// Displays a Yes/No/Cancel dialog and returns the user input.
        /// </summary>
        /// <param name="message">The message to be displayed.</param>
        /// <param name="icon">The icon to be displayed.</param>
        /// <returns>User selection.</returns>
        MessageBoxResult ShowYesNoCancel(string message, MessageBoxImage icon);

        /// <summary>
        /// Displays a OK/Cancel dialog and returns the user input.
        /// </summary>
        /// <param name="message">The message to be displayed.</param>
        /// <param name="icon">The icon to be displayed.</param>
        /// <returns>User selection.</returns>
        MessageBoxResult ShowOkCancel(string message, MessageBoxImage icon);

    }
}
