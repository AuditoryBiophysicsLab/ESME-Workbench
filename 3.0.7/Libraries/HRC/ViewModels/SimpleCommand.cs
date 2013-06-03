using System;
using System.Diagnostics.CodeAnalysis;
using System.Windows.Input;

namespace HRC.ViewModels
{
    /// <summary>
    /// Interface that is used for ICommands that notify when they are
    /// completed
    /// </summary>
    public interface ICompletionAwareCommand
    {
        /// <summary>
        /// Notifies that the command has completed
        /// </summary>
        //event Action<Object> CommandCompleted;

        WeakActionEvent<object> CommandCompleted { get; set; }
    }

    /// <summary>
    /// Simple delegating command, based largely on DelegateCommand from PRISM/CAL
    /// </summary>
    /// <typeparam name="T1">The type for the </typeparam>
    /// <typeparam name="T2"> </typeparam>
    public class SimpleCommand<T1, T2> : ICommand, ICompletionAwareCommand
    {
        private readonly Func<T1, bool> _canExecuteMethod;
        private readonly Action<T2> _executeMethod;

        public SimpleCommand(Func<T1, bool> canExecuteMethod, Action<T2> executeMethod)
        {
            _executeMethod = executeMethod;
            _canExecuteMethod = canExecuteMethod;
            CommandCompleted = new WeakActionEvent<object>();
        }

        public SimpleCommand(Action<T2> executeMethod)
        {
            _executeMethod = executeMethod;
            _canExecuteMethod = x => true;
            CommandCompleted = new WeakActionEvent<object>();
        }

        public WeakActionEvent<object> CommandCompleted { get; set; }

        public bool CanExecute(T1 parameter)
        {
            if (_canExecuteMethod == null) return true;
            return _canExecuteMethod(parameter);
        }

        public void Execute(T2 parameter)
        {
            if (_executeMethod != null) _executeMethod(parameter);

            //now raise CommandCompleted for this ICommand
            var completedHandler = CommandCompleted;
            if (completedHandler != null) completedHandler.Invoke(parameter);
        }

        public bool CanExecute(object parameter)
        {
            return CanExecute((T1)parameter);
        }

        public void Execute(object parameter)
        {
            Execute((T2)parameter);
        }

        /// <summary>
        /// Occurs when changes occur that affect whether the command should execute.
        /// </summary>
        public event EventHandler CanExecuteChanged
        {
            add
            {
                if (_canExecuteMethod != null)
                {
                    CommandManager.RequerySuggested += value;
                }
            }

            remove
            {
                if (_canExecuteMethod != null)
                {
                    CommandManager.RequerySuggested -= value;
                }
            }
        }

        /// <summary>
        /// Raises the <see cref="CanExecuteChanged" /> event.
        /// </summary>
        [SuppressMessage("Microsoft.Design", "CA1030:UseEventsWhereAppropriate", Justification = "This cannot be an event")]
        public void RaiseCanExecuteChanged()
        {
            CommandManager.InvalidateRequerySuggested();
        }
    }
}
