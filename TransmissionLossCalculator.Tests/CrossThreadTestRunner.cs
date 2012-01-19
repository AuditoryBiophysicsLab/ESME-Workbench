using System;
using System.Reflection;
using System.Security.Permissions;
using System.Threading;

namespace TransmissionLossCalculator.Tests
{
    public class CrossThreadTestRunner
    {
        Exception _lastException;

        public void RunInMTA(ThreadStart userDelegate) { Run(userDelegate, ApartmentState.MTA); }

        public void RunInSTA(ThreadStart userDelegate) { Run(userDelegate, ApartmentState.STA); }

        void Run(ThreadStart userDelegate, ApartmentState apartmentState)
        {
            _lastException = null;

            var thread = new Thread(
                delegate()
                {
                    try
                    {
                        userDelegate.Invoke();
                    }
                    catch (Exception e)
                    {
                        _lastException = e;
                    }
                });
            thread.SetApartmentState(apartmentState);

            thread.Start();
            thread.Join();

            if (ExceptionWasThrown()) ThrowExceptionPreservingStack(_lastException);
        }

        bool ExceptionWasThrown() { return _lastException != null; }

        [ReflectionPermission(SecurityAction.Demand)]
        static void ThrowExceptionPreservingStack(Exception exception)
        {
            var remoteStackTraceString = typeof (Exception).GetField("_remoteStackTraceString",
                                                                     BindingFlags.Instance | BindingFlags.NonPublic);
            if (remoteStackTraceString != null) remoteStackTraceString.SetValue(exception, exception.StackTrace + Environment.NewLine);
            throw exception;
        }
    }
}
