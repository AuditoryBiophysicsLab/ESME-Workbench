using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using NUnit.Framework;

namespace TransmissionLossCalculator.Tests
{
    public class UITests
    {
        [Test]
        public void CanCreateAndShowWpfWindow()
        {
            var runner = new CrossThreadTestRunner();
            runner.RunInSTA(
              delegate
              {
                  Console.WriteLine(Thread.CurrentThread.GetApartmentState());

                  var window = new System.Windows.Window();
                  window.Show();
              });
        }
    }
}
