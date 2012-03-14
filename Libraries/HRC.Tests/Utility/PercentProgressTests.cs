using System;
using System.ComponentModel;
using System.Linq;
using HRC.Utility;
using NUnit.Framework;

namespace HRC.Tests.Utility
{
    public class PercentProgressTests
    {
        [Test]
        public void Test()
        {
            var bigList = new PercentProgressList<string>("                                Big List")
            {
                new PercentProgressList<string>("                 List One")
                {
                    new PercentProgress<string> {ProgressTarget = "Item One"},
                    new PercentProgress<string> {ProgressTarget = "Item Two"},
                    new PercentProgress<string> {ProgressTarget = "Item Three"},
                    new PercentProgress<string> {ProgressTarget = "Item Four"},
                },
                new PercentProgressList<string>("                 List Two")
                {
                    new PercentProgress<string> {ProgressTarget = "Item Five"},
                    new PercentProgress<string> {ProgressTarget = "Item Six"},
                    new PercentProgress<string> {ProgressTarget = "Item Seven"},
                    new PercentProgress<string> {ProgressTarget = "Item Eight"},
                },
            };
#if false
            ((INotifyPropertyChanged)bigList[0]).PropertyChanged += PropertyChangedHandler;
            ((INotifyPropertyChanged)bigList[1]).PropertyChanged += PropertyChangedHandler;
            ((INotifyPropertyChanged)bigList).PropertyChanged += PropertyChangedHandler;
            foreach (var item in bigList.Cast<PercentProgressList>().SelectMany(list => list)) 
                ((INotifyPropertyChanged)item).PropertyChanged += PropertyChangedHandler;
#endif

            foreach (PercentProgressList list in bigList)
            {
                foreach (var item in list)
                {
                    for (var i = 0; i <= 100; i++) item.Report(i);
                    Assert.AreEqual(100, item.PercentComplete);
                }
                Assert.AreEqual(100, list.PercentComplete);
            }
            Assert.AreEqual(100, bigList.PercentComplete);
        }

        static void PropertyChangedHandler(object sender, PropertyChangedEventArgs args)
        {
            var progressList = sender as PercentProgressList<string>;
            var progressItem = sender as PercentProgress<string>;
            if (progressList != null) Console.WriteLine("{0}: {1,3}%", progressList.ProgressTarget, progressList.PercentComplete);
            if (progressItem != null) Console.WriteLine("{0}: {1,3}%", progressItem.ProgressTarget, progressItem.PercentComplete);
        }
    }
}
