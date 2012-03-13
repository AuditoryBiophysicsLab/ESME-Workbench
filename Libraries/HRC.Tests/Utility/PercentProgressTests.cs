using System;
using System.ComponentModel;
using HRC.Navigation;
using HRC.Utility;
using NUnit.Framework;
using PostSharp;

namespace HRC.Tests.Utility
{
    public class PercentProgressTests
    {
        [Test]
        public void Test()
        {
            var progressList1 = new PercentProgressList<string>
            {
                new PercentProgress<string>{ProgressTarget = "One"},
                new PercentProgress<string>{ProgressTarget = "Two"},
                new PercentProgress<string>{ProgressTarget = "Three"},
                new PercentProgress<string>{ProgressTarget = "Four"},
            };
            progressList1.ProgressTarget = "List One";
            var progressList2 = new PercentProgressList<string>
            {
                new PercentProgress<string>{ProgressTarget = "One"},
                new PercentProgress<string>{ProgressTarget = "Two"},
                new PercentProgress<string>{ProgressTarget = "Three"},
                new PercentProgress<string>{ProgressTarget = "Four"},
            };
            progressList2.ProgressTarget = "List Two";
            Post.Cast<PercentProgressList<string>, INotifyPropertyChanged>(progressList1).PropertyChanged += (s, e) => Console.WriteLine("{0} PercentComplete: {1}", ((PercentProgressList<string>)s).ProgressTarget, ((PercentProgressList<string>)s).PercentComplete);
            Post.Cast<PercentProgressList<string>, INotifyPropertyChanged>(progressList2).PropertyChanged += (s, e) => Console.WriteLine("{0} PercentComplete: {1}", ((PercentProgressList<string>)s).ProgressTarget, ((PercentProgressList<string>)s).PercentComplete);
            var bigList = new PercentProgressList<string>
            {
                progressList1, progressList2
            };
            bigList.ProgressTarget = "                                Big List";
            Post.Cast<PercentProgressList<string>, INotifyPropertyChanged>(bigList).PropertyChanged += (s, e) => Console.WriteLine("{0} PercentComplete: {1}", ((PercentProgressList<string>)s).ProgressTarget, ((PercentProgressList<string>)s).PercentComplete);
            for (var k = 0; k < 2; k++)
                for (var j = 0; j < 4; j++)
                    for (var i = 0; i <= 100; i++)
                    {
                        //Assert.AreEqual((i / progressList1.Count) + (j * 25), progressList1.PercentComplete, string.Format("at j = {0} and i = {1}", j, i));
                        //Console.Write("{0} ", i);
                        ((PercentProgressList)bigList[k])[j].Report(i);
                    }
        }
    }
}
