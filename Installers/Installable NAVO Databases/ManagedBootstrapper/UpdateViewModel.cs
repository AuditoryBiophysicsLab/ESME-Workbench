﻿//-------------------------------------------------------------------------------------------------
// <copyright file="UpdateViewModel.cs" company="Microsoft">
// Copyright (c) Microsoft Corporation. All rights reserved.
//    
//    The use and distribution terms for this software are covered by the
//    Common Public License 1.0 (http://opensource.org/licenses/cpl1.0.php)
//    which can be found in the file CPL.TXT at the root of this distribution.
//    By using this software in any fashion, you are agreeing to be bound by
//    the terms of this license.
//    
//    You must not remove this notice, or any other, from this software.
// </copyright>
// 
// <summary>
// The model of the update view.
// </summary>
//-------------------------------------------------------------------------------------------------

using System;
using System.ComponentModel;
using System.Linq;
using System.Net;
using System.ServiceModel.Syndication;
using System.Windows.Input;
using System.Xml;

namespace WixBootstrapper
{
    /// <summary>
    /// The states of the update view model.
    /// </summary>
    public enum UpdateState
    {
        Initializing,
        Checking,
        Current,
        Available,
        Failed,
    }

    /// <summary>
    /// The model of the update view.
    /// </summary>
    public class UpdateViewModel : PropertyNotifyBase
    {
        private readonly RootViewModel _root;

        const string AppSyndicationNamespace = "http://appsyndication.org/2006/appsyn";

        private UpdateState _state;
        private string _updateUrl;
        private readonly BackgroundWorker _worker;

        private ICommand _checkCommand;
        private ICommand _launchCommand;

        public UpdateViewModel(RootViewModel root)
        {
            _root = root;
            _root.PropertyChanged += RootPropertyChanged;

            _worker = new BackgroundWorker();
            _worker.DoWork += WorkerDoWork;
        }

        public ICommand CheckCommand
        {
            get {
                return _checkCommand ??
                       (_checkCommand =
                        new RelayCommand(param => Refresh(),
                                         param => State == UpdateState.Current || State == UpdateState.Failed));
            }
        }

        public bool CheckEnabled
        {
            get { return CheckCommand.CanExecute(this); }
        }

        public bool CheckVisible
        {
            get { return CheckCommand.CanExecute(this) || State == UpdateState.Initializing; }
        }

        public ICommand LaunchCommand
        {
            get {
                return _launchCommand ??
                       (_launchCommand =
                        new RelayCommand(param => ESMEBootstrapper.LaunchUrl(UpdateUrl),
                                         param => State == UpdateState.Available && !String.IsNullOrEmpty(UpdateUrl)));
            }
        }

        public bool LaunchEnabled
        {
            get { return LaunchCommand.CanExecute(this); }
        }

        public bool CheckingEnabled
        {
            get { return State == UpdateState.Checking; }
        }

        /// <summary>
        /// Gets and sets the state of the update view model.
        /// </summary>
        public UpdateState State
        {
            get
            {
                return _state;
            }

            set
            {
                if (_state != value)
                {
                    _state = value;
                    base.OnPropertyChanged("State");
                    base.OnPropertyChanged("Title");
                    base.OnPropertyChanged("CheckVisible");
                    base.OnPropertyChanged("CheckEnabled");
                    base.OnPropertyChanged("CheckingEnabled");
                    base.OnPropertyChanged("LaunchEnabled");
                }
            }
        }

        /// <summary>
        /// Gets and sets the title of the update view model.
        /// </summary>
        public string Title
        {
            get
            {
                switch (_state)
                {
                    case UpdateState.Initializing:
                        return "Initializing update detection...";

                    case UpdateState.Checking:
                        return "Checking for updates...";

                    case UpdateState.Current:
                        return "Up to date";

                    case UpdateState.Available:
                        return "Newer version available";

                    case UpdateState.Failed:
                        return "Failed to check for updates";

                    default:
                        return "Unexpected state";
                }
            }
        }

        /// <summary>
        /// Gets and sets the update URL.
        /// </summary>
        public string UpdateUrl
        {
            get
            {
                return _updateUrl;
            }

            set
            {
                if (_updateUrl != value)
                {
                    _updateUrl = value;
                    base.OnPropertyChanged("UpdateUrl");
                    base.OnPropertyChanged("LaunchEnabled");
                }
            }
        }

        static void RootPropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            if ("State" == e.PropertyName)
            {
            }
        }

        /// <summary>
        /// Causes the update view to check updates again.
        /// </summary>
        public void Refresh()
        {
            // If we're already checking for updates, skip the refresh.
            if (State == UpdateState.Checking)
            {
                return;
            }

            State = UpdateState.Checking;
            UpdateUrl = null;

            _worker.RunWorkerAsync();
        }

        /// <summary>
        /// Worker thread to check for updates.
        /// </summary>
        /// <param name="sender">Sender.</param>
        /// <param name="e">Arguments.</param>
        private void WorkerDoWork(object sender, DoWorkEventArgs e)
        {
            var succeeded = false;
            try
            {
                var request = ESMEBootstrapper.Model.CreateWebRequest("http://wix.sourceforge.net/releases/wix3.6.feed");
                var response = (HttpWebResponse)request.GetResponse();

                if (response.StatusCode == HttpStatusCode.OK)
                {
                    SyndicationFeed feed;
                    using (var reader = XmlReader.Create(response.GetResponseStream()))
                    {
                        feed = SyndicationFeed.Load(reader);
                    }

                    if (feed != null) {
                        var updates = from entry in feed.Items
                                      from link in entry.Links
                                      from extension in entry.ElementExtensions
                                      where String.Equals(link.RelationshipType, "enclosure", StringComparison.Ordinal) &&
                                            String.Equals(extension.OuterNamespace, AppSyndicationNamespace, StringComparison.Ordinal) &&
                                            String.Equals(extension.OuterName, "version", StringComparison.Ordinal)
                                      select new Update
                                      {
                                              Url = link.Uri.AbsoluteUri,
                                              Version = new Version(extension.GetObject<string>())
                                      };

                        var update = updates.Where(u => u.Version > ESMEBootstrapper.Model.Version).OrderByDescending(u => u.Version).FirstOrDefault();
                        if (update == null)
                        {
                            UpdateUrl = null;
                            State = UpdateState.Current;
                        }
                        else
                        {
                            UpdateUrl = update.Url;
                            State = UpdateState.Available;
                        }
                    }

                    succeeded = true;
                }
            }
            catch (ArgumentException)
            {
            }
            catch (FormatException)
            {
            }
            catch (OverflowException)
            {
            }
            catch (WebException)
            {
            }
            catch (XmlException)
            {
            }

            if (!succeeded)
            {
                State = UpdateState.Failed;
                UpdateUrl = null;
            }
        }

        /// <summary>
        /// Helper class to store AppSyndication URLs associated with their version.
        /// </summary>
        private class Update
        {
            public string Url { get; set; }
            public Version Version { get; set; }
        }
    }
}
