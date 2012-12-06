﻿using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Media;
using System.Windows.Threading;
using ESME.SimulationAnalysis;
using ESME.Simulator;
using ESME.Views.Simulation;
using HRC;
using HRC.Aspects;
using HRC.Services;
using HRC.ViewModels;
using HRC.WPF;
using MEFedMVVM.ViewModelLocator;

namespace SimulationLogAnalysis
{
    [ExportViewModel("SimulationLogAnalysisMainViewModel")]
    public class SimulationLogAnalysisMainViewModel : ViewModelBase, IHistogramSource
    {
        readonly IViewAwareStatus _viewAwareStatus;
        readonly IMessageBoxService _messageBox;
        readonly IUIVisualizerService _visualizer;
        readonly IHRCSaveFileService _saveFile;
        [UsedImplicitly] readonly PropertyObserver<SimulationLogAnalysisMainViewModel> _propertyObserver;
        [UsedImplicitly] CollectionObserver _modeBinsCollectionObserver;
        Dispatcher _dispatcher;

        [ImportingConstructor]
        public SimulationLogAnalysisMainViewModel(IViewAwareStatus viewAwareStatus,
                                                  IMessageBoxService messageBox,
                                                  IUIVisualizerService visualizer,
                                                  IHRCSaveFileService saveFile)
        {
            _viewAwareStatus = viewAwareStatus;
            _messageBox = messageBox;
            _visualizer = visualizer;
            _saveFile = saveFile;
            _viewAwareStatus.ViewLoaded += () =>
            {
                _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
                SelectedFileName = (string)Application.Current.Properties["SelectedFileName"] ?? NoFileOpen;
            };
            _propertyObserver = new PropertyObserver<SimulationLogAnalysisMainViewModel>(this)
                .RegisterHandler(p => SelectedFileName, () => TaskEx.Run(SelectedFileNameChanged));
        }

        async void SelectedFileNameChanged()
        {
            if (SelectedFileName == NoFileOpen || !File.Exists(SelectedFileName)) return;
            Task<bool> processTask = null;
            _dispatcher.InvokeIfRequired(() => _visualizer.ShowWindow("SimulationExposuresView", new SimulationExposuresViewModel(HistogramBinsViewModels)));
            using (var log = SimulationLog.Open(SelectedFileName))
            {
                for (var speciesIndex = 0; speciesIndex < log.SpeciesRecords.Count; speciesIndex++) GuidToColorMap.Add(log.SpeciesRecords[speciesIndex].Guid, _barColors[speciesIndex % _barColors.Count]);
                ModeThresholdHistogram = new ModeThresholdHistogram(this, log);
                _modeBinsCollectionObserver = new CollectionObserver(ModeThresholdHistogram.GroupedExposures.Groups)
                    .RegisterHandler((s, e) =>
                    {
                        switch (e.Action)
                        {
                            case NotifyCollectionChangedAction.Add:
                                foreach (GroupedExposuresHistogram histogram in e.NewItems)
                                    HistogramBinsViewModels.Add(new HistogramBinsViewModel(histogram));
                                break;
                        }
                    });
                var timeStepIndex = 0;
                foreach (var timeStepRecord in log)
                {
                    timeStepRecord.ReadAll();
                    timeStepIndex++;
                    var record = timeStepRecord;
                    if (processTask != null) await processTask;
                    processTask = ModeThresholdHistogram.Process(record, _dispatcher);
                    if (timeStepIndex % 10 == 0) _dispatcher.InvokeIfRequired(UpdateHistogramDisplay);
                }
            }
            if (processTask != null) await processTask;
            _dispatcher.InvokeIfRequired(UpdateHistogramDisplay);
            Debug.WriteLine("Finished processing simulation exposure file");
        }
        [Initialize, UsedImplicitly] public ObservableCollection<HistogramBinsViewModel> HistogramBinsViewModels { get; private set; }

        void UpdateHistogramDisplay()
        {
            var handlers = GraphicsUpdate;
            if (handlers == null) return;
            foreach (EventHandler<EventArgs> handler in handlers.GetInvocationList())
            {
                if (handler.Target is DispatcherObject)
                {
                    var localHandler = handler;
                    ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => localHandler(this, new EventArgs()));
                }
                else
                    handler(this, new EventArgs());
            }
        }

        const string NoFileOpen = "<No file open>";
        [Initialize("<No file open>")] public string SelectedFileName { get; set; }

        public ModeThresholdHistogram ModeThresholdHistogram { get; set; }

        public event EventHandler<EventArgs> GraphicsUpdate;
        [Initialize, UsedImplicitly] public Dictionary<Guid, Color> GuidToColorMap { get; private set; }

        readonly List<Color> _barColors = new List<Color>
        {
            Colors.Red, Colors.Green, Colors.Blue, Colors.Cyan, Colors.Magenta, Colors.DarkKhaki, Colors.Orange, Colors.Purple, Colors.Fuchsia, Colors.Teal, Colors.Black
        };

    }

}