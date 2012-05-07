using System;
using System.ComponentModel.DataAnnotations;
using ESME.Locations;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Scenarios
{
    public class Source : IHaveGuid
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string PSMSourceGuid { get; set; }
        public string SourceName { get; set; }
        public string SourceType { get; set; }

        public virtual Platform Platform { get; set; }
        [Initialize]
        public virtual ObservableList<Mode> Modes { get; set; }
        [Initialize]
        public virtual ObservableList<LogEntry> Logs { get; set; }
        [NotMapped] public string PSMName { get { return string.Format("{0}:{1}", Platform.PlatformName, SourceName); } }

        #region AddModeCommand
        public SimpleCommand<object, EventToCommandArgs> AddModeCommand { get { return _addMode ?? (_addMode = new SimpleCommand<object, EventToCommandArgs>(AddModeHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _addMode;

        void AddModeHandler(EventToCommandArgs args)
        {
            //var parameter = args.CommandParameter;
            MediatorMessage.Send(MediatorMessage.AddMode, this);
        }
        #endregion

        #region DeleteSourceCommand
        public SimpleCommand<object, EventToCommandArgs> DeleteSourceCommand { get { return _deleteSource ?? (_deleteSource = new SimpleCommand<object, EventToCommandArgs>(DeleteSourceHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _deleteSource;

        void DeleteSourceHandler(EventToCommandArgs args)
        {
            //var parameter = args.CommandParameter;
            MediatorMessage.Send(MediatorMessage.DeleteSource, this);
        }
        #endregion
    }
}