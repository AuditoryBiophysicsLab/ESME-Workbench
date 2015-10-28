using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Linq;
using ESME.Locations;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Scenarios
{
    public abstract class SourceBase : IHaveGuid
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        
        public string SourceName { get; set; }
        public string SourceType { get; set; }

        public Platform Platform { get; set; }
        [Initialize]
        public ObservableList<Mode> Modes { get; set; }
    }

    public class SourcePSM : SourceBase { }

    [NotifyPropertyChanged]
    public sealed class Source : SourceBase
    {
        public Source() { }

        public Source(Source source)
        {
            
            SourceName = source.SourceName;
            SourceType = source.SourceType;
            if (source.Modes != null)
                foreach (var newmode in source.Modes.Select(mode => new Mode(mode)))
                {
                    Modes.Add(newmode);
                }
        }

        public static Source NewPSMSource(Platform platform = null)
        {
            return new Source
            {
                SourceName = "New Source",
                SourceType = "new source",
                Platform = platform,
            };
        }
        #region Mapped Properties
      
        [Initialize]
        public ObservableList<LogEntry> Logs { get; set; }
        #endregion


        #region Unmapped Properties
        [NotMapped]
        public string PSMName { get { return string.Format("{0}:{1}", Platform.PlatformName, SourceName); } }
        [NotMapped]
        public bool IsNew { get; set; }
        [NotMapped]
        public object LayerControl
        {
            get { return _layerControl; }
            set
            {
                _layerControl = value;
                MediatorMessage.Send(MediatorMessage.SourceBoundToLayer, this);
            }
        }
         
        object _layerControl;

        #endregion

        public bool Equals(Source other)
        {
            if (Guid != other.Guid) return false;
            if (SourceName != other.SourceName) return false;
            if (SourceType != other.SourceType) return false;
            var modes = (from m in Modes orderby m.ModeName select m).ToList();
            var othermodes = (from m in other.Modes orderby m.ModeName select m).ToList();
            if (modes.Count != othermodes.Count) return false;
            if (modes.Where((t, i) => !t.Equals(othermodes[i])).Any()) return false;

            return true;
        }

        #region commands

        #region AddModeCommand
        public SimpleCommand<object, EventToCommandArgs> AddModeCommand
        {
            get { return _addMode ?? (_addMode = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.AddMode, this))); }
        }
         
        SimpleCommand<object, EventToCommandArgs> _addMode;
        #endregion

        #region DeleteSourceCommand
        public SimpleCommand<object, EventToCommandArgs> DeleteSourceCommand { get { return _deleteSource ?? (_deleteSource = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.DeleteSource, this))); } }
         
        SimpleCommand<object, EventToCommandArgs> _deleteSource;
        #endregion

        #region SourcePropertiesCommand
        public SimpleCommand<object, EventToCommandArgs> SourcePropertiesCommand { get { return _sourceProperties ?? (_sourceProperties = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.SourceProperties, this))); } }
         
        SimpleCommand<object, EventToCommandArgs> _sourceProperties;
        #endregion
        #endregion
        
        public void Delete()
        {
            Platform.Sources.Remove(this);
            foreach (var mode in Modes.ToList()) mode.Delete();
            Globals.MasterDatabaseService.Context.Sources.Remove(this);
        }
    }
}