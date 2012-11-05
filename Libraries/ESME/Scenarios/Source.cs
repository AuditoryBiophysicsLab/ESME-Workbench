using System;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using ESME.Locations;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Scenarios
{
     [NotifyPropertyChanged]
    public class Source : IHaveGuid
    {
         public Source() {}

         public Source(Source source) 
         {
             PSMSourceGuid = source.PSMSourceGuid;
             SourceName = source.SourceName;
             SourceType = source.SourceType;
         }

         public static Source NewPSMSource()
         {
             return new Source
             {
                 SourceName = "New Source",
                 SourceType = "new source",
                 Platform = null,
                 PSMSourceGuid = "",
             };
         }
         #region Mapped Properties
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

         #region commands

         #region AddModeCommand
         public SimpleCommand<object, EventToCommandArgs> AddModeCommand { get { return _addMode ?? (_addMode = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.AddMode, this))); } }
         SimpleCommand<object, EventToCommandArgs> _addMode;
         #endregion

         #region PSM database commands
         #region AddPSMModeCommand
         public SimpleCommand<object, EventToCommandArgs> AddPSMModeCommand
         {
             get
             {
                 return _addPSMMode ?? (_addPSMMode = new SimpleCommand<object, EventToCommandArgs>(o =>
                     {
                         var mode = new Mode() { Source = this, ModeName = "New Mode", IsNew = true };
                         MediatorMessage.Send(MediatorMessage.AddPSMMode, mode);
                     }));
             }
         }

         SimpleCommand<object, EventToCommandArgs> _addPSMMode;
         #endregion



         #region EditPSMSourceCommand
         public SimpleCommand<object, EventToCommandArgs> EditPSMSourceCommand
         {
             get { return _editPSMSource ?? (_editPSMSource = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.EditPSMSource, this))); }
         }

         SimpleCommand<object, EventToCommandArgs> _editPSMSource;

         #endregion

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
            Scenario.Database.Context.Sources.Remove(this);
        }
    }
}