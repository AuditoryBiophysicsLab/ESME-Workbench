using System;
using System.IO;
using System.Linq;
using ESME.Environment;
using ESME.Scenarios;
using HRC.Validation;
using HRC.ViewModels;

namespace ESME.Views.Scenarios
{
    /// <summary>
    /// To create and show the view as a dialog:
    /// var vm = new ScenarioPropertiesViewModel {...};
    /// var result = _visualizerService.ShowDialog("ScenarioPropertiesView", vm);
    /// if ((!result.HasValue) || (!result.Value)) return;
    /// 
    /// To create and show the view as a window:
    /// var vm = new ScenarioPropertiesViewModel {...};
    /// var window = _visualizerService.ShowWindow("ScenarioPropertiesView", vm);
    /// </summary>
    public class ScenarioPropertiesViewModel : ValidatingViewModel
    {
        public Scenario Scenario { get; private set; }

        public ScenarioPropertiesViewModel(Scenario scenario)
        {
            Scenario = scenario;
            ScenarioName = Scenario.Name;
            Duration = Scenario.Duration;
            Comments = Scenario.Comments;
            ComputeSizes();
            ValidationRules.Add(new ValidationRule
            {
                PropertyName = "ScenarioName",
                Description = "Must be unique within the selected location and cannot be null or empty",
                RuleDelegate = (o, r) =>
                {
                    var target = (ScenarioPropertiesViewModel)o;
                    return !string.IsNullOrEmpty(target.ScenarioName);
                },
            });
        }
        public string ScenarioName { get; set; }
        public TimeSpan Duration { get; set; }
        public string DurationString { get { return Duration.ToString(@"hh\:mm"); } set { Duration = TimeSpan.ParseExact(value, @"hh\:mm", null); } }
        public string Comments { get; set; }
        public TimePeriod TimePeriod { get { return Scenario.TimePeriod; } }
        public string AcousticDataSize { get; private set; }
        public string EnvironmentDataSize { get; private set; }

        void ComputeSizes()
        {
            var speciesFileSize = Scenario.ScenarioSpecies.Select(species => new FileInfo(species.PopulationFilePath)).Select(fi => fi.Length).Sum();
            EnvironmentDataSize = FormattedSize(speciesFileSize + Scenario.Wind.FileSize + Scenario.SoundSpeed.FileSize + Scenario.Sediment.FileSize + Scenario.Bathymetry.FileSize);
            AcousticDataSize = FormattedSize((from p in Scenario.Platforms
                                              from s in p.Sources
                                              from m in s.Modes
                                              from t in m.TransmissionLosses
                                              from r in t.Radials
                                              select r.FileSize).Sum());
        }

        static string FormattedSize(long size)
        {
            var logSize = Math.Log(size, 2);
            if (logSize < 10) return String.Format("{0}b", size);
            if (logSize < 20) return String.Format("{0}K", size >> 10);
            if (logSize < 30) return String.Format("{0}M", size >> 20);
            if (logSize < 40) return String.Format("{0}G", size >> 30);
            if (logSize < 50) return String.Format("{0}T", size >> 40);
            return logSize < 60 ? String.Format("{0}P", size >> 50) : String.Format("{0:N}b", size);
        }

        public int PlatformCount { get { return Scenario.Platforms.Count; } }
        public int SourceCount { get { return (from p in Scenario.Platforms from s in p.Sources select s).Count(); } }
        public int ModeCount { get { return (from p in Scenario.Platforms from s in p.Sources from m in s.Modes select m).Count(); } }
        public int SpeciesCount { get { return Scenario.ScenarioSpecies.Count; } }

        #region OkCommand
        public SimpleCommand<object, object> OkCommand { get { return _ok ?? (_ok = new SimpleCommand<object, object>(OkHandler)); } }
        SimpleCommand<object, object> _ok;

        void OkHandler(object o)
        {
            Scenario.Name = ScenarioName;
            Scenario.Duration = Duration;
            Scenario.Comments = Comments;
            CloseActivePopUpCommand.Execute(true);
        }
        #endregion
    }
}
