using System.Collections.Generic;
using System.IO;
using System.Linq;
using ESME.Scenarios;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.Scenarios
{
   public class SpeciesPropertiesViewModel : ViewModelBase
    {
        public string WindowTitle { get; set; }
        public string LatinName { get; set; }
        public string SpeciesDefinitionFilename { get; set; }
        public string SpeciesDirectory { get; set; }
        public float PopulationDensity { get; set; }
        public List<string> PredefinedSpecies
        {
            get
            {
                return Directory.GetFiles(SpeciesDirectory, "*.spe").ToList().Select(Path.GetFileNameWithoutExtension).ToList();
            }
        }
        string _selectedSpecies;
        public string SelectedSpecies
        {
            get { return _selectedSpecies; }
            set
            {
                _selectedSpecies = value;
                foreach (var species in PredefinedSpecies.Where(species => _selectedSpecies == species)) SpeciesDefinitionFilename = species + ".spe";
                LatinName = _selectedSpecies;
            }
        }
        
        public SpeciesPropertiesViewModel(ScenarioSpecies species)
        {
            SpeciesDirectory = Path.GetDirectoryName(species.SpeciesDefinitionFilePath);
            LatinName = species.LatinName;
            PopulationDensity = species.PopulationDensity;
            SelectedSpecies = Path.GetFileNameWithoutExtension(species.SpeciesDefinitionFilename);
        }

        #region commands
        #region OkCommand
        public SimpleCommand<object, EventToCommandArgs> OkCommand
        {
            get { return _ok ?? (_ok = new SimpleCommand<object, EventToCommandArgs>(OkHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _ok;

        void OkHandler(EventToCommandArgs args)
        {
            CloseDialog(true);
        }
        #endregion

        #region CancelCommand
        public SimpleCommand<object, EventToCommandArgs> CancelCommand
        {
            get { return _cancel ?? (_cancel = new SimpleCommand<object, EventToCommandArgs>(CancelHandler)); }
        }

        SimpleCommand<object, EventToCommandArgs> _cancel;

        void CancelHandler(EventToCommandArgs args)
        {
            //var parameter = args.CommandParameter;
            CloseDialog(false);
        }
        #endregion
        #endregion
    }
}
