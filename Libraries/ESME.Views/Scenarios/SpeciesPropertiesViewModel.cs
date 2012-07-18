using System;
using System.Collections.Generic;
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
        public float PopulationDensity { get; set; }
        public List<string> PredefinedSpecies
        {
            get
            {
                return new List<string>
                {
                    "Generic Odontocete",
                    "Generic Mysticete",
                 //   "Load custom ...",
                };
            }
        }
        string _selectedSpecies;
        public string SelectedSpecies
        {
            get { return _selectedSpecies; }
            set
            {{}
                _selectedSpecies = value;
                switch (_selectedSpecies)
                {
                    case "Generic Odontocete":
                        SpeciesDefinitionFilename = "generic_odontocete.spe";
                        break;
                    case "Generic Mysticete":
                        SpeciesDefinitionFilename = "generic_mysticete.spe";
                        break;
                    //case "Load custom ...":f
                    //    break;
                    default:
                        throw new ApplicationException("Invalid species type selected!");
                }
            }
        }
        
        public SpeciesPropertiesViewModel(ScenarioSpecies species)
        {
            LatinName = species.LatinName;
            PopulationDensity = species.PopulationDensity;
            switch (species.SpeciesDefinitionFilename)
            {
                case "generic_odontocete.spe":
                    SelectedSpecies = "Generic Odontocete";
                    break;
                case "generic_mysticete.spe":
                    SelectedSpecies = "Generic Mysticete";
                    break;
                default:
                    throw new ApplicationException();
            }
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
