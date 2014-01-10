using System;
using System.IO;
using System.Xml.Serialization;
using ESME.Environment;
using ESME.Locations;
using ESME.Plugins;
using ESME.Views.Locations;
using HRC;
using HRC.Navigation;
using HRC.Validation;
using HRC.ViewModels;

namespace FileBasedDataSources
{
    [Serializable]
    public abstract class FileBasedDataSource<T> : EnvironmentalDataSourcePluginBase<T> where T: class
    {
        [UsedImplicitly]
        PropertyObserver<FileBasedDataSource<T>> _observer;
        protected FileBasedDataSource()
        {
            _observer = new PropertyObserver<FileBasedDataSource<T>>(this)
                .RegisterHandler(p => p.DataLocation, Save);

            AddValidationRules(new ValidationRule<FileBasedDataSource<T>>
            {
                PropertyName = "DataLocation",
                Description = "File must exist",
                IsRuleValid = (target, rule) => target.IsConfigured,
            });
            SelectionControlViewModel = new MultipleSelectionsViewModel<float>
            {
                UnitName = " min",
                AvailableSelections = new[]{0f},
            };
            SelectionControl = new MultipleSelectionsView { DataContext = SelectionControlViewModel };
        }

        public override bool IsConfigured { get { return DataLocation != null && File.Exists(DataLocation); } }

        [XmlIgnore] public string ControlCaption { get; set; }
        [XmlIgnore] public string DialogTitle { get; set; }
        [XmlIgnore] public string FilenameFilter { get; set; }
        [XmlIgnore] public string DataLocationHelp { get; set; }

        public string DataLocation { get; set; }

        [CanBeNull]
        protected string[] ReadFieldsFromLine(StreamReader reader, ref int lineNumber)
        {
            var curLine = "//";
            while (curLine.StartsWith("//"))
            {
                curLine = reader.ReadLine();
                lineNumber++;
                if (curLine == null) return null;
                curLine = curLine.Trim();
            }
            return curLine.Split(new[] { ',' }, StringSplitOptions.RemoveEmptyEntries);
        }

        protected Geo ParseGeo(string latString, string lonString, string fileTypeName, int lineNumber)
        {
            double latitude;
            if (!double.TryParse(latString, out latitude)) throw new FormatException(string.Format("{0} was not in the expected format at line {1}: Latitude field is not a floating point number", fileTypeName, lineNumber));
            if (double.IsNaN(latitude) || latitude < -90 || latitude > 90) throw new FormatException(string.Format("{0} was not in the expected format at line {1}: Latitude must be between -90 and +90", fileTypeName, lineNumber));

            double longitude;
            if (!double.TryParse(lonString, out longitude)) throw new FormatException(string.Format("{0} was not in the expected format at line {1}: Longitude field is not a floating point number", fileTypeName, lineNumber));
            if (double.IsNaN(longitude) || longitude < -360 || longitude > 360) throw new FormatException(string.Format("{0} was not in the expected format at line {1}: Longitude must be between -360 and +360", fileTypeName, lineNumber));
            
            return new Geo(latitude, longitude);
        }

        public override EnvironmentalDataSet SelectedDataSet
        {
            get
            {
                var selectedItem = ((MultipleSelectionsViewModel<float>)SelectionControlViewModel).SelectedItem;
                return new EnvironmentalDataSet { SourcePlugin = PluginIdentifier, Resolution = selectedItem.Value, TimePeriod = TimePeriod.Invalid };
            }
        }
    }
}