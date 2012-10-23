using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using ESME.Environment;

namespace EnvironmentBuilder.VisualControls
{
    public partial class EnvironmentFiles : UserControl
    {
        private string mSearchDirectory;

        public EnvironmentFiles()
        {
            InitializeComponent();
        }

        public string SearchDirectory
        {
            get { return mSearchDirectory; }
            set
            {
                mSearchDirectory = value;
                environmentFileWind.SearchDirectory = mSearchDirectory;
                environmentFileSoundSpeed.SearchDirectory = mSearchDirectory;
                environmentFileBottomType.SearchDirectory = mSearchDirectory;
                environmentFileBathymetry.SearchDirectory = mSearchDirectory;
            }
        }

        public DataLayer[] SelectedLayers
        {
            get
            {
                DataLayerList theList = new DataLayerList();
                if (environmentFileWind.SelectedLayer != null)
                    theList.Add(environmentFileWind.SelectedLayer);
                if (environmentFileSoundSpeed.SelectedLayer != null)
                    theList.Add(environmentFileSoundSpeed.SelectedLayer);
                if (environmentFileBottomType.SelectedLayer != null)
                    theList.Add(environmentFileBottomType.SelectedLayer);
                if (environmentFileBathymetry.SelectedLayer != null)
                    theList.Add(environmentFileBathymetry.SelectedLayer);
                return theList.ToArray();
            }
        }

        public bool CanExtract(EnvironmentFiles e)
        {
            bool wind, soundspeed, bottomtype, bathymetry;
            wind = soundspeed = bottomtype = bathymetry = false;

            /**uncomment when wind data is available**/

            /*if (e.environmentFileWind.LayerAvailable)
                if (e.environmentFileWind.SelectedLayer != null)
                    wind = true;*/
            if (e.environmentFileBottomType.LayerAvailable)
                if (e.environmentFileBottomType.SelectedLayer != null)
                    bottomtype = true;
            if (e.environmentFileSoundSpeed.LayerAvailable)
                if (e.environmentFileSoundSpeed.SelectedLayer != null)
                    soundspeed = true;
            if (e.environmentFileBathymetry.LayerAvailable)
                if (e.environmentFileBathymetry.SelectedLayer != null)
                    bathymetry = true;

            return /*wind &&*/ soundspeed && bottomtype && bathymetry;
        }
    }

}
