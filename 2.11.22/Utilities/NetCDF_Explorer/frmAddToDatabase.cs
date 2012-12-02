using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.IO;
using NetCDF;
using MySql.Data.MySqlClient;
using ESME.Environment;

namespace NetCDF_Explorer
{
    public partial class frmAddToDatabase: Form
    {
        private NcFile myFile;
        frmDBConnect frmDBConnect = new frmDBConnect();
        MySqlConnection dbConnection;
        ESME.Environment.Database envDB;
        private string lonName, latName, depthName, dataName, missingValueName, scalingName, offsetName;
        private const string NoAttributes = "No qualified attributes";

        public frmAddToDatabase()
        {
            InitializeComponent();
            dbConnection = frmDBConnect.Connect();
            try
            {
                dbConnection.Open();
                envDB = new ESME.Environment.Database(dbConnection);
                dbcDataSet.Database = envDB;
                dbcDataSubset.Database = envDB;
            }
            catch (MySqlException ex)
            {
                //System.Diagnostics.Debug.WriteLine(ex.Message);
                MessageBox.Show(ex.Message, "Error connecting to database", MessageBoxButtons.OK, MessageBoxIcon.Warning);
                ErrorText("Not connected to database");
                //frmDBConnect.ShowDialog();
            }
            IsImportOK();
        }

        private void ErrorText(string text)
        {
            tsStatus.Text = text;
            tsStatus.ForeColor = Color.Red;
        }

        private void StatusText(string text)
        {
            tsStatus.Text = text;
            tsStatus.ForeColor = SystemColors.ControlText;
        }

        private void openNetCDFFileToolStripMenuItem_Click(object sender, EventArgs e)
        {
            openFileDialog1.FileName = Properties.Settings.Default.LastFilename;
            if (openFileDialog1.ShowDialog() == DialogResult.OK)
            {
                Properties.Settings.Default.LastFilename = openFileDialog1.FileName;
                Properties.Settings.Default.Save();
                if (File.Exists(openFileDialog1.FileName))
                {
                    StatusText("Reading NetCDF file, please wait...");
                    this.Cursor = Cursors.WaitCursor;
                    Application.DoEvents();
                    myFile = new NcFile(openFileDialog1.FileName);
                    ncFileExplorer1.NcFile = myFile;
                    StatusText("Ready");
                    this.Cursor = Cursors.Arrow;
                    gbEnvironmentalDatabase.Enabled = true;
                    CheckEnvironmentalDatabaseSettings();
                    PopulateVariableDropdowns();
                }
                else
                {
                    ErrorText("File not found");
                    gbEnvironmentalDatabase.Enabled = false;
                    gbNetCDFVariables.Enabled = false;
                }
            }
        }

        private void connectToEnvironmentalDatabaseToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (frmDBConnect.ShowDialog() == DialogResult.OK)
            {
                dbConnection = frmDBConnect.Connect();
            }
        }

        private void IsImportOK()
        {
            if (myFile == null)
            {
                btnImport.Enabled = false;
                return;
            }
            if (dbcDataSubset.SelectedKey == 0)
            {
                btnImport.Enabled = false;
                return;
            }

            if ((latName == lonName) || (latName == depthName) || (lonName == depthName))
            {
                btnImport.Enabled = false;
                return;
            }
            btnImport.Enabled = true;
        }

        private void btnImport_Click(object sender, EventArgs e)
        {
            if (myFile == null)
                return;
            if (dbcDataSubset.SelectedKey == 0)
            {
                MessageBox.Show("A valid Data Subset must be selected", "Data Subset selection error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return;
            }

            if ((latName == lonName) || (latName == depthName) || (lonName == depthName))
            {
                MessageBox.Show("Latitude, Longitude and Depth variables must all be different", "Variable selection error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return;
            }

            envDB.InsertNewFile(dbcDataSubset.SelectedKey, openFileDialog1.FileName, lonName, latName, depthName, dataName, missingValueName, scalingName, offsetName);
#if false
		    ESME.Environment.XMLMetadata myMetadata = new XMLMetadata(XmlMetadataType.NetCDF, openFileDialog1.FileName);
            myMetadata.AddNode(new DatabaseInfoXML(myMetadata, cboDataType.SelectedText, dbcDataSet.SelectedText, dbcDataSubset.SelectedText));
            myMetadata.AddNode(new NetCDFXML(myMetadata,
                new NetCDFVariableXML(myMetadata, NetCDFXML.LongitudeName, lonName, (int)myFile.Variables[lonName].ElementCount, 0, 0),
                new NetCDFVariableXML(myMetadata, NetCDFXML.LatitudeName, latName, (int)myFile.Variables[latName].ElementCount, 0, 0),
                new NetCDFVariableXML(myMetadata, NetCDFXML.DepthName, depthName, (int)myFile.Variables[depthName].ElementCount, 0, 0),
                new NetCDFDataXml(myMetadata, dataName, new NetCDFDataDimensions(myMetadata, myFile.Variables[dataName].Dimensions), (int)myFile.Variables[dataName].ElementCount, 0, 0, missingValueName, scalingName, offsetName)
                ));
            myMetadata.Save();
#endif        
        }

        private void createNewEnvironmentalDatabaseToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ESME.Environment.Database.Create(frmDBConnect.Server, frmDBConnect.Database, frmDBConnect.Username, frmDBConnect.Password);
            dbConnection = frmDBConnect.Connect();
        }

        private void PopulateVariableDropdowns()
        {
            List<string> oneDvars = new List<string>();
            List<string> multiDvars = new List<string>();
            string[] varNames = new string[myFile.Variables.Count];

            foreach (NcVar var in myFile.Variables)
                if (var.Dimensions.Count == 1)
                    oneDvars.Add(var.ComponentName);
                else if (var.Dimensions.Count > 1)
                    multiDvars.Add(var.ComponentName);

            cboLongitudeName.Items.Clear();
            cboLongitudeName.Items.AddRange(oneDvars.ToArray());
            cboLatitudeName.Items.Clear();
            cboLatitudeName.Items.AddRange(oneDvars.ToArray());
            cboDepthName.Items.Clear();
            cboDepthName.Items.AddRange(oneDvars.ToArray());
            for (int i = 0; i < oneDvars.Count; i++)
            {
                if (oneDvars[i].ToLower().Contains("lon"))
                    cboLongitudeName.SelectedIndex = i;
                if (oneDvars[i].ToLower().Contains("lat"))
                    cboLatitudeName.SelectedIndex = i;
                if (oneDvars[i].ToLower().Contains("dep"))
                    cboDepthName.SelectedIndex = i;
            }
            cboDataName.Items.Clear();
            cboDataName.Items.AddRange(multiDvars.ToArray());
            if (cboDataName.Items.Count == 1)
                cboDataName.SelectedIndex = 0;
            IsImportOK();
        }

        #region Environmental Database Combo Box routines
        private void CheckEnvironmentalDatabaseSettings()
        {
            if ((cboDataType.Items.Count == 0) && (envDB != null))
                cboDataType.Items.AddRange(envDB.DataTypes);
        }

        private void cboDataType_SelectedIndexChanged(object sender, EventArgs e)
        {
            dbcDataSet.ForeignKeyValue = envDB.GetDataTypeID((string)cboDataType.SelectedItem);
            lblDataVariable.Text = (string)cboDataType.SelectedItem + " variable";
            gbAttributes.Text = (string)cboDataType.SelectedItem + " attributes";
            IsImportOK();
        }

        private void dbcDataSet_SelectedIndexChanged(object sender, EventArgs e)
        {
            dbcDataSubset.ForeignKeyValue = envDB.GetDataSetID((string)dbcDataSet.SelectedItem, dbcDataSet.ForeignKeyValue);
            IsImportOK();
        }

        private void dbcDataSubset_SelectedIndexChanged(object sender, EventArgs e)
        {
            gbNetCDFVariables.Enabled = true;
            IsImportOK();
        }
        #endregion

        private void cboDataName_SelectedIndexChanged(object sender, EventArgs e)
        {
            List<string> scaleNames = new List<string>();
            List<string> offsetNames = new List<string>();
            List<string> sameTypeNames = new List<string>();

            dataName = (string)cboDataName.SelectedItem;
            NcVar curDataVar = myFile.Variables[dataName];

            // Only enable depth for three-dimensional datasets
            if (curDataVar.Dimensions.Count > 2)
                cboDepthName.Enabled = true;
            else
            {
                cboDepthName.Enabled = false;
                cboDepthName.Items.Clear();
                cboDepthName.Items.Add("Not used for 2-D datasets");
                cboDepthName.SelectedIndex = 0;
            }

            if (curDataVar.Attributes.Count == 0)
            {
                cboScalingName.Items.Clear();
                cboScalingName.Items.Add("No attributes");
                cboScalingName.SelectedIndex = 0;
                cboOffsetName.Items.Clear();
                cboOffsetName.Items.Add("No attributes");
                cboOffsetName.SelectedIndex = 0;
            }
            else
            {
                foreach (NcAtt curAtt in curDataVar.Attributes)
                {
                    if ((curAtt is NcAttDouble) || (curAtt is NcAttFloat))
                    {
                        scaleNames.Add(curAtt.ComponentName);
                        offsetNames.Add(curAtt.ComponentName);
                    }
                    else if ((curAtt is NcAttInt) || (curAtt is NcAttShort))
                        offsetNames.Add(curAtt.ComponentName);
                    if (curDataVar is NcVarShort)
                    {
                        if (curAtt is NcAttShort)
                            sameTypeNames.Add(curAtt.ComponentName);
                    }
                    else if (curDataVar is NcVarFloat)
                    {
                        if (curAtt is NcAttFloat)
                            sameTypeNames.Add(curAtt.ComponentName);
                    }
                    else if (curDataVar is NcVarDouble)
                    {
                        if (curAtt is NcAttDouble)
                            sameTypeNames.Add(curAtt.ComponentName);
                    }
                }
                cboScalingName.Items.Clear();
                cboScalingName.Items.AddRange(scaleNames.ToArray());
                if (cboScalingName.Items.Count > 0)
                {
                    gbNetCDFVariables.Enabled = true;
                    cboScalingName.Enabled = true;
                    for (int i = 0; i < scaleNames.Count; i++)
                        if (scaleNames[i].ToLower().Contains("scale"))
                        {
                            cboScalingName.SelectedIndex = i;
                            break;
                        }
                }
                else
                {
                    cboScalingName.Items.Add(NoAttributes);
                    cboScalingName.SelectedIndex = 0;
                    cboScalingName.Enabled = false;
                }
                cboOffsetName.Items.Clear();
                cboOffsetName.Items.AddRange(offsetNames.ToArray());
                if (cboOffsetName.Items.Count > 0)
                {
                    gbNetCDFVariables.Enabled = true;
                    cboOffsetName.Enabled = true;
                    for (int i = 0; i < offsetNames.Count; i++)
                        if (offsetNames[i].ToLower().Contains("offset"))
                        {
                            cboOffsetName.SelectedIndex = i;
                            break;
                        }
                }
                else
                {
                    cboOffsetName.Items.Add(NoAttributes);
                    cboOffsetName.SelectedIndex = 0;
                    cboOffsetName.Enabled = false;
                }
                cboMissingValue.Items.Clear();
                cboMissingValue.Items.AddRange(sameTypeNames.ToArray());
                if (cboMissingValue.Items.Count > 0)
                {
                    gbNetCDFVariables.Enabled = true;
                    cboMissingValue.Enabled = true;
                    for (int i = 0; i < sameTypeNames.Count; i++)
                        if (sameTypeNames[i].ToLower().Contains("missing"))
                        {
                            cboMissingValue.SelectedIndex = i;
                            break;
                        }
                }
                else
                {
                    cboMissingValue.Items.Add(NoAttributes);
                    cboMissingValue.SelectedIndex = 0;
                    cboMissingValue.Enabled = false;
                }
            }
            IsImportOK();
        }

        private void cboLongitudeName_SelectedIndexChanged(object sender, EventArgs e)
        {
            string newVal = (string)cboLongitudeName.SelectedItem;
            if ((newVal == NoAttributes) || (cboLongitudeName.Enabled == false))
                newVal = null;
            lonName = newVal;
            IsImportOK();
        }

        private void cboLatitudeName_SelectedIndexChanged(object sender, EventArgs e)
        {
            string newVal = (string)cboLatitudeName.SelectedItem;
            if ((newVal == NoAttributes) || (cboLatitudeName.Enabled == false))
                newVal = null;
            latName = newVal;
            IsImportOK();
        }

        private void cboDepthName_SelectedIndexChanged(object sender, EventArgs e)
        {
            string newVal = (string)cboDepthName.SelectedItem;
            if ((newVal == NoAttributes) || (cboDepthName.Enabled == false))
                newVal = null;
            depthName = newVal;
            IsImportOK();
        }

        private void cboMissingValue_SelectedIndexChanged(object sender, EventArgs e)
        {
            string newVal = (string)cboMissingValue.SelectedItem;
            if ((newVal == NoAttributes) || (cboMissingValue.Enabled == false))
                newVal = null;
            missingValueName = newVal;
            IsImportOK();
        }

        private void cboScalingName_SelectedIndexChanged(object sender, EventArgs e)
        {
            string newVal = (string)cboScalingName.SelectedItem;
            if ((newVal == NoAttributes) || (cboScalingName.Enabled == false))
                newVal = null;
            scalingName = newVal;
            IsImportOK();
        }

        private void cboOffsetName_SelectedIndexChanged(object sender, EventArgs e)
        {
            string newVal = (string)cboOffsetName.SelectedItem;
            if ((newVal == NoAttributes) || (cboOffsetName.Enabled == false))
                newVal = null;
            offsetName = newVal;
            IsImportOK();
        }
    }
}