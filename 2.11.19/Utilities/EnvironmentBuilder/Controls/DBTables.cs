using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using MySql.Data.MySqlClient;

namespace IESME_GEO.Controls
{
    public partial class DBTables : UserControl
    {
        protected MySqlConnection DBConnection = new MySqlConnection();
        protected String DB;

        public DBTables()
        {
            InitializeComponent();
            this.datatypelist.Enabled = false;
            this.datasetlist.Enabled = false;
            this.datasubsetlist.Enabled = false;
            this.dbsubmit_btn.Enabled = false;

            DBConnection = IESME_GEO.Properties.Settings.Default.MySQLConn;
            DB = IESME_GEO.Properties.Settings.Default.DatabaseTableName;
        }

        public void GetDataType()
        {
            MySqlDataReader reader = null;


            DBConnection.ChangeDatabase(DB);

            MySqlCommand cmd2 = new MySqlCommand("SELECT Name FROM datatype", DBConnection);
            try
            {
                datatypelist.Enabled = true;
                reader = cmd2.ExecuteReader();
                datatypelist.Items.Clear();
                while (reader.Read())
                {
                    datatypelist.Items.Add(reader.GetString(0));
                }
            }
            catch (MySqlException ex)
            {
                MessageBox.Show("Failed to populate table list: " + ex.Message);
            }
            finally
            {
                if (reader != null) reader.Close();
            }
        }

        private void datatypelist_SelectedIndexChanged(object sender, EventArgs e)
        {
            MySqlDataReader reader = null;

            DBConnection.ChangeDatabase(DB);

            int index_dt = datatypelist.SelectedIndex + 1;
            MySqlCommand cmd2 = new MySqlCommand("SELECT Name FROM dataset WHERE idDataType = '" + index_dt + "'", DBConnection);
            try
            {
                datasetlist.Enabled = true;
                reader = cmd2.ExecuteReader();
                datasetlist.Items.Clear();
                while (reader.Read())
                {
                    datasetlist.Items.Add(reader.GetString(0));
                }
            }
            catch (MySqlException ex)
            {
                MessageBox.Show("Failed to populate table list: " + ex.Message);
            }
            finally
            {
                if (reader != null) reader.Close();
            }
        }

        private void datasetlist_SelectedIndexChanged(object sender, EventArgs e)
        {
            MySqlDataReader reader = null;

            DBConnection.ChangeDatabase(DB);

            int set_index = datatypelist.SelectedIndex + 1;

            MySqlCommand cmd2 = new MySqlCommand("SELECT Name FROM datasubset WHERE idDataSet = '" + set_index + "'", DBConnection);
            try
            {
                datasubsetlist.Enabled = true;
                reader = cmd2.ExecuteReader();
                datasubsetlist.Items.Clear();
                dbsubmit_btn.Enabled = true;
                while (reader.Read())
                {
                    datasubsetlist.Items.Add(reader.GetString(0));
                }
            }
            catch (MySqlException ex)
            {
                MessageBox.Show("Failed to populate table list: " + ex.Message);
            }
            finally
            {
                if (reader != null) reader.Close();
            }
        }

        private void dbsubmit_btn_Click(object sender, EventArgs e)
        {
            string datasubsetlistvalue;

            if (datasubsetlist.Text == "")
            {
                datasubsetlistvalue = "NULL";
            }
            else datasubsetlistvalue = datasubsetlist.Text;
            MessageBox.Show("ESME Environment Data Base: Presenting \n" + "Data Base: " + DB + "\nData Type: " + datatypelist.Text + "\nData Set: " + datasetlist.Text + "\nData Subset: " + datasubsetlistvalue);
        }

        public void OnClear()
        {
            datasetlist.Items.Clear();
            datasubsetlist.Items.Clear();
            datatypelist.Items.Clear();

            datasetlist.Enabled = false;
            datasubsetlist.Enabled = false;
            datatypelist.Enabled = false;
            dbsubmit_btn.Enabled = false;
        }
    }
}
