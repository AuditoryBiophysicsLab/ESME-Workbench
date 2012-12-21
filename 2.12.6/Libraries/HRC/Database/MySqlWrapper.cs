#if false
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using MySql.Data.MySqlClient;
using System.Windows.Forms;

namespace HRC.Database
{
    public class MySqlWrapper
    {
        /// <summary>
        /// IP Address or hostname of the MySQL server
        /// </summary>
        public string Server { get; set; }
        /// <summary>
        /// Username of an authorized account on the MySQL server
        /// </summary>
        public string User { get; set; }
        /// <summary>
        /// Password for the authorized account named in User
        /// </summary>
        public string Password { get; set; }
        /// <summary>
        /// (Optional) TCP/IP Port to be used to connect to the MySQL server
        /// </summary>
        public UInt16 Port { get; set; }
        /// <summary>
        /// If you wish to supply your own properly-formatted MySQL connection string, it will be used instead of the individual Server, User, Password, etc. specified.  If you wish to use the individual fields, make sure this field is either unset, or set to an empty string
        /// </summary>
        public string ConnectString { get; set; }
        /// <summary>
        /// Name of the database you wish to use.  This database must exist before the connection attempt, or the connection will fail
        /// </summary>
        public string Database { get; set; }

        public MySqlWrapper()
        {
            Server = User = Password = ConnectString = Database = "";
            Port = 0;
        }

        /// <summary>
        /// Attempt to connect to a MySQL server on the specified ConnectString (if specified), or the specified Server (host) and (optional) Port, with the specified User and Password, and (optional) Database.
        /// </summary>
        /// <returns></returns>
        public void Connect()
        {
            MySqlConnection connection;

            if (ConnectString == "")
            {
                StringBuilder tmpConnect = new StringBuilder();
                if (Server != "")
                    tmpConnect.AppendFormat("Server={0};", Server);
                if (User != "")
                    tmpConnect.AppendFormat("Uid={0};", User);
                if (Password != "")
                    tmpConnect.AppendFormat("Pwd={0};", Password);
                if (Database != "")
                    tmpConnect.AppendFormat("Database={0};", Database);
                if (Port != 0)
                    tmpConnect.AppendFormat("Port={0};", Port);
                ConnectString = tmpConnect.ToString();
            }
            connection = new MySqlConnection(ConnectString);
            connection.Open();
            connection.Close();
        }

        public void DeleteDatabase(string DatabaseName)
        {
            string dropDatabase = String.Format("DROP DATABASE IF EXISTS `{0}`;", DatabaseName);
            Execute(dropDatabase);
            Database = "";
        }

        public void CreateDatabase(string DatabaseName)
        {
            string createDatabase = String.Format("CREATE DATABASE IF NOT EXISTS `{0}`;USE `{0}`;", DatabaseName);
            Execute(createDatabase);
            Database = DatabaseName;
        }

        public void Execute(string NonQueryCommand)
        {
            MySqlHelper.ExecuteNonQuery(ConnectString, NonQueryCommand, null);
        }

        public object ExecuteScalar(string ScalarCommand)
        {
            return MySqlHelper.ExecuteScalar(ConnectString, ScalarCommand);
        }

        public MySqlDataReader ExecuteReader(string Query)
        {
            return MySqlHelper.ExecuteReader(ConnectString, Query);
        }

        public int InsertRow(string InsertCommand)
        {
            MySqlHelper.ExecuteScalar(ConnectString, InsertCommand);
            MySqlDataReader Reader = MySqlHelper.ExecuteReader(ConnectString, "SELECT InsertID AS LAST_INSERT_ID();");
            return Reader.GetInt32(Reader.GetOrdinal("InsertID"));
        }
    }
}
#endif
