using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using MySql.Data.MySqlClient;
using System.Globalization;
using System.Threading;
using System.IO;
using System.Security.Cryptography;


namespace IESME_GEO.Dialogs
{
    public partial class DBAccess : Form
    {

        public MySqlConnection sqlConnection = new MySqlConnection();
        public string ConnectString
        {
            get
            {
                if (textBox1.Text == "")
                    return "";

                if (textBox7.Text != "")
                    return String.Format("Server={0};Database={1};Uid={2};Pwd={3}", textBox1.Text, textBox2.Text, textBox4.Text, textBox7.Text);
                else
                    return String.Format("Server={0};Database={1};Uid={2}", textBox1.Text, textBox2.Text, textBox4.Text, textBox7.Text);
            }
        }

        public string Server { get { return textBox1.Text; } }
        public string Database { get { return textBox2.Text; } }
        public string Username { get { return textBox4.Text; } }
        public string Password { get { return textBox7.Text; } }

        public DBAccess()
        {
            InitializeComponent();
            this.Visible = false;

            if (checkBox1.Checked)
            {
                if (Properties.Settings.Default.DatabaseEncryptedPassword != "")
                    textBox7.Text = DecryptString(Properties.Settings.Default.DatabaseEncryptedPassword);
                else
                    textBox7.Text = "";
                textBox1.Text = Properties.Settings.Default.DatabaseHost;
                textBox4.Text = Properties.Settings.Default.DatabaseUsername;
            }


        }

        #region Encryption/Decryption routines
        byte[] rijndaelKey = {0xaf, 0x21, 0xb7, 0x6f, 0xcf, 0xa3, 0xc6, 0xb6,
                             0x62, 0x36, 0x5f, 0x72, 0xae, 0x53, 0xf1, 0x12,
                             0x0a, 0x47, 0x0b, 0xc9, 0x60, 0xcd, 0xd2, 0x5c,
                             0xde, 0xe1, 0x33, 0x84, 0x78, 0x77, 0x27, 0xcc};
        byte[] rijndaelIV = {0x77, 0x17, 0x6c, 0x99, 0xb5, 0x52, 0xf8, 0x47,
                            0x8b, 0x46, 0x62, 0x58, 0xe9, 0x4d, 0x6c, 0xbd};

        private string EncryptString(string InputText)
        {
            // We are now going to create an instance of the
            // Rihndael class.
            RijndaelManaged RijndaelCipher = new RijndaelManaged();

            // Create an encryptor from the existing SecretKey bytes.
            // We use 256 bytes for the secret key (the default Rijndael key length is 256 bit = 32 bytes) and
            // then 128 bytes for the IV (initialization vector) (the default Rijndael IV length is 128 bit = 16 bytes)
            ICryptoTransform Encryptor = RijndaelCipher.CreateEncryptor(rijndaelKey, rijndaelIV);

            // Create a MemoryStream that is going to hold the encrypted bytes
            MemoryStream memoryStream = new MemoryStream();

            // Create a CryptoStream through which we are going to be processing our data.
            // CryptoStreamMode.Write means that we are going to be writing data
            // to the stream and the output will be written in the MemoryStream
            // we have provided. (always use write mode for encryption)
            CryptoStream cryptoStream = new CryptoStream(memoryStream, Encryptor, CryptoStreamMode.Write);

            // Start the encryption process.
            cryptoStream.Write(Encoding.ASCII.GetBytes(InputText), 0, InputText.Length);

            // Finish encrypting.
            cryptoStream.FlushFinalBlock();

            // Convert our encrypted data from a memoryStream into a byte array.
            byte[] CipherBytes = memoryStream.ToArray();

            // Close both streams.
            memoryStream.Close();
            cryptoStream.Close();

            // Convert encrypted data into a base64-encoded string.
            // A common mistake would be to use an Encoding class for that.
            // It does not work, because not all byte values can be
            // represented by characters. We are going to be using Base64 encoding
            // That is designed exactly for what we are trying to do.
            string EncryptedData = Convert.ToBase64String(CipherBytes);

            // Return encrypted string.
            return EncryptedData;
        }

        private string DecryptString(string EncryptedBase64Text)
        {
            RijndaelManaged RijndaelCipher = new RijndaelManaged();
            byte[] EncryptedData = Convert.FromBase64String(EncryptedBase64Text);

            // Create a decryptor from the existing SecretKey bytes.
            ICryptoTransform Decryptor = RijndaelCipher.CreateDecryptor(rijndaelKey, rijndaelIV);

            MemoryStream memoryStream = new MemoryStream(EncryptedData);

            // Create a CryptoStream. (always use Read mode for decryption).
            CryptoStream cryptoStream = new CryptoStream(memoryStream, Decryptor, CryptoStreamMode.Read);

            // Since at this point we don't know what the size of decrypted data
            // will be, allocate the buffer long enough to hold EncryptedData;
            // DecryptedData is never longer than EncryptedData.
            byte[] PlainText = new byte[EncryptedData.Length];

            // Start decrypting.
            int DecryptedCount = cryptoStream.Read(PlainText, 0, PlainText.Length);

            memoryStream.Close();
            cryptoStream.Close();

            // Convert decrypted data into a string.
            string DecryptedData = Encoding.ASCII.GetString(PlainText, 0, DecryptedCount);

            // Return decrypted string.             
            return DecryptedData;
        }
        #endregion



        private void button1_Click(object sender, EventArgs e)
        {
            MySqlConnection sqlConnection = Connect();
            sqlConnection.Open();
            if (sqlConnection.State == ConnectionState.Open)
            {
                MessageBox.Show("Connection successful!");

                LoadESME(sqlConnection);
            }
            else
                MessageBox.Show("Connection unsuccessful!");

        }

        public MySqlConnection Connect()
        {
            
            sqlConnection.ConnectionString = ConnectString;
            Properties.Settings.Default.MySQLConn = sqlConnection;
            return sqlConnection;
        }

        private void LoadESME(MySqlConnection sqlConn)
        {
            IESME_GEO.Dialogs.ESME_GE newESME = new IESME_GEO.Dialogs.ESME_GE();
            if (sqlConn.State == ConnectionState.Open)
            {
                this.Hide();
               
                newESME.ShowDialog();

            }

        }

        private void checkBox1_CheckedChanged(object sender, EventArgs e)
        {
            Properties.Settings.Default.SaveDatabaseCredentials = checkBox1.Checked;
            if (!checkBox1.Checked)
            {
                Properties.Settings.Default.DatabaseEncryptedPassword = EncryptString("bogus");
                textBox7.Text = "";
                Properties.Settings.Default.Save();
            }
            else
            {
                Properties.Settings.Default.DatabaseHost = textBox1.Text;
                Properties.Settings.Default.DatabaseUsername = textBox4.Text;
                Properties.Settings.Default.DatabaseEncryptedPassword = EncryptString(textBox7.Text);
                Properties.Settings.Default.DatabaseTableName = textBox2.Text;
                Properties.Settings.Default.Save();
            }
        }

        private void textBox7_TextChanged(object sender, EventArgs e)
        {
            if (textBox7.Text != "")
            {
                if (checkBox1.Checked)
                {
                    Properties.Settings.Default.DatabaseEncryptedPassword = EncryptString(textBox7.Text);
                    Properties.Settings.Default.Save();
                }
            }
        }

        private void button2_Click(object sender, EventArgs e)
        {
            textBox1.Text = "";
            textBox2.Text = "";
            textBox4.Text = "";
            textBox7.Text = "";
            
            Properties.Settings.Default.DatabaseHost = textBox1.Text;
            Properties.Settings.Default.DatabaseUsername = textBox4.Text;
            Properties.Settings.Default.DatabaseEncryptedPassword = EncryptString(textBox7.Text);
            Properties.Settings.Default.DatabaseTableName = textBox2.Text;

            Properties.Settings.Default.SaveDatabaseCredentials = !checkBox1.Checked;

            checkBox1.Checked = false;
        }

        private void button3_Click(object sender, EventArgs e)
        {
            Application.Exit();
        }



    }
}