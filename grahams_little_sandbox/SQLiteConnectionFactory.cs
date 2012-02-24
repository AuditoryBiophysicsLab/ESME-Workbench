using System;
using System.Data.Common;
using System.Data.Entity.Infrastructure;
using System.Linq;
using System.Text;

namespace grahams_little_sandbox
{

    public class SQLiteConnectionFactory : IDbConnectionFactory
    {

        private const string invariantName = "Devart.Data.SQLite";

        #region Constructors

        public SQLiteConnectionFactory()
            : this(String.Empty, String.Empty)
        {
        }

        public SQLiteConnectionFactory(string basePath, string defaultConnectionString)
        {

            this.BasePath = basePath;
            this.DefaultConnectionString = defaultConnectionString;
        }

        #endregion

        #region IDbConnectionFactory Members

        public DbConnection CreateConnection(string nameOrConnectionString)
        {

            if (String.IsNullOrEmpty(nameOrConnectionString))
                throw new ArgumentNullException("nameOrConnectionString");

            DbProviderFactory sqliteProviderFactory = DbProviderFactories.GetFactory(invariantName);
            if (sqliteProviderFactory == null)
                throw new InvalidOperationException(String.Format("The '{0}' provider is not registered on the local machine.", invariantName));

            DbConnection connection = sqliteProviderFactory.CreateConnection();

            if (nameOrConnectionString.Contains('='))
                connection.ConnectionString = nameOrConnectionString;
            else
            {
                StringBuilder builder = new StringBuilder(128);
                builder.Append(DefaultConnectionString);

                if (builder.Length > 0 && builder[builder.Length - 1] != ';')
                    builder.Append(';');

                builder.Append("Data Source=");
                builder.Append(BasePath);
                string dbFileName = nameOrConnectionString;
                if (dbFileName.Contains('.'))
                {
                    int classNameFrom = nameOrConnectionString.LastIndexOf('.') + 1;
                    int classNameLength = nameOrConnectionString.Length - classNameFrom;
                    dbFileName = nameOrConnectionString.Substring(classNameFrom, classNameLength);
                }
                builder.Append(dbFileName);
                builder.Append(".db");

                connection.ConnectionString = builder.ToString();
            }

            return connection;
        }

        #endregion

        #region Properties

        public string BasePath { get; private set; }

        public string DefaultConnectionString { get; private set; }

        #endregion

    }

}
