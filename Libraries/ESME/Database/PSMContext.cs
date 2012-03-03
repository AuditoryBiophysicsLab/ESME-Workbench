using System.Data.Common;
using System.Data.Entity;

namespace ESME.Database
{
    public class PSMContext : DbContext
    {
        public PSMContext(DbConnection connection, bool contextOwnsConnection, IDatabaseInitializer<PSMContext> initializer)
                : base(connection, contextOwnsConnection)
        {
            System.Data.Entity.Database.SetInitializer(initializer);
        }

        public DbSet<Platform> Platforms { get; set; }
        public DbSet<Source> Sources { get; set; }
        public DbSet<Mode> Modes { get; set; }
    }
}