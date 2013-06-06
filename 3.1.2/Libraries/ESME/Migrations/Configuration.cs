using ESME.Locations;
using System.Data.Entity;
using System.Data.Entity.Migrations;

namespace ESME.Migrations
{
    internal sealed class Configuration : DbMigrationsConfiguration<LocationContext>
    {
        public Configuration()
        {
            AutomaticMigrationsEnabled = false;
        }

        protected override void Seed(LocationContext context)
        {
            //  This method will be called after migrating to the latest version.

            //  You can use the DbSet<T>.AddOrUpdate() helper extension method 
            //  to avoid creating duplicate seed data. E.g.
            //
            //    context.People.AddOrUpdate(
            //      p => p.FullName,
            //      new Person { FullName = "Andrew Peters" },
            //      new Person { FullName = "Brice Lambson" },
            //      new Person { FullName = "Rowan Miller" }
            //    );
            //
        }
    }
    internal sealed class Initializer
        : MigrateDatabaseToLatestVersion<LocationContext, Configuration>
    {
        // Wrapper to allow easier app.config configuration
    }
}
