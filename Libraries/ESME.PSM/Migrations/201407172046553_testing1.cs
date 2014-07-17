namespace ESME.PSM.Migrations
{
    using System;
    using System.Data.Entity.Migrations;
    
    public partial class testing1 : DbMigration
    {
        public override void Up()
        {
            AddColumn("dbo.Platform", "Name", c => c.String(maxLength: 4000));
            DropColumn("dbo.Platform", "Description");
        }
        
        public override void Down()
        {
            AddColumn("dbo.Platform", "Description", c => c.String(maxLength: 4000));
            DropColumn("dbo.Platform", "Name");
        }
    }
}
