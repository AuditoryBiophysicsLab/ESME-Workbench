namespace ESME.PSM.Migrations
{
    using System;
    using System.Data.Entity.Migrations;
    
    public partial class testing0 : DbMigration
    {
        public override void Up()
        {
            CreateTable(
                "dbo.Mode",
                c => new
                    {
                        ModeID = c.Guid(nullable: false),
                        Name = c.String(maxLength: 4000),
                        Type = c.Int(nullable: false),
                        MaxPropagationRadius = c.Single(nullable: false),
                        DepthOffset = c.Single(nullable: false),
                        SourceLevel = c.Single(nullable: false),
                        Frequency = c.Single(nullable: false),
                        PulseLengthTicks = c.Long(nullable: false),
                        PulseIntervalTicks = c.Long(nullable: false),
                        RelativeBeamAngle = c.Single(nullable: false),
                        HorizontalBeamWidth = c.Single(nullable: false),
                        VerticalBeamWidth = c.Single(nullable: false),
                        SideLobeAttenuation = c.Single(nullable: false),
                        DepressionElevationAngle = c.Single(nullable: false),
                        Source_SourceID = c.Guid(nullable: false),
                    })
                .PrimaryKey(t => t.ModeID)
                .ForeignKey("dbo.Source", t => t.Source_SourceID, cascadeDelete: true)
                .Index(t => t.Source_SourceID);
            
            CreateTable(
                "dbo.Source",
                c => new
                    {
                        SourceID = c.Guid(nullable: false),
                        Name = c.String(maxLength: 4000),
                        Type = c.Int(nullable: false),
                        Platform_PlatformID = c.Guid(nullable: false),
                    })
                .PrimaryKey(t => t.SourceID)
                .ForeignKey("dbo.Platform", t => t.Platform_PlatformID, cascadeDelete: true)
                .Index(t => t.Platform_PlatformID);
            
            CreateTable(
                "dbo.Platform",
                c => new
                    {
                        PlatformID = c.Guid(nullable: false),
                        Type = c.Int(nullable: false),
                        Description = c.String(maxLength: 4000),
                        Speed = c.Single(nullable: false),
                        IsRandom = c.Boolean(nullable: false),
                    })
                .PrimaryKey(t => t.PlatformID);
            
        }
        
        public override void Down()
        {
            DropForeignKey("dbo.Mode", "Source_SourceID", "dbo.Source");
            DropForeignKey("dbo.Source", "Platform_PlatformID", "dbo.Platform");
            DropIndex("dbo.Source", new[] { "Platform_PlatformID" });
            DropIndex("dbo.Mode", new[] { "Source_SourceID" });
            DropTable("dbo.Platform");
            DropTable("dbo.Source");
            DropTable("dbo.Mode");
        }
    }
}
