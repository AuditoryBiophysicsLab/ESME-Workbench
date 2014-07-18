namespace ESME.PSM.Migrations
{
    using System;
    using System.Data.Entity.Migrations;
    
    public partial class switchToScenarioPSM : DbMigration
    {
        public override void Up()
        {
            CreateTable(
                "dbo.Mode",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        PSMModeGuid = c.String(maxLength: 4000),
                        ModeName = c.String(maxLength: 4000),
                        ModeType = c.String(maxLength: 4000),
                        ActiveTime = c.Single(),
                        Depth = c.Single(),
                        SourceLevel = c.Single(nullable: false),
                        LowFrequency = c.Single(nullable: false),
                        HighFrequency = c.Single(nullable: false),
                        PulseInterval_Ticks = c.Long(nullable: false),
                        PulseLength_Ticks = c.Long(nullable: false),
                        HorizontalBeamWidth = c.Single(nullable: false),
                        VerticalBeamWidth = c.Single(nullable: false),
                        DepressionElevationAngle = c.Single(nullable: false),
                        RelativeBeamAngle = c.Single(nullable: false),
                        MaxPropagationRadius = c.Single(nullable: false),
                        TransmissionLossPluginType = c.String(maxLength: 4000),
                        RadialCount = c.Int(nullable: false),
                        SideLobeAttenuation = c.Single(nullable: false),
                        Source_Guid = c.Guid(nullable: false),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.Source", t => t.Source_Guid, cascadeDelete: true)
                .Index(t => t.Source_Guid);
            
            CreateTable(
                "dbo.LogEntry",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        MessageSource_Who = c.String(maxLength: 4000),
                        MessageSource_When_Ticks = c.Long(nullable: false),
                        MessageSource_Where = c.String(maxLength: 4000),
                        Message = c.String(maxLength: 4000),
                        Location_Guid = c.Guid(),
                        EnvironmentalDataSet_Guid = c.Guid(),
                        Scenario_Guid = c.Guid(),
                        Perimeter_Guid = c.Guid(),
                        Platform_Guid = c.Guid(),
                        ShipTrack_Guid = c.Guid(),
                        Source_Guid = c.Guid(),
                        ScenarioSpecies_Guid = c.Guid(),
                        AnalysisPoint_Guid = c.Guid(),
                        Mode_Guid = c.Guid(),
                        Radial_Guid = c.Guid(),
                        TransmissionLoss_Guid = c.Guid(),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.Location", t => t.Location_Guid)
                .ForeignKey("dbo.EnvironmentalDataSet", t => t.EnvironmentalDataSet_Guid)
                .ForeignKey("dbo.Scenario", t => t.Scenario_Guid)
                .ForeignKey("dbo.Perimeter", t => t.Perimeter_Guid)
                .ForeignKey("dbo.Platform", t => t.Platform_Guid)
                .ForeignKey("dbo.ShipTrack", t => t.ShipTrack_Guid)
                .ForeignKey("dbo.Source", t => t.Source_Guid)
                .ForeignKey("dbo.ScenarioSpecies", t => t.ScenarioSpecies_Guid)
                .ForeignKey("dbo.AnalysisPoint", t => t.AnalysisPoint_Guid)
                .ForeignKey("dbo.Mode", t => t.Mode_Guid)
                .ForeignKey("dbo.Radial", t => t.Radial_Guid)
                .ForeignKey("dbo.TransmissionLoss", t => t.TransmissionLoss_Guid)
                .Index(t => t.Location_Guid)
                .Index(t => t.EnvironmentalDataSet_Guid)
                .Index(t => t.Scenario_Guid)
                .Index(t => t.Perimeter_Guid)
                .Index(t => t.Platform_Guid)
                .Index(t => t.ShipTrack_Guid)
                .Index(t => t.Source_Guid)
                .Index(t => t.ScenarioSpecies_Guid)
                .Index(t => t.AnalysisPoint_Guid)
                .Index(t => t.Mode_Guid)
                .Index(t => t.Radial_Guid)
                .Index(t => t.TransmissionLoss_Guid);
            
            CreateTable(
                "dbo.AnalysisPoint",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        Geo_Latitude = c.Double(nullable: false),
                        Geo_Longitude = c.Double(nullable: false),
                        LayerSettings_Guid = c.Guid(),
                        Scenario_Guid = c.Guid(),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.LayerSettings", t => t.LayerSettings_Guid)
                .ForeignKey("dbo.Scenario", t => t.Scenario_Guid)
                .Index(t => t.LayerSettings_Guid)
                .Index(t => t.Scenario_Guid);
            
            CreateTable(
                "dbo.LayerSettings",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        IsChecked = c.Boolean(nullable: false),
                        PointSymbolType_PointSymbolTypeAsInt = c.Int(nullable: false),
                        LineOrSymbolDbColor_Alpha = c.Byte(nullable: false),
                        LineOrSymbolDbColor_Red = c.Byte(nullable: false),
                        LineOrSymbolDbColor_Green = c.Byte(nullable: false),
                        LineOrSymbolDbColor_Blue = c.Byte(nullable: false),
                        AreaDbColor_Alpha = c.Byte(nullable: false),
                        AreaDbColor_Red = c.Byte(nullable: false),
                        AreaDbColor_Green = c.Byte(nullable: false),
                        AreaDbColor_Blue = c.Byte(nullable: false),
                        LayerOrder = c.Int(nullable: false),
                        LineOrSymbolSize = c.Double(nullable: false),
                    })
                .PrimaryKey(t => t.Guid);
            
            CreateTable(
                "dbo.Scenario",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        Name = c.String(maxLength: 4000),
                        Comments = c.String(maxLength: 4000),
                        StorageDirectory = c.String(maxLength: 4000),
                        ShowAllAnalysisPoints = c.Boolean(nullable: false),
                        ShowAllPerimeters = c.Boolean(nullable: false),
                        ShowAllSpecies = c.Boolean(nullable: false),
                        StartTime_Ticks = c.Long(nullable: false),
                        Duration_Ticks = c.Long(nullable: false),
                        TimePeriod_TimePeriodAsByte = c.Byte(nullable: false),
                        Location_Guid = c.Guid(),
                        Bathymetry_Guid = c.Guid(),
                        Sediment_Guid = c.Guid(),
                        SoundSpeed_Guid = c.Guid(),
                        Wind_Guid = c.Guid(),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.Location", t => t.Location_Guid)
                .ForeignKey("dbo.EnvironmentalDataSet", t => t.Bathymetry_Guid)
                .ForeignKey("dbo.EnvironmentalDataSet", t => t.Sediment_Guid)
                .ForeignKey("dbo.EnvironmentalDataSet", t => t.SoundSpeed_Guid)
                .ForeignKey("dbo.EnvironmentalDataSet", t => t.Wind_Guid)
                .Index(t => t.Location_Guid)
                .Index(t => t.Bathymetry_Guid)
                .Index(t => t.Sediment_Guid)
                .Index(t => t.SoundSpeed_Guid)
                .Index(t => t.Wind_Guid);
            
            CreateTable(
                "dbo.EnvironmentalDataSet",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        Resolution = c.Single(nullable: false),
                        SampleCount = c.Int(nullable: false),
                        TimePeriod_TimePeriodAsByte = c.Byte(nullable: false),
                        FileName = c.String(maxLength: 4000),
                        SourcePlugin_PluginType_PluginTypeAsByte = c.Byte(nullable: false),
                        SourcePlugin_PluginSubtype_PluginSubtypeAsByte = c.Byte(nullable: false),
                        SourcePlugin_Type = c.String(maxLength: 4000),
                        PluginXml = c.String(maxLength: 4000),
                        LayerSettings_Guid = c.Guid(),
                        Location_Guid = c.Guid(),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.LayerSettings", t => t.LayerSettings_Guid)
                .ForeignKey("dbo.Location", t => t.Location_Guid)
                .Index(t => t.LayerSettings_Guid)
                .Index(t => t.Location_Guid);
            
            CreateTable(
                "dbo.Location",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        Name = c.String(maxLength: 4000),
                        Comments = c.String(maxLength: 4000),
                        GeoRect_North = c.Double(nullable: false),
                        GeoRect_South = c.Double(nullable: false),
                        GeoRect_East = c.Double(nullable: false),
                        GeoRect_West = c.Double(nullable: false),
                        StorageDirectory = c.String(maxLength: 4000),
                        LayerSettings_Guid = c.Guid(),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.LayerSettings", t => t.LayerSettings_Guid)
                .Index(t => t.LayerSettings_Guid);
            
            CreateTable(
                "dbo.Perimeter",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        Name = c.String(maxLength: 4000),
                        LayerSettings_Guid = c.Guid(),
                        Scenario_Guid = c.Guid(),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.LayerSettings", t => t.LayerSettings_Guid)
                .ForeignKey("dbo.Scenario", t => t.Scenario_Guid)
                .Index(t => t.LayerSettings_Guid)
                .Index(t => t.Scenario_Guid);
            
            CreateTable(
                "dbo.PerimeterCoordinate",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        Order = c.Int(nullable: false),
                        Geo_Latitude = c.Double(nullable: false),
                        Geo_Longitude = c.Double(nullable: false),
                        Perimeter_Guid = c.Guid(),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.Perimeter", t => t.Perimeter_Guid)
                .Index(t => t.Perimeter_Guid);
            
            CreateTable(
                "dbo.Platform",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        Description = c.String(maxLength: 4000),
                        Launches = c.Boolean(nullable: false),
                        Tows = c.Boolean(nullable: false),
                        RepeatCount = c.Int(nullable: false),
                        PSMPlatformGuid = c.String(maxLength: 4000),
                        PlatformName = c.String(maxLength: 4000),
                        PlatformType = c.String(maxLength: 4000),
                        TrackType_TrackTypeAsByte = c.Byte(nullable: false),
                        Geo_Latitude = c.Double(nullable: false),
                        Geo_Longitude = c.Double(nullable: false),
                        IsRandom = c.Boolean(nullable: false),
                        Depth = c.Single(nullable: false),
                        Course = c.Single(nullable: false),
                        Speed = c.Single(nullable: false),
                        LayerSettings_Guid = c.Guid(),
                        Perimeter_Guid = c.Guid(),
                        Scenario_Guid = c.Guid(),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.LayerSettings", t => t.LayerSettings_Guid)
                .ForeignKey("dbo.Perimeter", t => t.Perimeter_Guid)
                .ForeignKey("dbo.Scenario", t => t.Scenario_Guid)
                .Index(t => t.LayerSettings_Guid)
                .Index(t => t.Perimeter_Guid)
                .Index(t => t.Scenario_Guid);
            
            CreateTable(
                "dbo.ShipTrack",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        OverrideTimestamps = c.Boolean(nullable: false),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.Platform", t => t.Guid)
                .Index(t => t.Guid);
            
            CreateTable(
                "dbo.Waypoint",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        Geo_Latitude = c.Double(nullable: false),
                        Geo_Longitude = c.Double(nullable: false),
                        TimeAtWaypoint_Ticks = c.Long(nullable: false),
                        Order = c.Int(nullable: false),
                        ShipTrack_Guid = c.Guid(),
                        ShipTrack_Guid1 = c.Guid(nullable: false),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.ShipTrack", t => t.ShipTrack_Guid)
                .ForeignKey("dbo.ShipTrack", t => t.ShipTrack_Guid1, cascadeDelete: true)
                .Index(t => t.ShipTrack_Guid)
                .Index(t => t.ShipTrack_Guid1);
            
            CreateTable(
                "dbo.Source",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        PSMSourceGuid = c.String(maxLength: 4000),
                        SourceName = c.String(maxLength: 4000),
                        SourceType = c.String(maxLength: 4000),
                        Platform_Guid = c.Guid(nullable: false),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.Platform", t => t.Platform_Guid, cascadeDelete: true)
                .Index(t => t.Platform_Guid);
            
            CreateTable(
                "dbo.ScenarioSpecies",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        SpeciesFile = c.String(maxLength: 4000),
                        LatinName = c.String(maxLength: 4000),
                        PopulationDensity = c.Single(nullable: false),
                        SpeciesDefinitionFilename = c.String(maxLength: 4000),
                        PopulationFilename = c.String(maxLength: 4000),
                        LayerSettings_Guid = c.Guid(),
                        Scenario_Guid = c.Guid(),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.LayerSettings", t => t.LayerSettings_Guid)
                .ForeignKey("dbo.Scenario", t => t.Scenario_Guid)
                .Index(t => t.LayerSettings_Guid)
                .Index(t => t.Scenario_Guid);
            
            CreateTable(
                "dbo.TransmissionLoss",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        IsReadyToCalculate = c.Boolean(nullable: false),
                        AnalysisPoint_Guid = c.Guid(),
                        LayerSettings_Guid = c.Guid(),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.AnalysisPoint", t => t.AnalysisPoint_Guid)
                .ForeignKey("dbo.LayerSettings", t => t.LayerSettings_Guid)
                .Index(t => t.AnalysisPoint_Guid)
                .Index(t => t.LayerSettings_Guid);
            
            CreateTable(
                "dbo.Radial",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        IsCalculated = c.Boolean(nullable: false),
                        Filename = c.String(maxLength: 4000),
                        CalculationStarted_Ticks = c.Long(nullable: false),
                        CalculationCompleted_Ticks = c.Long(nullable: false),
                        Bearing = c.Double(nullable: false),
                        Length = c.Double(nullable: false),
                        TransmissionLoss_Guid = c.Guid(),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.TransmissionLoss", t => t.TransmissionLoss_Guid)
                .Index(t => t.TransmissionLoss_Guid);
            
            CreateTable(
                "dbo.TransmissionLossMode",
                c => new
                    {
                        TransmissionLoss_Guid = c.Guid(nullable: false),
                        Mode_Guid = c.Guid(nullable: false),
                    })
                .PrimaryKey(t => new { t.TransmissionLoss_Guid, t.Mode_Guid })
                .ForeignKey("dbo.TransmissionLoss", t => t.TransmissionLoss_Guid, cascadeDelete: true)
                .ForeignKey("dbo.Mode", t => t.Mode_Guid, cascadeDelete: true)
                .Index(t => t.TransmissionLoss_Guid)
                .Index(t => t.Mode_Guid);
            
        }
        
        public override void Down()
        {
            DropForeignKey("dbo.Mode", "Source_Guid", "dbo.Source");
            DropForeignKey("dbo.LogEntry", "TransmissionLoss_Guid", "dbo.TransmissionLoss");
            DropForeignKey("dbo.LogEntry", "Radial_Guid", "dbo.Radial");
            DropForeignKey("dbo.LogEntry", "Mode_Guid", "dbo.Mode");
            DropForeignKey("dbo.LogEntry", "AnalysisPoint_Guid", "dbo.AnalysisPoint");
            DropForeignKey("dbo.Radial", "TransmissionLoss_Guid", "dbo.TransmissionLoss");
            DropForeignKey("dbo.TransmissionLossMode", "Mode_Guid", "dbo.Mode");
            DropForeignKey("dbo.TransmissionLossMode", "TransmissionLoss_Guid", "dbo.TransmissionLoss");
            DropForeignKey("dbo.TransmissionLoss", "LayerSettings_Guid", "dbo.LayerSettings");
            DropForeignKey("dbo.TransmissionLoss", "AnalysisPoint_Guid", "dbo.AnalysisPoint");
            DropForeignKey("dbo.Scenario", "Wind_Guid", "dbo.EnvironmentalDataSet");
            DropForeignKey("dbo.Scenario", "SoundSpeed_Guid", "dbo.EnvironmentalDataSet");
            DropForeignKey("dbo.Scenario", "Sediment_Guid", "dbo.EnvironmentalDataSet");
            DropForeignKey("dbo.ScenarioSpecies", "Scenario_Guid", "dbo.Scenario");
            DropForeignKey("dbo.LogEntry", "ScenarioSpecies_Guid", "dbo.ScenarioSpecies");
            DropForeignKey("dbo.ScenarioSpecies", "LayerSettings_Guid", "dbo.LayerSettings");
            DropForeignKey("dbo.Source", "Platform_Guid", "dbo.Platform");
            DropForeignKey("dbo.LogEntry", "Source_Guid", "dbo.Source");
            DropForeignKey("dbo.ShipTrack", "Guid", "dbo.Platform");
            DropForeignKey("dbo.Waypoint", "ShipTrack_Guid1", "dbo.ShipTrack");
            DropForeignKey("dbo.Waypoint", "ShipTrack_Guid", "dbo.ShipTrack");
            DropForeignKey("dbo.LogEntry", "ShipTrack_Guid", "dbo.ShipTrack");
            DropForeignKey("dbo.Platform", "Scenario_Guid", "dbo.Scenario");
            DropForeignKey("dbo.Platform", "Perimeter_Guid", "dbo.Perimeter");
            DropForeignKey("dbo.LogEntry", "Platform_Guid", "dbo.Platform");
            DropForeignKey("dbo.Platform", "LayerSettings_Guid", "dbo.LayerSettings");
            DropForeignKey("dbo.Perimeter", "Scenario_Guid", "dbo.Scenario");
            DropForeignKey("dbo.PerimeterCoordinate", "Perimeter_Guid", "dbo.Perimeter");
            DropForeignKey("dbo.LogEntry", "Perimeter_Guid", "dbo.Perimeter");
            DropForeignKey("dbo.Perimeter", "LayerSettings_Guid", "dbo.LayerSettings");
            DropForeignKey("dbo.LogEntry", "Scenario_Guid", "dbo.Scenario");
            DropForeignKey("dbo.Scenario", "Bathymetry_Guid", "dbo.EnvironmentalDataSet");
            DropForeignKey("dbo.LogEntry", "EnvironmentalDataSet_Guid", "dbo.EnvironmentalDataSet");
            DropForeignKey("dbo.Scenario", "Location_Guid", "dbo.Location");
            DropForeignKey("dbo.LogEntry", "Location_Guid", "dbo.Location");
            DropForeignKey("dbo.Location", "LayerSettings_Guid", "dbo.LayerSettings");
            DropForeignKey("dbo.EnvironmentalDataSet", "Location_Guid", "dbo.Location");
            DropForeignKey("dbo.EnvironmentalDataSet", "LayerSettings_Guid", "dbo.LayerSettings");
            DropForeignKey("dbo.AnalysisPoint", "Scenario_Guid", "dbo.Scenario");
            DropForeignKey("dbo.AnalysisPoint", "LayerSettings_Guid", "dbo.LayerSettings");
            DropIndex("dbo.TransmissionLossMode", new[] { "Mode_Guid" });
            DropIndex("dbo.TransmissionLossMode", new[] { "TransmissionLoss_Guid" });
            DropIndex("dbo.Radial", new[] { "TransmissionLoss_Guid" });
            DropIndex("dbo.TransmissionLoss", new[] { "LayerSettings_Guid" });
            DropIndex("dbo.TransmissionLoss", new[] { "AnalysisPoint_Guid" });
            DropIndex("dbo.ScenarioSpecies", new[] { "Scenario_Guid" });
            DropIndex("dbo.ScenarioSpecies", new[] { "LayerSettings_Guid" });
            DropIndex("dbo.Source", new[] { "Platform_Guid" });
            DropIndex("dbo.Waypoint", new[] { "ShipTrack_Guid1" });
            DropIndex("dbo.Waypoint", new[] { "ShipTrack_Guid" });
            DropIndex("dbo.ShipTrack", new[] { "Guid" });
            DropIndex("dbo.Platform", new[] { "Scenario_Guid" });
            DropIndex("dbo.Platform", new[] { "Perimeter_Guid" });
            DropIndex("dbo.Platform", new[] { "LayerSettings_Guid" });
            DropIndex("dbo.PerimeterCoordinate", new[] { "Perimeter_Guid" });
            DropIndex("dbo.Perimeter", new[] { "Scenario_Guid" });
            DropIndex("dbo.Perimeter", new[] { "LayerSettings_Guid" });
            DropIndex("dbo.Location", new[] { "LayerSettings_Guid" });
            DropIndex("dbo.EnvironmentalDataSet", new[] { "Location_Guid" });
            DropIndex("dbo.EnvironmentalDataSet", new[] { "LayerSettings_Guid" });
            DropIndex("dbo.Scenario", new[] { "Wind_Guid" });
            DropIndex("dbo.Scenario", new[] { "SoundSpeed_Guid" });
            DropIndex("dbo.Scenario", new[] { "Sediment_Guid" });
            DropIndex("dbo.Scenario", new[] { "Bathymetry_Guid" });
            DropIndex("dbo.Scenario", new[] { "Location_Guid" });
            DropIndex("dbo.AnalysisPoint", new[] { "Scenario_Guid" });
            DropIndex("dbo.AnalysisPoint", new[] { "LayerSettings_Guid" });
            DropIndex("dbo.LogEntry", new[] { "TransmissionLoss_Guid" });
            DropIndex("dbo.LogEntry", new[] { "Radial_Guid" });
            DropIndex("dbo.LogEntry", new[] { "Mode_Guid" });
            DropIndex("dbo.LogEntry", new[] { "AnalysisPoint_Guid" });
            DropIndex("dbo.LogEntry", new[] { "ScenarioSpecies_Guid" });
            DropIndex("dbo.LogEntry", new[] { "Source_Guid" });
            DropIndex("dbo.LogEntry", new[] { "ShipTrack_Guid" });
            DropIndex("dbo.LogEntry", new[] { "Platform_Guid" });
            DropIndex("dbo.LogEntry", new[] { "Perimeter_Guid" });
            DropIndex("dbo.LogEntry", new[] { "Scenario_Guid" });
            DropIndex("dbo.LogEntry", new[] { "EnvironmentalDataSet_Guid" });
            DropIndex("dbo.LogEntry", new[] { "Location_Guid" });
            DropIndex("dbo.Mode", new[] { "Source_Guid" });
            DropTable("dbo.TransmissionLossMode");
            DropTable("dbo.Radial");
            DropTable("dbo.TransmissionLoss");
            DropTable("dbo.ScenarioSpecies");
            DropTable("dbo.Source");
            DropTable("dbo.Waypoint");
            DropTable("dbo.ShipTrack");
            DropTable("dbo.Platform");
            DropTable("dbo.PerimeterCoordinate");
            DropTable("dbo.Perimeter");
            DropTable("dbo.Location");
            DropTable("dbo.EnvironmentalDataSet");
            DropTable("dbo.Scenario");
            DropTable("dbo.LayerSettings");
            DropTable("dbo.AnalysisPoint");
            DropTable("dbo.LogEntry");
            DropTable("dbo.Mode");
        }
    }
}
