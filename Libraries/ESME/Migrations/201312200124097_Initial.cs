namespace ESME.Migrations
{
    using System;
    using System.Data.Entity.Migrations;
    
    public partial class Initial : DbMigration
    {
        public override void Up()
        {
            CreateTable(
                "dbo.AnalysisPoints",
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
                .ForeignKey("dbo.Scenarios", t => t.Scenario_Guid)
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
                "dbo.Scenarios",
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
                .ForeignKey("dbo.Locations", t => t.Location_Guid)
                .ForeignKey("dbo.EnvironmentalDataSets", t => t.Bathymetry_Guid)
                .ForeignKey("dbo.EnvironmentalDataSets", t => t.Sediment_Guid)
                .ForeignKey("dbo.EnvironmentalDataSets", t => t.SoundSpeed_Guid)
                .ForeignKey("dbo.EnvironmentalDataSets", t => t.Wind_Guid)
                .Index(t => t.Location_Guid)
                .Index(t => t.Bathymetry_Guid)
                .Index(t => t.Sediment_Guid)
                .Index(t => t.SoundSpeed_Guid)
                .Index(t => t.Wind_Guid);
            
            CreateTable(
                "dbo.EnvironmentalDataSets",
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
                .ForeignKey("dbo.Locations", t => t.Location_Guid)
                .Index(t => t.LayerSettings_Guid)
                .Index(t => t.Location_Guid);
            
            CreateTable(
                "dbo.Locations",
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
                "dbo.LogEntries",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        MessageSource_Who = c.String(maxLength: 4000),
                        MessageSource_When_Ticks = c.Long(nullable: false),
                        MessageSource_Where = c.String(maxLength: 4000),
                        Message = c.String(maxLength: 4000),
                        AnalysisPoint_Guid = c.Guid(),
                        EnvironmentalDataSet_Guid = c.Guid(),
                        Location_Guid = c.Guid(),
                        Mode_Guid = c.Guid(),
                        Source_Guid = c.Guid(),
                        Platform_Guid = c.Guid(),
                        Perimeter_Guid = c.Guid(),
                        ShipTrack_Guid = c.Guid(),
                        Radial_Guid = c.Guid(),
                        Scenario_Guid = c.Guid(),
                        ScenarioSpecies_Guid = c.Guid(),
                        TransmissionLoss_Guid = c.Guid(),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.AnalysisPoints", t => t.AnalysisPoint_Guid)
                .ForeignKey("dbo.EnvironmentalDataSets", t => t.EnvironmentalDataSet_Guid)
                .ForeignKey("dbo.Locations", t => t.Location_Guid)
                .ForeignKey("dbo.Modes", t => t.Mode_Guid)
                .ForeignKey("dbo.Sources", t => t.Source_Guid)
                .ForeignKey("dbo.Platforms", t => t.Platform_Guid)
                .ForeignKey("dbo.Perimeters", t => t.Perimeter_Guid)
                .ForeignKey("dbo.ShipTracks", t => t.ShipTrack_Guid)
                .ForeignKey("dbo.Radials", t => t.Radial_Guid)
                .ForeignKey("dbo.Scenarios", t => t.Scenario_Guid)
                .ForeignKey("dbo.ScenarioSpecies", t => t.ScenarioSpecies_Guid)
                .ForeignKey("dbo.TransmissionLosses", t => t.TransmissionLoss_Guid)
                .Index(t => t.AnalysisPoint_Guid)
                .Index(t => t.EnvironmentalDataSet_Guid)
                .Index(t => t.Location_Guid)
                .Index(t => t.Mode_Guid)
                .Index(t => t.Source_Guid)
                .Index(t => t.Platform_Guid)
                .Index(t => t.Perimeter_Guid)
                .Index(t => t.ShipTrack_Guid)
                .Index(t => t.Radial_Guid)
                .Index(t => t.Scenario_Guid)
                .Index(t => t.ScenarioSpecies_Guid)
                .Index(t => t.TransmissionLoss_Guid);
            
            CreateTable(
                "dbo.Modes",
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
                        Source_Guid = c.Guid(),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.Sources", t => t.Source_Guid)
                .Index(t => t.Source_Guid);
            
            CreateTable(
                "dbo.Sources",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        PSMSourceGuid = c.String(maxLength: 4000),
                        SourceName = c.String(maxLength: 4000),
                        SourceType = c.String(maxLength: 4000),
                        Platform_Guid = c.Guid(),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.Platforms", t => t.Platform_Guid)
                .Index(t => t.Platform_Guid);
            
            CreateTable(
                "dbo.Platforms",
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
                .ForeignKey("dbo.Perimeters", t => t.Perimeter_Guid)
                .ForeignKey("dbo.Scenarios", t => t.Scenario_Guid)
                .Index(t => t.LayerSettings_Guid)
                .Index(t => t.Perimeter_Guid)
                .Index(t => t.Scenario_Guid);
            
            CreateTable(
                "dbo.Perimeters",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        Name = c.String(maxLength: 4000),
                        LayerSettings_Guid = c.Guid(),
                        Scenario_Guid = c.Guid(),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.LayerSettings", t => t.LayerSettings_Guid)
                .ForeignKey("dbo.Scenarios", t => t.Scenario_Guid)
                .Index(t => t.LayerSettings_Guid)
                .Index(t => t.Scenario_Guid);
            
            CreateTable(
                "dbo.PerimeterCoordinates",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        Order = c.Int(nullable: false),
                        Geo_Latitude = c.Double(nullable: false),
                        Geo_Longitude = c.Double(nullable: false),
                        Perimeter_Guid = c.Guid(),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.Perimeters", t => t.Perimeter_Guid)
                .Index(t => t.Perimeter_Guid);
            
            CreateTable(
                "dbo.ShipTracks",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        OverrideTimestamps = c.Boolean(nullable: false),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.Platforms", t => t.Guid)
                .Index(t => t.Guid);
            
            CreateTable(
                "dbo.Waypoints",
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
                .ForeignKey("dbo.ShipTracks", t => t.ShipTrack_Guid)
                .ForeignKey("dbo.ShipTracks", t => t.ShipTrack_Guid1, cascadeDelete: true)
                .Index(t => t.ShipTrack_Guid)
                .Index(t => t.ShipTrack_Guid1);
            
            CreateTable(
                "dbo.TransmissionLosses",
                c => new
                    {
                        Guid = c.Guid(nullable: false),
                        IsReadyToCalculate = c.Boolean(nullable: false),
                        AnalysisPoint_Guid = c.Guid(),
                        LayerSettings_Guid = c.Guid(),
                    })
                .PrimaryKey(t => t.Guid)
                .ForeignKey("dbo.AnalysisPoints", t => t.AnalysisPoint_Guid)
                .ForeignKey("dbo.LayerSettings", t => t.LayerSettings_Guid)
                .Index(t => t.AnalysisPoint_Guid)
                .Index(t => t.LayerSettings_Guid);
            
            CreateTable(
                "dbo.Radials",
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
                .ForeignKey("dbo.TransmissionLosses", t => t.TransmissionLoss_Guid)
                .Index(t => t.TransmissionLoss_Guid);
            
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
                .ForeignKey("dbo.Scenarios", t => t.Scenario_Guid)
                .Index(t => t.LayerSettings_Guid)
                .Index(t => t.Scenario_Guid);
            
            CreateTable(
                "dbo.TransmissionLossModes",
                c => new
                    {
                        TransmissionLoss_Guid = c.Guid(nullable: false),
                        Mode_Guid = c.Guid(nullable: false),
                    })
                .PrimaryKey(t => new { t.TransmissionLoss_Guid, t.Mode_Guid })
                .ForeignKey("dbo.TransmissionLosses", t => t.TransmissionLoss_Guid, cascadeDelete: true)
                .ForeignKey("dbo.Modes", t => t.Mode_Guid, cascadeDelete: true)
                .Index(t => t.TransmissionLoss_Guid)
                .Index(t => t.Mode_Guid);
            
        }
        
        public override void Down()
        {
            DropForeignKey("dbo.Scenarios", "Wind_Guid", "dbo.EnvironmentalDataSets");
            DropForeignKey("dbo.Scenarios", "SoundSpeed_Guid", "dbo.EnvironmentalDataSets");
            DropForeignKey("dbo.Scenarios", "Sediment_Guid", "dbo.EnvironmentalDataSets");
            DropForeignKey("dbo.Scenarios", "Bathymetry_Guid", "dbo.EnvironmentalDataSets");
            DropForeignKey("dbo.Scenarios", "Location_Guid", "dbo.Locations");
            DropForeignKey("dbo.LogEntries", "TransmissionLoss_Guid", "dbo.TransmissionLosses");
            DropForeignKey("dbo.ScenarioSpecies", "Scenario_Guid", "dbo.Scenarios");
            DropForeignKey("dbo.LogEntries", "ScenarioSpecies_Guid", "dbo.ScenarioSpecies");
            DropForeignKey("dbo.ScenarioSpecies", "LayerSettings_Guid", "dbo.LayerSettings");
            DropForeignKey("dbo.LogEntries", "Scenario_Guid", "dbo.Scenarios");
            DropForeignKey("dbo.LogEntries", "Radial_Guid", "dbo.Radials");
            DropForeignKey("dbo.Radials", "TransmissionLoss_Guid", "dbo.TransmissionLosses");
            DropForeignKey("dbo.TransmissionLossModes", "Mode_Guid", "dbo.Modes");
            DropForeignKey("dbo.TransmissionLossModes", "TransmissionLoss_Guid", "dbo.TransmissionLosses");
            DropForeignKey("dbo.TransmissionLosses", "LayerSettings_Guid", "dbo.LayerSettings");
            DropForeignKey("dbo.TransmissionLosses", "AnalysisPoint_Guid", "dbo.AnalysisPoints");
            DropForeignKey("dbo.Sources", "Platform_Guid", "dbo.Platforms");
            DropForeignKey("dbo.ShipTracks", "Guid", "dbo.Platforms");
            DropForeignKey("dbo.Waypoints", "ShipTrack_Guid1", "dbo.ShipTracks");
            DropForeignKey("dbo.Waypoints", "ShipTrack_Guid", "dbo.ShipTracks");
            DropForeignKey("dbo.LogEntries", "ShipTrack_Guid", "dbo.ShipTracks");
            DropForeignKey("dbo.Platforms", "Scenario_Guid", "dbo.Scenarios");
            DropForeignKey("dbo.Platforms", "Perimeter_Guid", "dbo.Perimeters");
            DropForeignKey("dbo.Perimeters", "Scenario_Guid", "dbo.Scenarios");
            DropForeignKey("dbo.PerimeterCoordinates", "Perimeter_Guid", "dbo.Perimeters");
            DropForeignKey("dbo.LogEntries", "Perimeter_Guid", "dbo.Perimeters");
            DropForeignKey("dbo.Perimeters", "LayerSettings_Guid", "dbo.LayerSettings");
            DropForeignKey("dbo.LogEntries", "Platform_Guid", "dbo.Platforms");
            DropForeignKey("dbo.Platforms", "LayerSettings_Guid", "dbo.LayerSettings");
            DropForeignKey("dbo.Modes", "Source_Guid", "dbo.Sources");
            DropForeignKey("dbo.LogEntries", "Source_Guid", "dbo.Sources");
            DropForeignKey("dbo.LogEntries", "Mode_Guid", "dbo.Modes");
            DropForeignKey("dbo.LogEntries", "Location_Guid", "dbo.Locations");
            DropForeignKey("dbo.LogEntries", "EnvironmentalDataSet_Guid", "dbo.EnvironmentalDataSets");
            DropForeignKey("dbo.LogEntries", "AnalysisPoint_Guid", "dbo.AnalysisPoints");
            DropForeignKey("dbo.Locations", "LayerSettings_Guid", "dbo.LayerSettings");
            DropForeignKey("dbo.EnvironmentalDataSets", "Location_Guid", "dbo.Locations");
            DropForeignKey("dbo.EnvironmentalDataSets", "LayerSettings_Guid", "dbo.LayerSettings");
            DropForeignKey("dbo.AnalysisPoints", "Scenario_Guid", "dbo.Scenarios");
            DropForeignKey("dbo.AnalysisPoints", "LayerSettings_Guid", "dbo.LayerSettings");
            DropIndex("dbo.Scenarios", new[] { "Wind_Guid" });
            DropIndex("dbo.Scenarios", new[] { "SoundSpeed_Guid" });
            DropIndex("dbo.Scenarios", new[] { "Sediment_Guid" });
            DropIndex("dbo.Scenarios", new[] { "Bathymetry_Guid" });
            DropIndex("dbo.Scenarios", new[] { "Location_Guid" });
            DropIndex("dbo.LogEntries", new[] { "TransmissionLoss_Guid" });
            DropIndex("dbo.ScenarioSpecies", new[] { "Scenario_Guid" });
            DropIndex("dbo.LogEntries", new[] { "ScenarioSpecies_Guid" });
            DropIndex("dbo.ScenarioSpecies", new[] { "LayerSettings_Guid" });
            DropIndex("dbo.LogEntries", new[] { "Scenario_Guid" });
            DropIndex("dbo.LogEntries", new[] { "Radial_Guid" });
            DropIndex("dbo.Radials", new[] { "TransmissionLoss_Guid" });
            DropIndex("dbo.TransmissionLossModes", new[] { "Mode_Guid" });
            DropIndex("dbo.TransmissionLossModes", new[] { "TransmissionLoss_Guid" });
            DropIndex("dbo.TransmissionLosses", new[] { "LayerSettings_Guid" });
            DropIndex("dbo.TransmissionLosses", new[] { "AnalysisPoint_Guid" });
            DropIndex("dbo.Sources", new[] { "Platform_Guid" });
            DropIndex("dbo.ShipTracks", new[] { "Guid" });
            DropIndex("dbo.Waypoints", new[] { "ShipTrack_Guid1" });
            DropIndex("dbo.Waypoints", new[] { "ShipTrack_Guid" });
            DropIndex("dbo.LogEntries", new[] { "ShipTrack_Guid" });
            DropIndex("dbo.Platforms", new[] { "Scenario_Guid" });
            DropIndex("dbo.Platforms", new[] { "Perimeter_Guid" });
            DropIndex("dbo.Perimeters", new[] { "Scenario_Guid" });
            DropIndex("dbo.PerimeterCoordinates", new[] { "Perimeter_Guid" });
            DropIndex("dbo.LogEntries", new[] { "Perimeter_Guid" });
            DropIndex("dbo.Perimeters", new[] { "LayerSettings_Guid" });
            DropIndex("dbo.LogEntries", new[] { "Platform_Guid" });
            DropIndex("dbo.Platforms", new[] { "LayerSettings_Guid" });
            DropIndex("dbo.Modes", new[] { "Source_Guid" });
            DropIndex("dbo.LogEntries", new[] { "Source_Guid" });
            DropIndex("dbo.LogEntries", new[] { "Mode_Guid" });
            DropIndex("dbo.LogEntries", new[] { "Location_Guid" });
            DropIndex("dbo.LogEntries", new[] { "EnvironmentalDataSet_Guid" });
            DropIndex("dbo.LogEntries", new[] { "AnalysisPoint_Guid" });
            DropIndex("dbo.Locations", new[] { "LayerSettings_Guid" });
            DropIndex("dbo.EnvironmentalDataSets", new[] { "Location_Guid" });
            DropIndex("dbo.EnvironmentalDataSets", new[] { "LayerSettings_Guid" });
            DropIndex("dbo.AnalysisPoints", new[] { "Scenario_Guid" });
            DropIndex("dbo.AnalysisPoints", new[] { "LayerSettings_Guid" });
            DropTable("dbo.TransmissionLossModes");
            DropTable("dbo.ScenarioSpecies");
            DropTable("dbo.Radials");
            DropTable("dbo.TransmissionLosses");
            DropTable("dbo.Waypoints");
            DropTable("dbo.ShipTracks");
            DropTable("dbo.PerimeterCoordinates");
            DropTable("dbo.Perimeters");
            DropTable("dbo.Platforms");
            DropTable("dbo.Sources");
            DropTable("dbo.Modes");
            DropTable("dbo.LogEntries");
            DropTable("dbo.Locations");
            DropTable("dbo.EnvironmentalDataSets");
            DropTable("dbo.Scenarios");
            DropTable("dbo.LayerSettings");
            DropTable("dbo.AnalysisPoints");
        }
    }
}
