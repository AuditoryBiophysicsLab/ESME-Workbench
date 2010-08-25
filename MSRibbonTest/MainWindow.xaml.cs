using System.Windows;
using Microsoft.Windows.Controls.Ribbon;
using ThinkGeo.MapSuite.Core;
using ThinkGeo.MapSuite.WpfDesktopEdition;

namespace ESMERibbonDemo
{
	/// <summary>
	/// Interaction logic for MainWindow.xaml
	/// </summary>
	public partial class MainWindow : RibbonWindow
	{
		public MainWindow()
		{
			InitializeComponent();

			// Insert code required on object creation below this point.
		}

#if false
 	    private void MapLoaded(object sender, RoutedEventArgs e)
		{
			Map1.MapUnit = GeographyUnit.DecimalDegree;
		    Map1.MapTools.PanZoomBar.HorizontalAlignment = HorizontalAlignment.Right;
		    Map1.MapTools.PanZoomBar.VerticalAlignment = VerticalAlignment.Bottom;

			var worldLayer = new ShapeFileFeatureLayer(@"Sample GIS Data\Countries02.shp");
		    worldLayer.ZoomLevelSet.ZoomLevel01.DefaultAreaStyle = AreaStyles.Country1;
			worldLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;


			var capitalLayer = new ShapeFileFeatureLayer(@"Sample GIS Data\WorldCapitals.shp");
			// Similarly, we use the presetPointStyle for cities.     
			capitalLayer.ZoomLevelSet.ZoomLevel01.DefaultPointStyle = PointStyles.Capital3;
			// This setting also applies from ZoonLevel01 to ZoomLevel20, that means we can see city symbols the same style with ZoomLevel01 all the time. 
			capitalLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;

			// We create a new Layer for labeling the capitals.
			var capitalLabelLayer = new ShapeFileFeatureLayer(@"Sample GIS Data\WorldCapitals.shp");
			// We use the preset TextStyle. Here we passed in the “CITY_NAME”, which is the name of the field we want to label on map.
			capitalLabelLayer.ZoomLevelSet.ZoomLevel01.DefaultTextStyle = TextStyles.Capital3("CITY_NAME");
			capitalLabelLayer.ZoomLevelSet.ZoomLevel01.ApplyUntilZoomLevel = ApplyUntilZoomLevel.Level20;
			// As the map is drawn by tiles, it needs to draw on the margin to make sure the text is complete after we joining the tiles together.
			// Change the number to another one (for example 0) and you can see the difference expecially when panning.
			capitalLabelLayer.DrawingMarginPercentage = 50;

			//GeoTiffRasterLayer rasterLayer = new GeoTiffRasterLayer(@"C:\Users\Dave Anderson\Desktop\ETOPO1_Ice_c.tif", new RectangleShape(-180, 90, 180, -90));
			//GdiPlusRasterSource source = new GdiPlusRasterSource();
			//var rasterLayer = new GdiPlusRasterLayer(@"C:\Users\Dave Anderson\Downloads\world.topo.bathy.200407.3x21600x10800.jpg", new RectangleShape(-180, 90, 180, -90));
			//rasterLayer.IsGrayscale = false;

			var layerOverlay = new LayerOverlay();
			// We need to add both of the new layers to the Layer OverLay. 
			//layerOverlay.Layers.Add(rasterLayer);
			layerOverlay.Layers.Add(worldLayer);
			layerOverlay.Layers.Add(capitalLayer);
			layerOverlay.Layers.Add(capitalLabelLayer);

			Map1.Overlays.Add(layerOverlay);
			Map1.CurrentExtent = new RectangleShape(-180, 90, 180, -90);

			Map1.Refresh();
		}
#endif
    }
}
