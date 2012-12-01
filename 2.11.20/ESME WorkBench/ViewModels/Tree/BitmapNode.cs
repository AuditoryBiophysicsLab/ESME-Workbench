using ESME;
using ESME.Locations;
using HRC.ViewModels;
using HRC.WPF;

namespace ESMEWorkbench.ViewModels.Tree
{
    public class BitmapNode : ViewModelBase
    {
        public BitmapNode(string layerName, EnvironmentalDataSet bitmapData)
        {
            LayerName = layerName;
            BitmapData = bitmapData;
        }

        public string LayerName { get; private set; }
        public bool IsChecked
        {
            get { return BitmapData.LayerSettings.IsChecked; }
            set { BitmapData.LayerSettings.IsChecked = value; }
        }

        public EnvironmentalDataSet BitmapData { get; set; }
        #region Layer Move commands
        #region MoveLayerToFrontCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerToFrontCommand { get { return _moveLayerToFront ?? (_moveLayerToFront = new SimpleCommand<object, EventToCommandArgs>(MoveLayerToFront)); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerToFront;
        void MoveLayerToFront(EventToCommandArgs args)
        {
            BitmapData.LayerSettings.MoveLayerToFront();
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }
        #endregion

        #region MoveLayerForwardCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerForwardCommand { get { return _moveLayerForward ?? (_moveLayerForward = new SimpleCommand<object, EventToCommandArgs>(MoveLayerForward)); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerForward;
        void MoveLayerForward(EventToCommandArgs args)
        {
            BitmapData.LayerSettings.MoveLayerForward();
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }
        #endregion

        #region MoveLayerBackwardCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerBackwardCommand { get { return _moveLayerBackward ?? (_moveLayerBackward = new SimpleCommand<object, EventToCommandArgs>(MoveLayerBackward)); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerBackward;
        void MoveLayerBackward(EventToCommandArgs args)
        {
            BitmapData.LayerSettings.MoveLayerBackward();
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }
        #endregion

        #region MoveLayerToBackCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerToBackCommand { get { return _moveLayerToBack ?? (_moveLayerToBack = new SimpleCommand<object, EventToCommandArgs>(MoveLayerToBack)); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerToBack;
        void MoveLayerToBack(EventToCommandArgs args)
        {
            BitmapData.LayerSettings.MoveLayerToBack();
            MediatorMessage.Send(MediatorMessage.RefreshMap, true);
        }
        #endregion
        #endregion
    }
}