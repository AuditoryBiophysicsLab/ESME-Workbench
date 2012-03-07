using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using Cinch;

namespace ESMEWorkbench.ViewModels.Main
{
    public class NewLocationViewModel : ViewModelBase
    {
        public NewLocationViewModel() 
        {
            
        }

        #region public double North { get; set; }

        public double North
        {
            get { return _north; }
            set
            {
                _north = value;
                NotifyPropertyChanged(NorthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NorthChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.North);
        double _north;

        #endregion
        #region public double South { get; set; }

        public double South
        {
            get { return _south; }
            set
            {
                _south = value;
                NotifyPropertyChanged(SouthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SouthChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.South);
        double _south;

        #endregion
        #region public double East { get; set; }

        public double East
        {
            get { return _east; }
            set
            {
                _east = value;
                NotifyPropertyChanged(EastChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs EastChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.East);
        double _east;

        #endregion
        #region public double West { get; set; }

        public double West
        {
            get { return _west; }
            set
            {
                _west = value;
                NotifyPropertyChanged(WestChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs WestChangedEventArgs = ObservableHelper.CreateArgs<NewLocationViewModel>(x => x.West);
        double _west;

        #endregion
    }
}
