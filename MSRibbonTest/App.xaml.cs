﻿using System.Windows;
using System.Collections.Generic;
using Cinch;
using System.Reflection;

<<<<<<< .mine
namespace ESME.View
=======
namespace ESMERibbonDemo
>>>>>>> .r1384
{
    /// <summary>
    /// Interaction logic for App.xaml
    /// </summary>
    public partial class App : Application
    {
        #region Initialization
        /// <summary>
        /// Initialize Cinch using the CinchBootStrapper. 
        /// </summary>
        public App()
        {
            CinchBootStrapper.Initialise(new List<Assembly> { typeof(App).Assembly });
            InitializeComponent();
        }
        #endregion
    }
}
