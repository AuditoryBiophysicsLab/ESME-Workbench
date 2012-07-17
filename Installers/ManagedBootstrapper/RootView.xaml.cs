//-------------------------------------------------------------------------------------------------
// <copyright file="RootView.xaml.cs" company="Microsoft">
// Copyright (c) Microsoft Corporation. All rights reserved.
//    
//    The use and distribution terms for this software are covered by the
//    Common Public License 1.0 (http://opensource.org/licenses/cpl1.0.php)
//    which can be found in the file CPL.TXT at the root of this distribution.
//    By using this software in any fashion, you are agreeing to be bound by
//    the terms of this license.
//    
//    You must not remove this notice, or any other, from this software.
// </copyright>
// 
// <summary>
// View for WiX UX.
// </summary>
//-------------------------------------------------------------------------------------------------

using System.Windows.Input;

namespace WixBootstrapper
{
    /// <summary>
    /// Interaction logic for View.xaml
    /// </summary>
    public partial class RootView
    {
        /// <summary>
        /// Creates the view populated with it's model.
        /// </summary>
        /// <param name="viewModel">Model for the view.</param>
        public RootView(RootViewModel viewModel)
        {
            DataContext = viewModel;

            Loaded += (sender, e) => ESMEBootstrapper.Model.Engine.CloseSplashScreen();
            Closed += (sender, e) => Dispatcher.InvokeShutdown(); // shutdown dispatcher when the window is closed.

            InitializeComponent();
        }

        /// <summary>
        /// Allows the user to drag the window around by grabbing the background rectangle.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void BackgroundMouseLeftButtonDown(object sender, MouseButtonEventArgs e)
        {
            DragMove();
        }
    }
}
