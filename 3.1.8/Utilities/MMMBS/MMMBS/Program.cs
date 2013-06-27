using System;
using System.Collections.Generic;
using System.Windows.Forms;
using MMMBSLib; // mmmb C# code code and data types.

namespace MBSGUI
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new FormSpeciesDefinition());
        }
    }
}