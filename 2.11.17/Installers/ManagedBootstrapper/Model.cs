using System;
using System.Diagnostics;
using System.Net;
using System.Reflection;
using Microsoft.Tools.WindowsInstallerXml.Bootstrapper;

namespace WixBootstrapper
{
    /// <summary>
    /// The model.
    /// </summary>
    public class Model
    {
        private Version _version;

        /// <summary>
        /// Creates a new model for the UX.
        /// </summary>
        /// <param name="bootstrapper">Bootstrapper hosting the UX.</param>
        public Model(BootstrapperApplication bootstrapper)
        {
            Bootstrapper = bootstrapper;
        }

        /// <summary>
        /// Gets the bootstrapper.
        /// </summary>
        public BootstrapperApplication Bootstrapper { get; private set; }

        /// <summary>
        /// Gets the bootstrapper command-line.
        /// </summary>
        public Command Command { get { return Bootstrapper.Command; } }

        /// <summary>
        /// Gets the bootstrapper engine.
        /// </summary>
        public Engine Engine { get { return Bootstrapper.Engine; } }

        /// <summary>
        /// Get the version of the install.
        /// </summary>
        public Version Version
        {
            get
            {
                if (null == _version)
                {
                    var assembly = Assembly.GetExecutingAssembly();
                    var fileVersion = FileVersionInfo.GetVersionInfo(assembly.Location);

                    _version = new Version(fileVersion.FileVersion);
                }

                return _version;
            }
        }

        /// <summary>
        /// Creates a correctly configured HTTP web request.
        /// </summary>
        /// <param name="uri">URI to connect to.</param>
        /// <returns>Correctly configured HTTP web request.</returns>
        public HttpWebRequest CreateWebRequest(string uri)
        {
            var request = (HttpWebRequest)WebRequest.Create(uri);
            request.UserAgent = String.Concat("WixInstall", Version.ToString());

            return request;
        }
    }
}
