using System.Diagnostics;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using HRC.Utility;

namespace HRC.MSTest.Utility
{
    [TestClass]
    public class OnlineUpdaterTest
    {
        [TestMethod]
        async public Task CheckForUpdateTest()
        {
            var onlineUpdater = new OnlineUpdater();
            await onlineUpdater.CheckForUpdate("http://esme.bu.edu/download/version.xml");
            Debug.WriteLine(string.Format("Product name: {0}", onlineUpdater.ProductName));
            Debug.WriteLine(string.Format("Installer filename: {0}", onlineUpdater.InstallerFilename));
            Debug.WriteLine(string.Format("Version number: {0}", onlineUpdater.ProductVersion));
            Debug.WriteLine(string.Format("Installer MD5 Checksum (base64): {0}", onlineUpdater.InstallerMD5SumBase64));
            Debug.WriteLine(string.Format("Version file URL: {0}", onlineUpdater.VersionFileUrl));
            Debug.WriteLine(string.Format("Installer file URL: {0}", onlineUpdater.InstallerFileUrl));
            try
            {
                string updateInstaller;
                updateInstaller = await onlineUpdater.DownloadAndVerifyUpdate();
                Debug.WriteLine("Downloaded update installer " + updateInstaller);
            }
            catch (ChecksumMismatchException cs)
            {
                Debug.WriteLine("Caught ChecksumMismatchException: " + cs.Message);
            }
            catch (FileVersionMismatchException fv)
            {
                Debug.WriteLine("Caught FileVersionMismatchException: " + fv.Message);
            }
        }
    }
}
