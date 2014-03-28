using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Runtime.Serialization;
using System.Security.Cryptography;
using System.Threading.Tasks;
using System.Security.Cryptography.X509Certificates;
using System.Net.Security;
using System.Xml.Linq;

namespace HRC.Utility
{
    public class OnlineUpdater
    {
        public string ProductName { get; private set; }
        public string InstallerFilename { get; private set; }
        public string ProductVersion { get; private set; }
        public string InstallerMD5SumBase64 { get; private set; }
        public string VersionFileUrl { get; private set; }
        public string InstallerFileUrl { get; private set; }

        /// <summary>
        /// Check the version file at the specified URL with optional security validation
        /// </summary>
        /// <param name="versionFileUrl">
        /// The URL of the version file to check
        /// </param>
        /// <param name="validateServerCertificateChain">
        /// True to perform extra validation the server's https certificate chain, 
        /// if the url passed to CheckForUpdate starts with https://
        /// False to perform the default validation (the default validation only 
        /// validates that the server's certificate has the same hostname as the server being accessed)
        /// </param>
        /// <param name="allowSelfSignedCertificatesInServerCertificateChain">
        /// True to allow self-signed certificates in the server's certificate chain, false otherwise
        /// </param>
        /// <returns></returns>
        async public Task CheckForUpdate(string versionFileUrl, bool validateServerCertificateChain = true, bool allowSelfSignedCertificatesInServerCertificateChain = true)
        {
            if (validateServerCertificateChain) ServicePointManager.ServerCertificateValidationCallback = CertificateValidationCallback;
            _allowSelfSignedCertificatesInServerCertificateChain = allowSelfSignedCertificatesInServerCertificateChain;
            VersionFileUrl = versionFileUrl;
            var http = new HttpClient();

            var versionFileUri = new Uri(versionFileUrl);
            var versionXml = await http.GetStringAsync(versionFileUri);
            XDocument document;
            using (var reader = new StringReader(versionXml)) document = XDocument.Load(reader);
            if (document.Root != null)
            {
                ProductName = document.Root.Element("name").Value;
                var archElement = document.Root.Descendants(Environment.Is64BitProcess ? "x64" : "x86").First();
                InstallerFilename = archElement.Element("installer").Value;
                ProductVersion = archElement.Element("version").Value;
                InstallerMD5SumBase64 = archElement.Element("md5SumBase64").Value;
                InstallerFileUrl = versionFileUrl.Substring(0, versionFileUrl.LastIndexOf('/') + 1) + InstallerFilename;
            }
        }

        async public Task<string> DownloadAndVerifyUpdate()
        {
            if (InstallerFileUrl == null) throw new NullReferenceException("InstallerFileUrl cannot be null");
            if (InstallerFilename == null) throw new NullReferenceException("InstallerFilename cannot be null");
            var tmp = Path.Combine(Path.GetTempPath(), InstallerFilename);
            await new WebClient().DownloadFileTaskAsync(new Uri(InstallerFileUrl), tmp);
            var downloadedFileVersionString = FileVersionInfo.GetVersionInfo(tmp).FileVersion;
            string downloadedFileMD5SumBase64;
            using (var stream = File.Open(tmp, FileMode.Open, FileAccess.Read))
                downloadedFileMD5SumBase64 = Convert.ToBase64String(new MD5CryptoServiceProvider().ComputeHash(stream));
            if (downloadedFileMD5SumBase64 != InstallerMD5SumBase64) throw new ChecksumMismatchException("Downloaded installer does not have the expected MD5sum");
            if (downloadedFileVersionString != ProductVersion) throw new FileVersionMismatchException("Downloaded file does not have the expected version number, even though the MD5sum matches. Probably should never happen.");
            return tmp;
        }

        bool _allowSelfSignedCertificatesInServerCertificateChain;

        bool CertificateValidationCallback(object sender, X509Certificate certificate, X509Chain chain, SslPolicyErrors sslPolicyErrors)
        {
            // If the certificate is a valid, signed certificate, return true.
            if (sslPolicyErrors == SslPolicyErrors.None) return true;

            // If there are any errors apart from errors in the certificate chain, the certificate is considered invalid
            if ((sslPolicyErrors & SslPolicyErrors.RemoteCertificateChainErrors) == 0) return false;

            // If there are errors in the certificate chain, but the ChainStatus is empty, something is wrong
            if (chain == null) throw new ArgumentNullException("chain", "Validation reported errors in the certificate chain, yet no errors were returned");

            // If the only errors in the certificate chain are untrusted root errors for self-signed certificates, 
            // defer to the user's stated policy about the validity of such certificates
            // Any other error results in the certificate being considered invalid
            return chain.ChainStatus.Where(status => (certificate.Subject != certificate.Issuer) || (status.Status != X509ChainStatusFlags.UntrustedRoot)).All(status => status.Status == X509ChainStatusFlags.NoError) && _allowSelfSignedCertificatesInServerCertificateChain;
        }
    }

    [Serializable]
    public class ChecksumMismatchException : Exception
    {
        //
        // For guidelines regarding the creation of new exception types, see
        //    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/cpgenref/html/cpconerrorraisinghandlingguidelines.asp
        // and
        //    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dncscol/html/csharp07192001.asp
        //

        public ChecksumMismatchException() { }
        public ChecksumMismatchException(string message) : base(message) { }
        public ChecksumMismatchException(string message, Exception inner) : base(message, inner) { }

        protected ChecksumMismatchException(
            SerializationInfo info,
            StreamingContext context) : base(info, context) { }
    }

    [Serializable]
    public class FileVersionMismatchException : Exception
    {
        //
        // For guidelines regarding the creation of new exception types, see
        //    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/cpgenref/html/cpconerrorraisinghandlingguidelines.asp
        // and
        //    http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dncscol/html/csharp07192001.asp
        //

        public FileVersionMismatchException() { }
        public FileVersionMismatchException(string message) : base(message) { }
        public FileVersionMismatchException(string message, Exception inner) : base(message, inner) { }

        protected FileVersionMismatchException(
            SerializationInfo info,
            StreamingContext context) : base(info, context) { }
    }
}
