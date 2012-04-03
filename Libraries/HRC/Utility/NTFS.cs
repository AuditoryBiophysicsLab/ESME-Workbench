using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;

namespace System.IO
{
    static class NTFS
    {
        [DllImport("kernel32.dll")]
        static extern bool CreateSymbolicLink(string lpSymlinkFileName, string lpTargetFileName, int dwFlags);

        static int SYMLINK_FLAG_DIRECTORY = 1;

        [DllImport("kernel32.dll")]
        static extern bool CreateHardLink(string lpSymlinkFileName, string lpTargetFileName, int lpSecurityAttributes = 0);
    }
}
