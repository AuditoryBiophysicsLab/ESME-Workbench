using System;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using System.Threading;

namespace ESMEWorkBench
{
    public static class MiniDump
    {
        private static int _dumpError;
        private static MakeDumpArgs _args;
        private static MinidumpExceptionInfo _mei;

        public static bool TryDump(String dmpPath, MiniDumpType dmpType)
        {
            _args.Path = dmpPath;
            _args.Type = dmpType;
            _mei.ThreadId = GetCurrentThreadId();
            _mei.ExceptionPointers = Marshal.GetExceptionPointers();
            _mei.ClientPointers = false;
            var t = new Thread(MakeDump);
            t.Start();
            t.Join();
            return _dumpError == 0;
        }

        private static void MakeDump()
        {
            using (var stream = new FileStream(_args.Path, FileMode.Create))
            {
                var process = Process.GetCurrentProcess();

                var mem = Marshal.AllocHGlobal(Marshal.SizeOf(_mei));
                Marshal.StructureToPtr(_mei, mem, false);

                var res = MiniDumpWriteDump(
                    process.Handle,
                    process.Id,
                    stream.SafeFileHandle.DangerousGetHandle(),
                    _args.Type,
                    _mei.ClientPointers ? mem : IntPtr.Zero,
                    IntPtr.Zero,
                    IntPtr.Zero);

                _dumpError = res ? 0 : Marshal.GetLastWin32Error();
                Marshal.FreeHGlobal(mem);
            }
        }

        [DllImport("DbgHelp.dll", SetLastError = true, CallingConvention = CallingConvention.Winapi)]
        private static extern Boolean MiniDumpWriteDump(
            IntPtr hProcess,
            Int32 processId,
            IntPtr fileHandle,
            MiniDumpType dumpType,
            IntPtr excepInfo,
            IntPtr userInfo,
            IntPtr extInfo);

        [DllImport("kernel32.dll")]
        private static extern int GetCurrentThreadId();

        #region Nested type: MakeDumpArgs

        private struct MakeDumpArgs
        {
            public string Path;
            public MiniDumpType Type;
        }

        #endregion

        #region Nested type: MinidumpExceptionInfo

        [StructLayout(LayoutKind.Sequential)]
        private struct MinidumpExceptionInfo
        {
            public Int32 ThreadId;
            public IntPtr ExceptionPointers;
            public bool ClientPointers;
        }

        #endregion
    }

    public enum MiniDumpType
    {
        Normal = 0x00000000,
        WithDataSegs = 0x00000001,
        WithFullMemory = 0x00000002,
        WithHandleData = 0x00000004,
        FilterMemory = 0x00000008,
        ScanMemory = 0x00000010,
        WithUnloadedModules = 0x00000020,
        WithIndirectlyReferencedMemory = 0x00000040,
        FilterModulePaths = 0x00000080,
        WithProcessThreadData = 0x00000100,
        WithPrivateReadWriteMemory = 0x00000200,
        WithoutOptionalData = 0x00000400,
        WithFullMemoryInfo = 0x00000800,
        WithThreadInfo = 0x00001000,
        WithCodeSegs = 0x00002000,
        WithoutAuxiliaryState = 0x00004000,
        WithFullAuxiliaryState = 0x00008000
    }
}