using System;
using System.Collections.Generic;
using System.IO.Ports;
using System.Runtime.InteropServices;
using System.Text;

using System.Diagnostics;

using Microsoft.Win32;
using Microsoft.VisualBasic;
using Microsoft.VisualBasic.CompilerServices;

namespace TEMPer.Communication
{
    public class TEMPerInterface
    {
        internal const String Version = "2008.02.23.1";

        private String m_PortName = null;
        private SerialPort m_SerialPort = null;
        private String m_CHic = null;
        private bool m_Init = false;

        #region Static Methods

        public static double CtoF(double celsius)
        {
            return (((0.9 / 0.5) * celsius) + 32);
        }

        [DllImport("kernel32.dll")]
        internal static extern Boolean GetCommProperties(IntPtr hFile, out COMMPROP cp);
        [StructLayout(LayoutKind.Sequential)]
        internal struct COMMPROP
        {
            internal UInt16 wPacketLength;
            internal UInt16 wPacketVersion;
            internal UInt32 dwServiceMask;
            internal UInt32 dwReserved1;
            internal UInt32 dwMaxTxQueue;
            internal UInt32 dwMaxRxQueue;
            internal UInt32 dwMaxBaud;
            internal UInt32 dwProvSubType;
            internal UInt32 dwProvCapabilities;
            internal UInt32 dwSettableParams;
            internal UInt32 dwSettableBaud;
            internal UInt16 wSettableData;
            internal UInt16 wSettableStopParity;
            internal UInt32 dwCurrentTxQueue;
            internal UInt32 dwCurrentRxQueue;
            internal UInt32 dwProvSpec1;
            internal UInt32 dwProvSpec2;
            internal Byte wcProvChar;
        }

        [DllImport("kernel32.dll", SetLastError = true)]
        internal static extern IntPtr CreateFile(String lpFileName,
           UInt32 dwDesiredAccess, UInt32 dwShareMode,
           IntPtr lpSecurityAttributes, UInt32 dwCreationDisposition,
           UInt32 dwFlagsAndAttributes, IntPtr hTemplateFile);

        [DllImport("kernel32.dll", SetLastError = true)]
        internal static extern bool CloseHandle(IntPtr hObject);

        public static bool CheckCOMPort(String COMPort)
        {
            try
            {
                IntPtr m_Port = CreateFile(COMPort, 0xC0000000u, 3u, IntPtr.Zero, 3u, 0x800u, IntPtr.Zero);
                if (m_Port.ToInt32() == -1) return false;

                COMMPROP m_CommProp = new COMMPROP();
                m_CommProp.dwProvSpec2 = 0;
                m_CommProp.wPacketLength = 64;
                m_CommProp.dwProvSpec1 = 0xE73CF52E;

                GetCommProperties(m_Port, out m_CommProp);

                CloseHandle(m_Port);

                return m_CommProp.dwProvSpec2 == 1128813859 || m_CommProp.dwProvSpec2 == 1128813842;
            }
            catch (Exception) { }

            return false;
        }

        public static bool CommPortExists(String COMPort)
        {
            String Key = COMPort.Trim().ToUpper();

            foreach (String Port in SerialPort.GetPortNames())
                if (COMPort == Port) return true;

            return false;
        }

        public static String[] FindDevices()
        {
            List<String> found_ports = new List<String>();

            String[] ports = SerialPort.GetPortNames();
            if (ports == null) return null;

            String PortName = null;
            for (int i = 0; i < ports.Length; i++)
            {
                PortName = @"\\.\" + ports[i] + "\0";

                if(CheckCOMPort(PortName))
                    found_ports.Add(ports[i]);
            }

            found_ports.Sort();

            return found_ports.ToArray();
        }

        private static double Bin2Dec(String strBin)
        {
            try
            {
                double lDec = 0.0;
                if (strBin == null || strBin.Length == 0)
                    strBin = "0";
                double lCount = Strings.Len(strBin);
                double t_double = lCount;
                for (double i = 1.0; i <= t_double; i++)
                {
                    lDec += Conversions.ToInteger(Strings.Left(strBin, 1)) * Math.Pow(2.0, (double)(Strings.Len(strBin) - 1));
                    strBin = Strings.Right(strBin, Strings.Len(strBin) - 1);
                }
                return lDec;
            }
            catch (Exception) { }

            return double.MinValue;
        }

        private static void Delay(int x)
        {
            for (int i = 0; i <= x; i++) ;
        }

        #endregion

        #region Class Methods

        public TEMPerInterface(String COMPort)
        {
            m_PortName = COMPort;
            Init();
        }

        private void Init()
        {
            if (m_PortName == null) return;

            m_SerialPort = new SerialPort(m_PortName);
            m_CHic = "T";

            if (CommPortExists(m_PortName))
            {
                m_SerialPort.Open();

                WriteP1P0(1, "0110000");
                WriteP1P0(0, "00000000");

                if (m_SerialPort.IsOpen)
                    m_SerialPort.Close();

                m_Init = true;
            }
        }

        public String PortName
        {
            get { return m_PortName; }
        }

        public double ReadTEMP()
        {
            try
            {
                if (m_SerialPort == null)
                    return double.MinValue;

                if (!m_Init)
                    Init();

                if (!m_Init)
                    return double.MinValue;

                if (!m_SerialPort.IsOpen)
                    m_SerialPort.Open();

                double ReadTEMP = double.MinValue;

                SDout(1);
                SDin();
                SDout(0);
                SDin();
                Start_IIC();
                SDout(1);
                HiLowSCLK();
                SDout(0);
                HiLowSCLK();
                SDout(0);
                HiLowSCLK();
                SDout(1);
                HiLowSCLK();
                SDout(1);
                HiLowSCLK();
                SDout(1);
                HiLowSCLK();
                SDout(1);
                HiLowSCLK();
                SDout(1);
                HiLowSCLK();
                Sclk(1);
                Delay(100);
                byte tt = SDin();
                HiLowSCLK();

                if (tt == 1)
                {
                    if (m_CHic == "T")
                        m_CHic = "R";
                    else
                        m_CHic = "T";
                }
                if (tt == 1)
                    return double.MinValue;

                StringBuilder data = new StringBuilder(16);

                data.Append(SDin());
                HiLowSCLK();
                data.Append(SDin());
                HiLowSCLK();
                data.Append(SDin());
                HiLowSCLK();
                data.Append(SDin());
                HiLowSCLK();
                data.Append(SDin());
                HiLowSCLK();
                data.Append(SDin());
                HiLowSCLK();
                data.Append(SDin());
                HiLowSCLK();
                data.Append(SDin());
                HiLowSCLK();
                tt = SDin();
                Delay(100);
                HiLowSCLK();
                data.Append(SDin());
                HiLowSCLK();
                data.Append(SDin());
                HiLowSCLK();
                data.Append(SDin());
                HiLowSCLK();
                data.Append(SDin());
                HiLowSCLK();
                data.Append(SDin());
                HiLowSCLK();
                data.Append(SDin());
                HiLowSCLK();
                data.Append(SDin());
                HiLowSCLK();
                data.Append(SDin());
                Sclk(0);
                Delay(1);
                String str_data = data.ToString();
                String FuHao = Strings.Left(str_data, 1);
                String str_temp = Strings.Left(str_data, 11);
                String str_msb = Strings.Left(str_temp, 3);
                String str_lsb = Strings.Right(str_temp, 8);
                double msb = Bin2Dec(str_msb);
                double lsb = Bin2Dec(str_lsb);
                double tempdata = (msb * 256.0) + lsb;
                switch (FuHao)
                {
                    case "0":
                        ReadTEMP = tempdata * 0.125;
                        break;

                    case "1":
                        ReadTEMP = -(2048.0 - tempdata) * 0.125;
                        break;
                }

                SDout(0);
                HiLowSCLK();
                Stop_IIC();

                if (m_SerialPort.IsOpen)
                    m_SerialPort.Close();

                return ReadTEMP;
            }
            catch (Exception) { }

            m_Init = false;
            return double.MinValue;
        }

        private void Start_IIC()
        {
            SDout(1);
            Delay(4);
            Sclk(1);
            Delay(40);
            SDout(0);
            Delay(30);
            Sclk(0);
        }

        private void Stop_IIC()
        {
            SDout(0);
            Delay(50);
            Sclk(1);
            Delay(50);
            SDout(1);
            Delay(50);
        }

        private void Sclk(byte ad01)
        {
            if (m_CHic == "T")
                m_SerialPort.DtrEnable = ad01 == 0;
            else if (m_CHic == "R")
                m_SerialPort.DtrEnable = ad01 == 1;
        }

        private void HiLowSCLK()
        {
            Delay(10);
            Sclk(1);
            Delay(20);
            Sclk(0);
            Delay(20);
        }

        private byte SDin()
        {
            byte SDin = 0;

            SDout(1);
            Delay(50);
            Delay(50);
            bool a = m_SerialPort.CtsHolding;
            if (m_CHic == "T")
            {
                if (!a)
                    SDin = 1;
                else
                    SDin = 0;
            }
            if (m_CHic != "R")
                return SDin;
            if (!a)
                return 0;
            return 1;
        }

        private void SDout(byte ad01)
        {
            if (m_CHic == "T")
            {
                if (ad01 == 0)
                    m_SerialPort.RtsEnable = true;
                if (ad01 == 1)
                    m_SerialPort.RtsEnable = false;
            }
            if (m_CHic == "R")
            {
                if (ad01 == 0)
                    m_SerialPort.RtsEnable = false;
                if (ad01 == 1)
                    m_SerialPort.RtsEnable = true;
            }
        }

        private void WriteP1P0(byte P0123, String dataS)
        {
            Stop_IIC();
            Delay(100);
            Start_IIC();
            SDout(1);
            HiLowSCLK();
            SDout(0);
            HiLowSCLK();
            SDout(0);
            HiLowSCLK();
            SDout(1);
            HiLowSCLK();
            SDout(1);
            HiLowSCLK();
            SDout(1);
            HiLowSCLK();
            SDout(1);
            HiLowSCLK();
            SDout(0);
            HiLowSCLK();
            Delay(100);
            Sclk(1);
            Delay(100);
            SDin();
            HiLowSCLK();

            SDout(0);
            HiLowSCLK();
            SDout(0);
            HiLowSCLK();
            SDout(0);
            HiLowSCLK();
            SDout(0);
            HiLowSCLK();
            SDout(0);
            HiLowSCLK();
            SDout(0);
            HiLowSCLK();
            if (P0123 == 0)
            {
                SDout(0);
                HiLowSCLK();
                SDout(0);
                HiLowSCLK();
            }
            else if (P0123 == 1)
            {
                SDout(0);
                HiLowSCLK();
                SDout(1);
                HiLowSCLK();
            }
            else if (P0123 == 2)
            {
                SDout(1);
                HiLowSCLK();
                SDout(0);
                HiLowSCLK();
            }
            else if (P0123 == 3)
            {
                SDout(1);
                HiLowSCLK();
                SDout(1);
                HiLowSCLK();
            }
            Delay(100);
            Sclk(1);
            Delay(100);
            SDin();
            HiLowSCLK();

            byte xx = 1;
            do
            {
                if (Strings.Mid(dataS, xx, 1) == "1")
                    SDout(1);
                else
                    SDout(0);
                HiLowSCLK();
                Delay(100);
                xx = (byte)(xx + 1);
            }
            while (xx <= 8);
            Sclk(1);
            Delay(100);
            Delay(100);
            SDin();
            HiLowSCLK();
            SDout(0);
            HiLowSCLK();
            Stop_IIC();
        }

        #endregion
    }
}
