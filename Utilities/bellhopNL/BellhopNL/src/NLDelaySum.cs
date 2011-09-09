/*
* MATLAB Compiler: 4.15 (R2011a)
* Date: Fri Sep 09 14:14:20 2011
* Arguments: "-B" "macro_default" "-W" "dotnet:BellhopNL,NLDelaySum,4.0,private" "-T"
* "link:lib" "-d" "C:\Projects\ESME Deliverables\Utilities\bellhopNL\BellhopNL\src" "-w"
* "enable:specified_file_mismatch" "-w" "enable:repeated_file" "-w"
* "enable:switch_ignored" "-w" "enable:missing_lib_sentinel" "-w" "enable:demo_license"
* "-v" "class{NLDelaySum:C:\Projects\ESME
* Deliverables\Utilities\bellhopNL\NL_delaysum.m,C:\Projects\ESME
* Deliverables\Utilities\bellhopNL\read_arrivals_bin.m,C:\Projects\ESME
* Deliverables\Utilities\bellhopNL\ts_arons.m,C:\Projects\ESME
* Deliverables\Utilities\bellhopNL\ts_chapman.m}" 
*/
using System;
using System.Reflection;
using System.IO;
using MathWorks.MATLAB.NET.Arrays;
using MathWorks.MATLAB.NET.Utility;

#if SHARED
[assembly: System.Reflection.AssemblyKeyFile(@"")]
#endif

namespace BellhopNL
{

  /// <summary>
  /// The NLDelaySum class provides a CLS compliant, MWArray interface to the M-functions
  /// contained in the files:
  /// <newpara></newpara>
  /// C:\Projects\ESME Deliverables\Utilities\bellhopNL\NL_delaysum.m
  /// <newpara></newpara>
  /// C:\Projects\ESME Deliverables\Utilities\bellhopNL\read_arrivals_bin.m
  /// <newpara></newpara>
  /// C:\Projects\ESME Deliverables\Utilities\bellhopNL\ts_arons.m
  /// <newpara></newpara>
  /// C:\Projects\ESME Deliverables\Utilities\bellhopNL\ts_chapman.m
  /// <newpara></newpara>
  /// deployprint.m
  /// <newpara></newpara>
  /// printdlg.m
  /// </summary>
  /// <remarks>
  /// @Version 4.0
  /// </remarks>
  public class NLDelaySum : IDisposable
  {
    #region Constructors

    /// <summary internal= "true">
    /// The static constructor instantiates and initializes the MATLAB Compiler Runtime
    /// instance.
    /// </summary>
    static NLDelaySum()
    {
      if (MWMCR.MCRAppInitialized)
      {
        Assembly assembly= Assembly.GetExecutingAssembly();

        string ctfFilePath= assembly.Location;

        int lastDelimiter= ctfFilePath.LastIndexOf(@"\");

        ctfFilePath= ctfFilePath.Remove(lastDelimiter, (ctfFilePath.Length - lastDelimiter));

        string ctfFileName = "BellhopNL.ctf";

        Stream embeddedCtfStream = null;

        String[] resourceStrings = assembly.GetManifestResourceNames();

        foreach (String name in resourceStrings)
        {
          if (name.Contains(ctfFileName))
          {
            embeddedCtfStream = assembly.GetManifestResourceStream(name);
            break;
          }
        }
        mcr= new MWMCR("",
                       ctfFilePath, embeddedCtfStream, true);
      }
      else
      {
        throw new ApplicationException("MWArray assembly could not be initialized");
      }
    }


    /// <summary>
    /// Constructs a new instance of the NLDelaySum class.
    /// </summary>
    public NLDelaySum()
    {
    }


    #endregion Constructors

    #region Finalize

    /// <summary internal= "true">
    /// Class destructor called by the CLR garbage collector.
    /// </summary>
    ~NLDelaySum()
    {
      Dispose(false);
    }


    /// <summary>
    /// Frees the native resources associated with this object
    /// </summary>
    public void Dispose()
    {
      Dispose(true);

      GC.SuppressFinalize(this);
    }


    /// <summary internal= "true">
    /// Internal dispose function
    /// </summary>
    protected virtual void Dispose(bool disposing)
    {
      if (!disposed)
      {
        disposed= true;

        if (disposing)
        {
          // Free managed resources;
        }

        // Free native resources
      }
    }


    #endregion Finalize

    #region Methods

    /// <summary>
    /// Provides a single output, 0-input MWArrayinterface to the NL_delaysum M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// NL_DELAYSUM - Incorporates nonlinear effects using similitude equations
    /// Usage: rtsmat = NL_delaysum(ARRFIL, z, w, fs, T, model)
    /// Inputs:
    /// ARRFIL - string containing name of (binary) BELLHOP arrivals file
    /// z - charge depth (meters)
    /// w - charge weight (kilograms)
    /// fs - sample rate for output (Hz)
    /// T - total duration of output waveform (seconds)
    /// model - similitude time series model (either: 'arons' or 'chapman')
    /// Output:
    /// rtsmat - matrix of received time series samples (one column for each rcv)
    /// rr - receiver ranges
    /// rd - receiver depths
    /// Based on delaysum from the Acoustics Toolbox, which computes the receiver
    /// time series by summing up the echoes in the waveguide based on the
    /// amplitude and delay of each echo.
    /// Modified by J. Peterson to account for the nonlinear effects via
    /// similitude formulas. He also added the Arons formula for the explosive
    /// waveform
    /// Further work by L. Henderson fixing an issue due to the fact that the
    /// Hilbert transform is non-causal, i.e. starts before the original waveform
    /// $Id: $
    /// </remarks>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray NL_delaysum()
    {
      return mcr.EvaluateFunction("NL_delaysum", new MWArray[]{});
    }


    /// <summary>
    /// Provides a single output, 1-input MWArrayinterface to the NL_delaysum M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// NL_DELAYSUM - Incorporates nonlinear effects using similitude equations
    /// Usage: rtsmat = NL_delaysum(ARRFIL, z, w, fs, T, model)
    /// Inputs:
    /// ARRFIL - string containing name of (binary) BELLHOP arrivals file
    /// z - charge depth (meters)
    /// w - charge weight (kilograms)
    /// fs - sample rate for output (Hz)
    /// T - total duration of output waveform (seconds)
    /// model - similitude time series model (either: 'arons' or 'chapman')
    /// Output:
    /// rtsmat - matrix of received time series samples (one column for each rcv)
    /// rr - receiver ranges
    /// rd - receiver depths
    /// Based on delaysum from the Acoustics Toolbox, which computes the receiver
    /// time series by summing up the echoes in the waveguide based on the
    /// amplitude and delay of each echo.
    /// Modified by J. Peterson to account for the nonlinear effects via
    /// similitude formulas. He also added the Arons formula for the explosive
    /// waveform
    /// Further work by L. Henderson fixing an issue due to the fact that the
    /// Hilbert transform is non-causal, i.e. starts before the original waveform
    /// $Id: $
    /// </remarks>
    /// <param name="ARRFIL">Input argument #1</param>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray NL_delaysum(MWArray ARRFIL)
    {
      return mcr.EvaluateFunction("NL_delaysum", ARRFIL);
    }


    /// <summary>
    /// Provides a single output, 2-input MWArrayinterface to the NL_delaysum M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// NL_DELAYSUM - Incorporates nonlinear effects using similitude equations
    /// Usage: rtsmat = NL_delaysum(ARRFIL, z, w, fs, T, model)
    /// Inputs:
    /// ARRFIL - string containing name of (binary) BELLHOP arrivals file
    /// z - charge depth (meters)
    /// w - charge weight (kilograms)
    /// fs - sample rate for output (Hz)
    /// T - total duration of output waveform (seconds)
    /// model - similitude time series model (either: 'arons' or 'chapman')
    /// Output:
    /// rtsmat - matrix of received time series samples (one column for each rcv)
    /// rr - receiver ranges
    /// rd - receiver depths
    /// Based on delaysum from the Acoustics Toolbox, which computes the receiver
    /// time series by summing up the echoes in the waveguide based on the
    /// amplitude and delay of each echo.
    /// Modified by J. Peterson to account for the nonlinear effects via
    /// similitude formulas. He also added the Arons formula for the explosive
    /// waveform
    /// Further work by L. Henderson fixing an issue due to the fact that the
    /// Hilbert transform is non-causal, i.e. starts before the original waveform
    /// $Id: $
    /// </remarks>
    /// <param name="ARRFIL">Input argument #1</param>
    /// <param name="z">Input argument #2</param>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray NL_delaysum(MWArray ARRFIL, MWArray z)
    {
      return mcr.EvaluateFunction("NL_delaysum", ARRFIL, z);
    }


    /// <summary>
    /// Provides a single output, 3-input MWArrayinterface to the NL_delaysum M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// NL_DELAYSUM - Incorporates nonlinear effects using similitude equations
    /// Usage: rtsmat = NL_delaysum(ARRFIL, z, w, fs, T, model)
    /// Inputs:
    /// ARRFIL - string containing name of (binary) BELLHOP arrivals file
    /// z - charge depth (meters)
    /// w - charge weight (kilograms)
    /// fs - sample rate for output (Hz)
    /// T - total duration of output waveform (seconds)
    /// model - similitude time series model (either: 'arons' or 'chapman')
    /// Output:
    /// rtsmat - matrix of received time series samples (one column for each rcv)
    /// rr - receiver ranges
    /// rd - receiver depths
    /// Based on delaysum from the Acoustics Toolbox, which computes the receiver
    /// time series by summing up the echoes in the waveguide based on the
    /// amplitude and delay of each echo.
    /// Modified by J. Peterson to account for the nonlinear effects via
    /// similitude formulas. He also added the Arons formula for the explosive
    /// waveform
    /// Further work by L. Henderson fixing an issue due to the fact that the
    /// Hilbert transform is non-causal, i.e. starts before the original waveform
    /// $Id: $
    /// </remarks>
    /// <param name="ARRFIL">Input argument #1</param>
    /// <param name="z">Input argument #2</param>
    /// <param name="w">Input argument #3</param>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray NL_delaysum(MWArray ARRFIL, MWArray z, MWArray w)
    {
      return mcr.EvaluateFunction("NL_delaysum", ARRFIL, z, w);
    }


    /// <summary>
    /// Provides a single output, 4-input MWArrayinterface to the NL_delaysum M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// NL_DELAYSUM - Incorporates nonlinear effects using similitude equations
    /// Usage: rtsmat = NL_delaysum(ARRFIL, z, w, fs, T, model)
    /// Inputs:
    /// ARRFIL - string containing name of (binary) BELLHOP arrivals file
    /// z - charge depth (meters)
    /// w - charge weight (kilograms)
    /// fs - sample rate for output (Hz)
    /// T - total duration of output waveform (seconds)
    /// model - similitude time series model (either: 'arons' or 'chapman')
    /// Output:
    /// rtsmat - matrix of received time series samples (one column for each rcv)
    /// rr - receiver ranges
    /// rd - receiver depths
    /// Based on delaysum from the Acoustics Toolbox, which computes the receiver
    /// time series by summing up the echoes in the waveguide based on the
    /// amplitude and delay of each echo.
    /// Modified by J. Peterson to account for the nonlinear effects via
    /// similitude formulas. He also added the Arons formula for the explosive
    /// waveform
    /// Further work by L. Henderson fixing an issue due to the fact that the
    /// Hilbert transform is non-causal, i.e. starts before the original waveform
    /// $Id: $
    /// </remarks>
    /// <param name="ARRFIL">Input argument #1</param>
    /// <param name="z">Input argument #2</param>
    /// <param name="w">Input argument #3</param>
    /// <param name="fs">Input argument #4</param>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray NL_delaysum(MWArray ARRFIL, MWArray z, MWArray w, MWArray fs)
    {
      return mcr.EvaluateFunction("NL_delaysum", ARRFIL, z, w, fs);
    }


    /// <summary>
    /// Provides a single output, 5-input MWArrayinterface to the NL_delaysum M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// NL_DELAYSUM - Incorporates nonlinear effects using similitude equations
    /// Usage: rtsmat = NL_delaysum(ARRFIL, z, w, fs, T, model)
    /// Inputs:
    /// ARRFIL - string containing name of (binary) BELLHOP arrivals file
    /// z - charge depth (meters)
    /// w - charge weight (kilograms)
    /// fs - sample rate for output (Hz)
    /// T - total duration of output waveform (seconds)
    /// model - similitude time series model (either: 'arons' or 'chapman')
    /// Output:
    /// rtsmat - matrix of received time series samples (one column for each rcv)
    /// rr - receiver ranges
    /// rd - receiver depths
    /// Based on delaysum from the Acoustics Toolbox, which computes the receiver
    /// time series by summing up the echoes in the waveguide based on the
    /// amplitude and delay of each echo.
    /// Modified by J. Peterson to account for the nonlinear effects via
    /// similitude formulas. He also added the Arons formula for the explosive
    /// waveform
    /// Further work by L. Henderson fixing an issue due to the fact that the
    /// Hilbert transform is non-causal, i.e. starts before the original waveform
    /// $Id: $
    /// </remarks>
    /// <param name="ARRFIL">Input argument #1</param>
    /// <param name="z">Input argument #2</param>
    /// <param name="w">Input argument #3</param>
    /// <param name="fs">Input argument #4</param>
    /// <param name="T">Input argument #5</param>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray NL_delaysum(MWArray ARRFIL, MWArray z, MWArray w, MWArray fs, MWArray 
                         T)
    {
      return mcr.EvaluateFunction("NL_delaysum", ARRFIL, z, w, fs, T);
    }


    /// <summary>
    /// Provides a single output, 6-input MWArrayinterface to the NL_delaysum M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// NL_DELAYSUM - Incorporates nonlinear effects using similitude equations
    /// Usage: rtsmat = NL_delaysum(ARRFIL, z, w, fs, T, model)
    /// Inputs:
    /// ARRFIL - string containing name of (binary) BELLHOP arrivals file
    /// z - charge depth (meters)
    /// w - charge weight (kilograms)
    /// fs - sample rate for output (Hz)
    /// T - total duration of output waveform (seconds)
    /// model - similitude time series model (either: 'arons' or 'chapman')
    /// Output:
    /// rtsmat - matrix of received time series samples (one column for each rcv)
    /// rr - receiver ranges
    /// rd - receiver depths
    /// Based on delaysum from the Acoustics Toolbox, which computes the receiver
    /// time series by summing up the echoes in the waveguide based on the
    /// amplitude and delay of each echo.
    /// Modified by J. Peterson to account for the nonlinear effects via
    /// similitude formulas. He also added the Arons formula for the explosive
    /// waveform
    /// Further work by L. Henderson fixing an issue due to the fact that the
    /// Hilbert transform is non-causal, i.e. starts before the original waveform
    /// $Id: $
    /// </remarks>
    /// <param name="ARRFIL">Input argument #1</param>
    /// <param name="z">Input argument #2</param>
    /// <param name="w">Input argument #3</param>
    /// <param name="fs">Input argument #4</param>
    /// <param name="T">Input argument #5</param>
    /// <param name="model">Input argument #6</param>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray NL_delaysum(MWArray ARRFIL, MWArray z, MWArray w, MWArray fs, MWArray 
                         T, MWArray model)
    {
      return mcr.EvaluateFunction("NL_delaysum", ARRFIL, z, w, fs, T, model);
    }


    /// <summary>
    /// Provides the standard 0-input MWArray interface to the NL_delaysum M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// NL_DELAYSUM - Incorporates nonlinear effects using similitude equations
    /// Usage: rtsmat = NL_delaysum(ARRFIL, z, w, fs, T, model)
    /// Inputs:
    /// ARRFIL - string containing name of (binary) BELLHOP arrivals file
    /// z - charge depth (meters)
    /// w - charge weight (kilograms)
    /// fs - sample rate for output (Hz)
    /// T - total duration of output waveform (seconds)
    /// model - similitude time series model (either: 'arons' or 'chapman')
    /// Output:
    /// rtsmat - matrix of received time series samples (one column for each rcv)
    /// rr - receiver ranges
    /// rd - receiver depths
    /// Based on delaysum from the Acoustics Toolbox, which computes the receiver
    /// time series by summing up the echoes in the waveguide based on the
    /// amplitude and delay of each echo.
    /// Modified by J. Peterson to account for the nonlinear effects via
    /// similitude formulas. He also added the Arons formula for the explosive
    /// waveform
    /// Further work by L. Henderson fixing an issue due to the fact that the
    /// Hilbert transform is non-causal, i.e. starts before the original waveform
    /// $Id: $
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] NL_delaysum(int numArgsOut)
    {
      return mcr.EvaluateFunction(numArgsOut, "NL_delaysum", new MWArray[]{});
    }


    /// <summary>
    /// Provides the standard 1-input MWArray interface to the NL_delaysum M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// NL_DELAYSUM - Incorporates nonlinear effects using similitude equations
    /// Usage: rtsmat = NL_delaysum(ARRFIL, z, w, fs, T, model)
    /// Inputs:
    /// ARRFIL - string containing name of (binary) BELLHOP arrivals file
    /// z - charge depth (meters)
    /// w - charge weight (kilograms)
    /// fs - sample rate for output (Hz)
    /// T - total duration of output waveform (seconds)
    /// model - similitude time series model (either: 'arons' or 'chapman')
    /// Output:
    /// rtsmat - matrix of received time series samples (one column for each rcv)
    /// rr - receiver ranges
    /// rd - receiver depths
    /// Based on delaysum from the Acoustics Toolbox, which computes the receiver
    /// time series by summing up the echoes in the waveguide based on the
    /// amplitude and delay of each echo.
    /// Modified by J. Peterson to account for the nonlinear effects via
    /// similitude formulas. He also added the Arons formula for the explosive
    /// waveform
    /// Further work by L. Henderson fixing an issue due to the fact that the
    /// Hilbert transform is non-causal, i.e. starts before the original waveform
    /// $Id: $
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <param name="ARRFIL">Input argument #1</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] NL_delaysum(int numArgsOut, MWArray ARRFIL)
    {
      return mcr.EvaluateFunction(numArgsOut, "NL_delaysum", ARRFIL);
    }


    /// <summary>
    /// Provides the standard 2-input MWArray interface to the NL_delaysum M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// NL_DELAYSUM - Incorporates nonlinear effects using similitude equations
    /// Usage: rtsmat = NL_delaysum(ARRFIL, z, w, fs, T, model)
    /// Inputs:
    /// ARRFIL - string containing name of (binary) BELLHOP arrivals file
    /// z - charge depth (meters)
    /// w - charge weight (kilograms)
    /// fs - sample rate for output (Hz)
    /// T - total duration of output waveform (seconds)
    /// model - similitude time series model (either: 'arons' or 'chapman')
    /// Output:
    /// rtsmat - matrix of received time series samples (one column for each rcv)
    /// rr - receiver ranges
    /// rd - receiver depths
    /// Based on delaysum from the Acoustics Toolbox, which computes the receiver
    /// time series by summing up the echoes in the waveguide based on the
    /// amplitude and delay of each echo.
    /// Modified by J. Peterson to account for the nonlinear effects via
    /// similitude formulas. He also added the Arons formula for the explosive
    /// waveform
    /// Further work by L. Henderson fixing an issue due to the fact that the
    /// Hilbert transform is non-causal, i.e. starts before the original waveform
    /// $Id: $
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <param name="ARRFIL">Input argument #1</param>
    /// <param name="z">Input argument #2</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] NL_delaysum(int numArgsOut, MWArray ARRFIL, MWArray z)
    {
      return mcr.EvaluateFunction(numArgsOut, "NL_delaysum", ARRFIL, z);
    }


    /// <summary>
    /// Provides the standard 3-input MWArray interface to the NL_delaysum M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// NL_DELAYSUM - Incorporates nonlinear effects using similitude equations
    /// Usage: rtsmat = NL_delaysum(ARRFIL, z, w, fs, T, model)
    /// Inputs:
    /// ARRFIL - string containing name of (binary) BELLHOP arrivals file
    /// z - charge depth (meters)
    /// w - charge weight (kilograms)
    /// fs - sample rate for output (Hz)
    /// T - total duration of output waveform (seconds)
    /// model - similitude time series model (either: 'arons' or 'chapman')
    /// Output:
    /// rtsmat - matrix of received time series samples (one column for each rcv)
    /// rr - receiver ranges
    /// rd - receiver depths
    /// Based on delaysum from the Acoustics Toolbox, which computes the receiver
    /// time series by summing up the echoes in the waveguide based on the
    /// amplitude and delay of each echo.
    /// Modified by J. Peterson to account for the nonlinear effects via
    /// similitude formulas. He also added the Arons formula for the explosive
    /// waveform
    /// Further work by L. Henderson fixing an issue due to the fact that the
    /// Hilbert transform is non-causal, i.e. starts before the original waveform
    /// $Id: $
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <param name="ARRFIL">Input argument #1</param>
    /// <param name="z">Input argument #2</param>
    /// <param name="w">Input argument #3</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] NL_delaysum(int numArgsOut, MWArray ARRFIL, MWArray z, MWArray w)
    {
      return mcr.EvaluateFunction(numArgsOut, "NL_delaysum", ARRFIL, z, w);
    }


    /// <summary>
    /// Provides the standard 4-input MWArray interface to the NL_delaysum M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// NL_DELAYSUM - Incorporates nonlinear effects using similitude equations
    /// Usage: rtsmat = NL_delaysum(ARRFIL, z, w, fs, T, model)
    /// Inputs:
    /// ARRFIL - string containing name of (binary) BELLHOP arrivals file
    /// z - charge depth (meters)
    /// w - charge weight (kilograms)
    /// fs - sample rate for output (Hz)
    /// T - total duration of output waveform (seconds)
    /// model - similitude time series model (either: 'arons' or 'chapman')
    /// Output:
    /// rtsmat - matrix of received time series samples (one column for each rcv)
    /// rr - receiver ranges
    /// rd - receiver depths
    /// Based on delaysum from the Acoustics Toolbox, which computes the receiver
    /// time series by summing up the echoes in the waveguide based on the
    /// amplitude and delay of each echo.
    /// Modified by J. Peterson to account for the nonlinear effects via
    /// similitude formulas. He also added the Arons formula for the explosive
    /// waveform
    /// Further work by L. Henderson fixing an issue due to the fact that the
    /// Hilbert transform is non-causal, i.e. starts before the original waveform
    /// $Id: $
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <param name="ARRFIL">Input argument #1</param>
    /// <param name="z">Input argument #2</param>
    /// <param name="w">Input argument #3</param>
    /// <param name="fs">Input argument #4</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] NL_delaysum(int numArgsOut, MWArray ARRFIL, MWArray z, MWArray w, 
                           MWArray fs)
    {
      return mcr.EvaluateFunction(numArgsOut, "NL_delaysum", ARRFIL, z, w, fs);
    }


    /// <summary>
    /// Provides the standard 5-input MWArray interface to the NL_delaysum M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// NL_DELAYSUM - Incorporates nonlinear effects using similitude equations
    /// Usage: rtsmat = NL_delaysum(ARRFIL, z, w, fs, T, model)
    /// Inputs:
    /// ARRFIL - string containing name of (binary) BELLHOP arrivals file
    /// z - charge depth (meters)
    /// w - charge weight (kilograms)
    /// fs - sample rate for output (Hz)
    /// T - total duration of output waveform (seconds)
    /// model - similitude time series model (either: 'arons' or 'chapman')
    /// Output:
    /// rtsmat - matrix of received time series samples (one column for each rcv)
    /// rr - receiver ranges
    /// rd - receiver depths
    /// Based on delaysum from the Acoustics Toolbox, which computes the receiver
    /// time series by summing up the echoes in the waveguide based on the
    /// amplitude and delay of each echo.
    /// Modified by J. Peterson to account for the nonlinear effects via
    /// similitude formulas. He also added the Arons formula for the explosive
    /// waveform
    /// Further work by L. Henderson fixing an issue due to the fact that the
    /// Hilbert transform is non-causal, i.e. starts before the original waveform
    /// $Id: $
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <param name="ARRFIL">Input argument #1</param>
    /// <param name="z">Input argument #2</param>
    /// <param name="w">Input argument #3</param>
    /// <param name="fs">Input argument #4</param>
    /// <param name="T">Input argument #5</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] NL_delaysum(int numArgsOut, MWArray ARRFIL, MWArray z, MWArray w, 
                           MWArray fs, MWArray T)
    {
      return mcr.EvaluateFunction(numArgsOut, "NL_delaysum", ARRFIL, z, w, fs, T);
    }


    /// <summary>
    /// Provides the standard 6-input MWArray interface to the NL_delaysum M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// NL_DELAYSUM - Incorporates nonlinear effects using similitude equations
    /// Usage: rtsmat = NL_delaysum(ARRFIL, z, w, fs, T, model)
    /// Inputs:
    /// ARRFIL - string containing name of (binary) BELLHOP arrivals file
    /// z - charge depth (meters)
    /// w - charge weight (kilograms)
    /// fs - sample rate for output (Hz)
    /// T - total duration of output waveform (seconds)
    /// model - similitude time series model (either: 'arons' or 'chapman')
    /// Output:
    /// rtsmat - matrix of received time series samples (one column for each rcv)
    /// rr - receiver ranges
    /// rd - receiver depths
    /// Based on delaysum from the Acoustics Toolbox, which computes the receiver
    /// time series by summing up the echoes in the waveguide based on the
    /// amplitude and delay of each echo.
    /// Modified by J. Peterson to account for the nonlinear effects via
    /// similitude formulas. He also added the Arons formula for the explosive
    /// waveform
    /// Further work by L. Henderson fixing an issue due to the fact that the
    /// Hilbert transform is non-causal, i.e. starts before the original waveform
    /// $Id: $
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <param name="ARRFIL">Input argument #1</param>
    /// <param name="z">Input argument #2</param>
    /// <param name="w">Input argument #3</param>
    /// <param name="fs">Input argument #4</param>
    /// <param name="T">Input argument #5</param>
    /// <param name="model">Input argument #6</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] NL_delaysum(int numArgsOut, MWArray ARRFIL, MWArray z, MWArray w, 
                           MWArray fs, MWArray T, MWArray model)
    {
      return mcr.EvaluateFunction(numArgsOut, "NL_delaysum", ARRFIL, z, w, fs, T, model);
    }


    /// <summary>
    /// Provides an interface for the NL_delaysum function in which the input and output
    /// arguments are specified as an array of MWArrays.
    /// </summary>
    /// <remarks>
    /// This method will allocate and return by reference the output argument
    /// array.<newpara></newpara>
    /// M-Documentation:
    /// NL_DELAYSUM - Incorporates nonlinear effects using similitude equations
    /// Usage: rtsmat = NL_delaysum(ARRFIL, z, w, fs, T, model)
    /// Inputs:
    /// ARRFIL - string containing name of (binary) BELLHOP arrivals file
    /// z - charge depth (meters)
    /// w - charge weight (kilograms)
    /// fs - sample rate for output (Hz)
    /// T - total duration of output waveform (seconds)
    /// model - similitude time series model (either: 'arons' or 'chapman')
    /// Output:
    /// rtsmat - matrix of received time series samples (one column for each rcv)
    /// rr - receiver ranges
    /// rd - receiver depths
    /// Based on delaysum from the Acoustics Toolbox, which computes the receiver
    /// time series by summing up the echoes in the waveguide based on the
    /// amplitude and delay of each echo.
    /// Modified by J. Peterson to account for the nonlinear effects via
    /// similitude formulas. He also added the Arons formula for the explosive
    /// waveform
    /// Further work by L. Henderson fixing an issue due to the fact that the
    /// Hilbert transform is non-causal, i.e. starts before the original waveform
    /// $Id: $
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return</param>
    /// <param name= "argsOut">Array of MWArray output arguments</param>
    /// <param name= "argsIn">Array of MWArray input arguments</param>
    ///
    public void NL_delaysum(int numArgsOut, ref MWArray[] argsOut, MWArray[] argsIn)
    {
      mcr.EvaluateFunction("NL_delaysum", numArgsOut, ref argsOut, argsIn);
    }


    /// <summary>
    /// Provides a single output, 0-input MWArrayinterface to the read_arrivals_bin
    /// M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// Read the arrival time/amplitude data computed by BELLHOP
    /// usage:
    /// [ Arr, Pos ] = read_arrivals_bin( ARRFile, Narrmx );
    /// Arr is a structure containing all the arrivals information
    /// Pos is a structure containing the positions of source and receivers
    /// ARRFile is the name of the Arrivals File
    /// Narrmx is the maximum number of arrivals allowed
    /// mbp 9/96
    /// </remarks>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray read_arrivals_bin()
    {
      return mcr.EvaluateFunction("read_arrivals_bin", new MWArray[]{});
    }


    /// <summary>
    /// Provides a single output, 1-input MWArrayinterface to the read_arrivals_bin
    /// M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// Read the arrival time/amplitude data computed by BELLHOP
    /// usage:
    /// [ Arr, Pos ] = read_arrivals_bin( ARRFile, Narrmx );
    /// Arr is a structure containing all the arrivals information
    /// Pos is a structure containing the positions of source and receivers
    /// ARRFile is the name of the Arrivals File
    /// Narrmx is the maximum number of arrivals allowed
    /// mbp 9/96
    /// </remarks>
    /// <param name="ARRFile">Input argument #1</param>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray read_arrivals_bin(MWArray ARRFile)
    {
      return mcr.EvaluateFunction("read_arrivals_bin", ARRFile);
    }


    /// <summary>
    /// Provides a single output, 2-input MWArrayinterface to the read_arrivals_bin
    /// M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// Read the arrival time/amplitude data computed by BELLHOP
    /// usage:
    /// [ Arr, Pos ] = read_arrivals_bin( ARRFile, Narrmx );
    /// Arr is a structure containing all the arrivals information
    /// Pos is a structure containing the positions of source and receivers
    /// ARRFile is the name of the Arrivals File
    /// Narrmx is the maximum number of arrivals allowed
    /// mbp 9/96
    /// </remarks>
    /// <param name="ARRFile">Input argument #1</param>
    /// <param name="Narrmx">Input argument #2</param>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray read_arrivals_bin(MWArray ARRFile, MWArray Narrmx)
    {
      return mcr.EvaluateFunction("read_arrivals_bin", ARRFile, Narrmx);
    }


    /// <summary>
    /// Provides the standard 0-input MWArray interface to the read_arrivals_bin
    /// M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// Read the arrival time/amplitude data computed by BELLHOP
    /// usage:
    /// [ Arr, Pos ] = read_arrivals_bin( ARRFile, Narrmx );
    /// Arr is a structure containing all the arrivals information
    /// Pos is a structure containing the positions of source and receivers
    /// ARRFile is the name of the Arrivals File
    /// Narrmx is the maximum number of arrivals allowed
    /// mbp 9/96
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] read_arrivals_bin(int numArgsOut)
    {
      return mcr.EvaluateFunction(numArgsOut, "read_arrivals_bin", new MWArray[]{});
    }


    /// <summary>
    /// Provides the standard 1-input MWArray interface to the read_arrivals_bin
    /// M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// Read the arrival time/amplitude data computed by BELLHOP
    /// usage:
    /// [ Arr, Pos ] = read_arrivals_bin( ARRFile, Narrmx );
    /// Arr is a structure containing all the arrivals information
    /// Pos is a structure containing the positions of source and receivers
    /// ARRFile is the name of the Arrivals File
    /// Narrmx is the maximum number of arrivals allowed
    /// mbp 9/96
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <param name="ARRFile">Input argument #1</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] read_arrivals_bin(int numArgsOut, MWArray ARRFile)
    {
      return mcr.EvaluateFunction(numArgsOut, "read_arrivals_bin", ARRFile);
    }


    /// <summary>
    /// Provides the standard 2-input MWArray interface to the read_arrivals_bin
    /// M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// Read the arrival time/amplitude data computed by BELLHOP
    /// usage:
    /// [ Arr, Pos ] = read_arrivals_bin( ARRFile, Narrmx );
    /// Arr is a structure containing all the arrivals information
    /// Pos is a structure containing the positions of source and receivers
    /// ARRFile is the name of the Arrivals File
    /// Narrmx is the maximum number of arrivals allowed
    /// mbp 9/96
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <param name="ARRFile">Input argument #1</param>
    /// <param name="Narrmx">Input argument #2</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] read_arrivals_bin(int numArgsOut, MWArray ARRFile, MWArray Narrmx)
    {
      return mcr.EvaluateFunction(numArgsOut, "read_arrivals_bin", ARRFile, Narrmx);
    }


    /// <summary>
    /// Provides an interface for the read_arrivals_bin function in which the input and
    /// output
    /// arguments are specified as an array of MWArrays.
    /// </summary>
    /// <remarks>
    /// This method will allocate and return by reference the output argument
    /// array.<newpara></newpara>
    /// M-Documentation:
    /// Read the arrival time/amplitude data computed by BELLHOP
    /// usage:
    /// [ Arr, Pos ] = read_arrivals_bin( ARRFile, Narrmx );
    /// Arr is a structure containing all the arrivals information
    /// Pos is a structure containing the positions of source and receivers
    /// ARRFile is the name of the Arrivals File
    /// Narrmx is the maximum number of arrivals allowed
    /// mbp 9/96
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return</param>
    /// <param name= "argsOut">Array of MWArray output arguments</param>
    /// <param name= "argsIn">Array of MWArray input arguments</param>
    ///
    public void read_arrivals_bin(int numArgsOut, ref MWArray[] argsOut, MWArray[] argsIn)
    {
      mcr.EvaluateFunction("read_arrivals_bin", numArgsOut, ref argsOut, argsIn);
    }


    /// <summary>
    /// Provides a single output, 0-input MWArrayinterface to the ts_arons M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// TS_ARONS - compute the time history of an explosives generated shock wave
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray ts_arons()
    {
      return mcr.EvaluateFunction("ts_arons", new MWArray[]{});
    }


    /// <summary>
    /// Provides a single output, 1-input MWArrayinterface to the ts_arons M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// TS_ARONS - compute the time history of an explosives generated shock wave
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="z">Input argument #1</param>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray ts_arons(MWArray z)
    {
      return mcr.EvaluateFunction("ts_arons", z);
    }


    /// <summary>
    /// Provides a single output, 2-input MWArrayinterface to the ts_arons M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// TS_ARONS - compute the time history of an explosives generated shock wave
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="z">Input argument #1</param>
    /// <param name="w">Input argument #2</param>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray ts_arons(MWArray z, MWArray w)
    {
      return mcr.EvaluateFunction("ts_arons", z, w);
    }


    /// <summary>
    /// Provides a single output, 3-input MWArrayinterface to the ts_arons M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// TS_ARONS - compute the time history of an explosives generated shock wave
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="z">Input argument #1</param>
    /// <param name="w">Input argument #2</param>
    /// <param name="R">Input argument #3</param>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray ts_arons(MWArray z, MWArray w, MWArray R)
    {
      return mcr.EvaluateFunction("ts_arons", z, w, R);
    }


    /// <summary>
    /// Provides a single output, 4-input MWArrayinterface to the ts_arons M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// TS_ARONS - compute the time history of an explosives generated shock wave
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="z">Input argument #1</param>
    /// <param name="w">Input argument #2</param>
    /// <param name="R">Input argument #3</param>
    /// <param name="fs">Input argument #4</param>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray ts_arons(MWArray z, MWArray w, MWArray R, MWArray fs)
    {
      return mcr.EvaluateFunction("ts_arons", z, w, R, fs);
    }


    /// <summary>
    /// Provides a single output, 5-input MWArrayinterface to the ts_arons M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// TS_ARONS - compute the time history of an explosives generated shock wave
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="z">Input argument #1</param>
    /// <param name="w">Input argument #2</param>
    /// <param name="R">Input argument #3</param>
    /// <param name="fs">Input argument #4</param>
    /// <param name="T">Input argument #5</param>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray ts_arons(MWArray z, MWArray w, MWArray R, MWArray fs, MWArray T)
    {
      return mcr.EvaluateFunction("ts_arons", z, w, R, fs, T);
    }


    /// <summary>
    /// Provides the standard 0-input MWArray interface to the ts_arons M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// TS_ARONS - compute the time history of an explosives generated shock wave
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] ts_arons(int numArgsOut)
    {
      return mcr.EvaluateFunction(numArgsOut, "ts_arons", new MWArray[]{});
    }


    /// <summary>
    /// Provides the standard 1-input MWArray interface to the ts_arons M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// TS_ARONS - compute the time history of an explosives generated shock wave
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <param name="z">Input argument #1</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] ts_arons(int numArgsOut, MWArray z)
    {
      return mcr.EvaluateFunction(numArgsOut, "ts_arons", z);
    }


    /// <summary>
    /// Provides the standard 2-input MWArray interface to the ts_arons M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// TS_ARONS - compute the time history of an explosives generated shock wave
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <param name="z">Input argument #1</param>
    /// <param name="w">Input argument #2</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] ts_arons(int numArgsOut, MWArray z, MWArray w)
    {
      return mcr.EvaluateFunction(numArgsOut, "ts_arons", z, w);
    }


    /// <summary>
    /// Provides the standard 3-input MWArray interface to the ts_arons M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// TS_ARONS - compute the time history of an explosives generated shock wave
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <param name="z">Input argument #1</param>
    /// <param name="w">Input argument #2</param>
    /// <param name="R">Input argument #3</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] ts_arons(int numArgsOut, MWArray z, MWArray w, MWArray R)
    {
      return mcr.EvaluateFunction(numArgsOut, "ts_arons", z, w, R);
    }


    /// <summary>
    /// Provides the standard 4-input MWArray interface to the ts_arons M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// TS_ARONS - compute the time history of an explosives generated shock wave
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <param name="z">Input argument #1</param>
    /// <param name="w">Input argument #2</param>
    /// <param name="R">Input argument #3</param>
    /// <param name="fs">Input argument #4</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] ts_arons(int numArgsOut, MWArray z, MWArray w, MWArray R, MWArray fs)
    {
      return mcr.EvaluateFunction(numArgsOut, "ts_arons", z, w, R, fs);
    }


    /// <summary>
    /// Provides the standard 5-input MWArray interface to the ts_arons M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// TS_ARONS - compute the time history of an explosives generated shock wave
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <param name="z">Input argument #1</param>
    /// <param name="w">Input argument #2</param>
    /// <param name="R">Input argument #3</param>
    /// <param name="fs">Input argument #4</param>
    /// <param name="T">Input argument #5</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] ts_arons(int numArgsOut, MWArray z, MWArray w, MWArray R, MWArray 
                        fs, MWArray T)
    {
      return mcr.EvaluateFunction(numArgsOut, "ts_arons", z, w, R, fs, T);
    }


    /// <summary>
    /// Provides an interface for the ts_arons function in which the input and output
    /// arguments are specified as an array of MWArrays.
    /// </summary>
    /// <remarks>
    /// This method will allocate and return by reference the output argument
    /// array.<newpara></newpara>
    /// M-Documentation:
    /// TS_ARONS - compute the time history of an explosives generated shock wave
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return</param>
    /// <param name= "argsOut">Array of MWArray output arguments</param>
    /// <param name= "argsIn">Array of MWArray input arguments</param>
    ///
    public void ts_arons(int numArgsOut, ref MWArray[] argsOut, MWArray[] argsIn)
    {
      mcr.EvaluateFunction("ts_arons", numArgsOut, ref argsOut, argsIn);
    }


    /// <summary>
    /// Provides a single output, 0-input MWArrayinterface to the ts_chapman M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// Author: Gopu Potty
    /// Organization: University of Rhode Island
    /// Date
    /// Function: Explosive source model to simulate the pressure time
    /// history for a high explosive (e.g. TNT) charge. The primary reference
    /// is: Chapman, JASA v78, no 2, 1985, p672-681
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray ts_chapman()
    {
      return mcr.EvaluateFunction("ts_chapman", new MWArray[]{});
    }


    /// <summary>
    /// Provides a single output, 1-input MWArrayinterface to the ts_chapman M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// Author: Gopu Potty
    /// Organization: University of Rhode Island
    /// Date
    /// Function: Explosive source model to simulate the pressure time
    /// history for a high explosive (e.g. TNT) charge. The primary reference
    /// is: Chapman, JASA v78, no 2, 1985, p672-681
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="z">Input argument #1</param>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray ts_chapman(MWArray z)
    {
      return mcr.EvaluateFunction("ts_chapman", z);
    }


    /// <summary>
    /// Provides a single output, 2-input MWArrayinterface to the ts_chapman M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// Author: Gopu Potty
    /// Organization: University of Rhode Island
    /// Date
    /// Function: Explosive source model to simulate the pressure time
    /// history for a high explosive (e.g. TNT) charge. The primary reference
    /// is: Chapman, JASA v78, no 2, 1985, p672-681
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="z">Input argument #1</param>
    /// <param name="w">Input argument #2</param>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray ts_chapman(MWArray z, MWArray w)
    {
      return mcr.EvaluateFunction("ts_chapman", z, w);
    }


    /// <summary>
    /// Provides a single output, 3-input MWArrayinterface to the ts_chapman M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// Author: Gopu Potty
    /// Organization: University of Rhode Island
    /// Date
    /// Function: Explosive source model to simulate the pressure time
    /// history for a high explosive (e.g. TNT) charge. The primary reference
    /// is: Chapman, JASA v78, no 2, 1985, p672-681
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="z">Input argument #1</param>
    /// <param name="w">Input argument #2</param>
    /// <param name="R">Input argument #3</param>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray ts_chapman(MWArray z, MWArray w, MWArray R)
    {
      return mcr.EvaluateFunction("ts_chapman", z, w, R);
    }


    /// <summary>
    /// Provides a single output, 4-input MWArrayinterface to the ts_chapman M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// Author: Gopu Potty
    /// Organization: University of Rhode Island
    /// Date
    /// Function: Explosive source model to simulate the pressure time
    /// history for a high explosive (e.g. TNT) charge. The primary reference
    /// is: Chapman, JASA v78, no 2, 1985, p672-681
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="z">Input argument #1</param>
    /// <param name="w">Input argument #2</param>
    /// <param name="R">Input argument #3</param>
    /// <param name="fs">Input argument #4</param>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray ts_chapman(MWArray z, MWArray w, MWArray R, MWArray fs)
    {
      return mcr.EvaluateFunction("ts_chapman", z, w, R, fs);
    }


    /// <summary>
    /// Provides a single output, 5-input MWArrayinterface to the ts_chapman M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// Author: Gopu Potty
    /// Organization: University of Rhode Island
    /// Date
    /// Function: Explosive source model to simulate the pressure time
    /// history for a high explosive (e.g. TNT) charge. The primary reference
    /// is: Chapman, JASA v78, no 2, 1985, p672-681
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="z">Input argument #1</param>
    /// <param name="w">Input argument #2</param>
    /// <param name="R">Input argument #3</param>
    /// <param name="fs">Input argument #4</param>
    /// <param name="T">Input argument #5</param>
    /// <returns>An MWArray containing the first output argument.</returns>
    ///
    public MWArray ts_chapman(MWArray z, MWArray w, MWArray R, MWArray fs, MWArray T)
    {
      return mcr.EvaluateFunction("ts_chapman", z, w, R, fs, T);
    }


    /// <summary>
    /// Provides the standard 0-input MWArray interface to the ts_chapman M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// Author: Gopu Potty
    /// Organization: University of Rhode Island
    /// Date
    /// Function: Explosive source model to simulate the pressure time
    /// history for a high explosive (e.g. TNT) charge. The primary reference
    /// is: Chapman, JASA v78, no 2, 1985, p672-681
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] ts_chapman(int numArgsOut)
    {
      return mcr.EvaluateFunction(numArgsOut, "ts_chapman", new MWArray[]{});
    }


    /// <summary>
    /// Provides the standard 1-input MWArray interface to the ts_chapman M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// Author: Gopu Potty
    /// Organization: University of Rhode Island
    /// Date
    /// Function: Explosive source model to simulate the pressure time
    /// history for a high explosive (e.g. TNT) charge. The primary reference
    /// is: Chapman, JASA v78, no 2, 1985, p672-681
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <param name="z">Input argument #1</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] ts_chapman(int numArgsOut, MWArray z)
    {
      return mcr.EvaluateFunction(numArgsOut, "ts_chapman", z);
    }


    /// <summary>
    /// Provides the standard 2-input MWArray interface to the ts_chapman M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// Author: Gopu Potty
    /// Organization: University of Rhode Island
    /// Date
    /// Function: Explosive source model to simulate the pressure time
    /// history for a high explosive (e.g. TNT) charge. The primary reference
    /// is: Chapman, JASA v78, no 2, 1985, p672-681
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <param name="z">Input argument #1</param>
    /// <param name="w">Input argument #2</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] ts_chapman(int numArgsOut, MWArray z, MWArray w)
    {
      return mcr.EvaluateFunction(numArgsOut, "ts_chapman", z, w);
    }


    /// <summary>
    /// Provides the standard 3-input MWArray interface to the ts_chapman M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// Author: Gopu Potty
    /// Organization: University of Rhode Island
    /// Date
    /// Function: Explosive source model to simulate the pressure time
    /// history for a high explosive (e.g. TNT) charge. The primary reference
    /// is: Chapman, JASA v78, no 2, 1985, p672-681
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <param name="z">Input argument #1</param>
    /// <param name="w">Input argument #2</param>
    /// <param name="R">Input argument #3</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] ts_chapman(int numArgsOut, MWArray z, MWArray w, MWArray R)
    {
      return mcr.EvaluateFunction(numArgsOut, "ts_chapman", z, w, R);
    }


    /// <summary>
    /// Provides the standard 4-input MWArray interface to the ts_chapman M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// Author: Gopu Potty
    /// Organization: University of Rhode Island
    /// Date
    /// Function: Explosive source model to simulate the pressure time
    /// history for a high explosive (e.g. TNT) charge. The primary reference
    /// is: Chapman, JASA v78, no 2, 1985, p672-681
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <param name="z">Input argument #1</param>
    /// <param name="w">Input argument #2</param>
    /// <param name="R">Input argument #3</param>
    /// <param name="fs">Input argument #4</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] ts_chapman(int numArgsOut, MWArray z, MWArray w, MWArray R, MWArray 
                          fs)
    {
      return mcr.EvaluateFunction(numArgsOut, "ts_chapman", z, w, R, fs);
    }


    /// <summary>
    /// Provides the standard 5-input MWArray interface to the ts_chapman M-function.
    /// </summary>
    /// <remarks>
    /// M-Documentation:
    /// Author: Gopu Potty
    /// Organization: University of Rhode Island
    /// Date
    /// Function: Explosive source model to simulate the pressure time
    /// history for a high explosive (e.g. TNT) charge. The primary reference
    /// is: Chapman, JASA v78, no 2, 1985, p672-681
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return.</param>
    /// <param name="z">Input argument #1</param>
    /// <param name="w">Input argument #2</param>
    /// <param name="R">Input argument #3</param>
    /// <param name="fs">Input argument #4</param>
    /// <param name="T">Input argument #5</param>
    /// <returns>An Array of length "numArgsOut" containing the output
    /// arguments.</returns>
    ///
    public MWArray[] ts_chapman(int numArgsOut, MWArray z, MWArray w, MWArray R, MWArray 
                          fs, MWArray T)
    {
      return mcr.EvaluateFunction(numArgsOut, "ts_chapman", z, w, R, fs, T);
    }


    /// <summary>
    /// Provides an interface for the ts_chapman function in which the input and output
    /// arguments are specified as an array of MWArrays.
    /// </summary>
    /// <remarks>
    /// This method will allocate and return by reference the output argument
    /// array.<newpara></newpara>
    /// M-Documentation:
    /// Author: Gopu Potty
    /// Organization: University of Rhode Island
    /// Date
    /// Function: Explosive source model to simulate the pressure time
    /// history for a high explosive (e.g. TNT) charge. The primary reference
    /// is: Chapman, JASA v78, no 2, 1985, p672-681
    /// Calling sequence:
    /// Inputs:
    /// z  - charge depth in m
    /// w  - charge weight in kg
    /// R  - range in m
    /// fs - sample rate for output (Hz)
    /// T  - total duration of output waveform
    /// Output:
    /// u  - waveform samples of the shock time series
    /// </remarks>
    /// <param name="numArgsOut">The number of output arguments to return</param>
    /// <param name= "argsOut">Array of MWArray output arguments</param>
    /// <param name= "argsIn">Array of MWArray input arguments</param>
    ///
    public void ts_chapman(int numArgsOut, ref MWArray[] argsOut, MWArray[] argsIn)
    {
      mcr.EvaluateFunction("ts_chapman", numArgsOut, ref argsOut, argsIn);
    }



    /// <summary>
    /// This method will cause a MATLAB figure window to behave as a modal dialog box.
    /// The method will not return until all the figure windows associated with this
    /// component have been closed.
    /// </summary>
    /// <remarks>
    /// An application should only call this method when required to keep the
    /// MATLAB figure window from disappearing.  Other techniques, such as calling
    /// Console.ReadLine() from the application should be considered where
    /// possible.</remarks>
    ///
    public void WaitForFiguresToDie()
    {
      mcr.WaitForFiguresToDie();
    }



    #endregion Methods

    #region Class Members

    private static MWMCR mcr= null;

    private bool disposed= false;

    #endregion Class Members
  }
}
