using System;
using System.Linq;
using HRC.Utility;

namespace ESME.TransmissionLoss.Bellhop
{
    public static class ExplosiveWaveformGenerators
    {
        /// <summary>
        /// Compute the time history of an explosives generated shock wave
        /// </summary>
        /// <param name="z">charge depth in m</param>
        /// <param name="w">charge weight in kg</param>
        /// <param name="r">range in m</param>
        /// <param name="fs">sample rate for output (Hz)</param>
        /// <param name="T">total duration of output waveform (s)</param>
        /// <returns>waveform samples of the shock time series</returns>
        public static double[] Arons(double z, double w, double r, double fs, double T)
        {
            var nsamples = (int)Math.Round(fs * T);
            var t = new double[nsamples];
            for (var i = 0; i < t.Length; i++) t[i] = (i / fs) - 0.1;
            var w13 = Math.Pow(w, 1.0 / 3.0);
            // peak pressure
            var ps = 5.04e+13 * Math.Pow(w13 / r, 1.13);       // muPa
            // exponential decay constant
            var tauS = 9.25e-05 * w13 * Math.Pow(w13 / r, -0.22);
            var u = new double[nsamples];
            for (var i = 0; i < t.Length; i++)
                if (t[i] < 0) u[i] = 0;                         // Hilbert transform is earlier in time.
                else u[i] = ps * Math.Exp(-t[i] / tauS);
            return u;
        }

        public static double[] Chapman(double z, double w, double r, double fs, double T)
        {
            // Author: Gopu Potty
            // Organization: University of Rhode Island
            // Date
            // Function: Explosive source model to simulate the pressure time
            // history for a high explosive (e.g. TNT) charge. The primary reference
            // is: Chapman, JASA v78, no 2, 1985, p672-681
            //
            // Calling sequence:
            //  Inputs:
            //   z  - charge depth in m
            //   w  - charge weight in kg
            //   R  - range in m
            //   fs - sample rate for output (Hz)
            //   T  - total duration of output waveform
            //  Output:
            //   u  - waveform samples of the shock time series

            // Change Log:
            //
            // March 30,2006 Haw-Jye Shyu did the following
            // 1) convert Gopu's spectrum plot from the unit of erg/cm^2/Hz to uPa^2-sec/Hz
            // 2) write out the wave file
            // 3) write out the src.mat
            //
            // Major modifications to incorporate into delay-sum (channel convolution)
            // Fri Jun 25 15:25:26 PDT 2010 jcp@hlsresearch.com made these changes:
            // 1) Changed units associated with charge weight w to kilograms (MKS std)
            // 2) Added sample rate and duration of waveform to the input arguments
            // 3) Trimmed the output arguments down to just the waveform samples
            // 4) Cosmetic tweaks to calculations of the pressures and time constants
            // 5) Very major changes to calculation of the waveform (see comments)
            // 6) Removed plotting
            //
            // $Id: $
            // ReSharper disable InconsistentNaming
            var nsamples = (int)Math.Round(fs * T);
            var time = new double[nsamples];
            for (var i = 0; i < time.Length; i++) time[i] = (i / fs);
            var w13 = Math.Pow(w, 1.0 / 3.0);

            var z0 = z + 10.1;
            var z056 = Math.Pow(z0, -5.0 / 6.0);
            // P_s = 5.04d+13*(w^(1/3)/R)^(1.13);      % Eq 1 (P_s is in muPa - Slifko)
            var ps = 5.04e+13 * Math.Pow(w13 / r, 1.13);

            // tau_s = 8.12d-05*w^(1/3)*(w^(1/3)/R)^(-0.14);   % Eq 12 
            var tauS = 8.12e-05 * w13 * Math.Pow(w13 / r, -0.14);

            // T1 = 2.11*w^(1/3)*z_0^(-5/6);           % first bubble pulse period - eqn 6
            var T1 = 2.11 * w13 * z056;
            // T2 = 1.48*w^(1/3)*z_0^(-5/6);           % second bubble pulse period - eqn 6
            var T2 = 1.48 * w13 * z056;
            // theta_s = 0.194*w^(1/3)*z_0^(-5/6);     % pulse duration of shock wave - Eq 13
            var thetaS = 0.194 * w13 * z056;

            // P_1 = 1.49d+12*(w^(1/3)/R)*z_0^(0.33);   % Eq 15 - first bubble pulse pressure 
            var p1 = 1.49e+12 * (w13 / r) * Math.Pow(z0, 0.33);
            // P_2 = 3.93d+11*(w^(1/3)/R)*z_0^(0.28);   % Eq 16 - second bubble pulse pressure
            var p2 = 3.93e+11 * (w13 / r) * Math.Pow(z0, 0.28);
            // P_min1 = 5.0d+10*(w^(1/3)/R)*z_0^(0.60); % Eq 17 - minimum pressure
            var pMin1 = 5.0e+10 * (w13 / r) * Math.Pow(z0, 0.60);
            // P_min2 = 0.58*P_min1;                    % See Table III
            var pMin2 = 0.58 * pMin1;

            // tau_r = 1.36d-02*w^(1/3)*z_0^(-0.6);    % Eq 20 - first bubble pulse rise time 
            var tauR = 1.36e-02 * w13 * Math.Pow(z0, -0.6);
            // tau_d = 0.87d-02*w^(1/3)*z_0^(-0.6);    % Eq 21 - first bubble pulse decay time
            var tauD = 0.87e-02 * w13 * Math.Pow(z0, -0.6);

            // tau_r2 = 2.0*tau_r;
            var tauR2 = 2.0 * tauR;
            // tau_d2 = 2.0*tau_d;
            var tauD2 = 2.0 * tauD;

            // theta_1 = 0.45*w^(1/3)*z_0^(-5/6);      % Eq 22 - bubble pulse durations
            var theta1 = 0.45 * w13 * z056;
            // theta_min1 = 1.64*w^(1/3)*z_0^(-5/6);   % Eq 23 - negative pressure duration
            var thetaMin1 = 1.64 * w13 * z056;
            // theta_min2 = 0.65*theta_min1;           % See Table VI
            var thetaMin2 = 0.65 * thetaMin1;

            // For the time interval from 0 <= t < T1 (to peak of 1-st bubble pulse),
            // we model the time series as linear combination of two exponentials and
            // a general quadratic polynomial. The arrival times for the initial
            // shock, the first bubble pulse, and the zero crossings from the bubble
            // rarefaction are given by similitude expressions in Chapman. The time
            // associated with peak negative pressure is not explicitly given by
            // Chapman, but the exponential terms will have significantly decayed
            // at that point, so we can just use the average of the zero crossing
            // times. We form a linear system that is then solved to find the
            // unknown coefficients.

            // Time values where pressure is specified by Chapman via similitude expressions
            // t0 = 0.0;			% time of initial shock
            var t0 = 0.0;
            // t1 = theta_s;			% time of first zero crossing
            var t1 = thetaS;
            // t2 = t1 + theta_min1*0.5;	% time of peak negative pressure (approximate)
            var t2 = t1 + thetaMin1 * 0.5;
            // t3 = t1 + theta_min1;		% time of second zero crossing
            var t3 = t1 + thetaMin1;
            // t4 = T1;			% time of first bubble pulse
            var t4 = T1;

            // The expression for the pressure as a function of time is;
            // p(t) = x0*exp(-t/tau_s) + x1*exp((t-t4)/tau_r) + x2*t^2 + x3*t + x4
            // We now form the coefficient matrix;

            var a = new[,]
            {
                {                 1.0, Math.Exp((t0 - t4) / tauR),    0.0, 0.0, 1.0},
                {Math.Exp(-t1 / tauS), Math.Exp((t1 - t4) / tauR), t1 * t1, t1, 1.0},
                {Math.Exp(-t2 / tauS), Math.Exp((t2 - t4) / tauR), t2 * t2, t2, 1.0},
                {Math.Exp(-t3 / tauS), Math.Exp((t3 - t4) / tauR), t3 * t3, t3, 1.0},
                {Math.Exp(-t4 / tauS),                        1.0, t4 * t4, t4, 1.0}
            };
            // Form the R.H.S. of the linear system (pressure values at above times)
            var b = new[] { ps, 0.0, -pMin1, 0.0, p1 };
            var x = Numerics.LeftDivide(a, b);
            // Evaluate the expression for time < arrival of first bubble pulse

            // t = time(time < t4);
            var time1 = time.Where(t => t < t4).ToArray();

            // Pa = x(1)*exp(-t/tau_s) + x(2)*exp((t-t4)/tau_r) + x(3)*t.^2 + x(4)*t + x(5);
            var Pa = new double[time1.Length];
            for (var i = 0; i < time1.Length; i++)
            {
                var t = time1[i];
                Pa[i] = x[0] * Math.Exp(-t / tauS) + x[1] * Math.Exp((t - t4) / tauR) + x[2] * t * t + x[3] * t + x[4];
            }

            // For the time interval from the first to second bubble pulses, or more
            // specifically, T1 <= t < (T1+T2) we basically repeat the same song and
            // dance from above, substituting in the respective times, pressures

            // Time values where pressure is specified by Chapman via similitude expressions

            // t0 = T1;				% time of first bubble pulse
            t0 = T1;
            // t1 = theta_s + theta_min1 + theta_1;	% time of first zero crossing
            t1 = thetaS + thetaMin1 + theta1;
            // t2 = t1 + theta_min2*0.5;	% time of peak negative pressure (approximate)
            t2 = t1 + thetaMin2 * 0.5;
            // t3 = t1 + theta_min2;		% time of second zero crossing
            t3 = t1 + thetaMin2;
            // t4 = T1 + T2;			% time of second bubble pulse
            t4 = T1 + T2;

            // The expression for the pressure as a function of time is;
            // p(t) = x0*exp(-(t-t0)/tau_d) + x1*exp((t-t4)/tau_r2) + x2*t^2 + x3*t + x4
            // We now form the coefficient matrix;

            // A = zeros(5, 5);
            // A(1,:) = [                 1.0, exp((t0-t4)/tau_r2), t0^2, t0, 1.0 ];
            // A(2,:) = [ exp(-(t1-t0)/tau_d), exp((t1-t4)/tau_r2), t1^2, t1, 1.0 ];
            // A(3,:) = [ exp(-(t2-t0)/tau_d), exp((t2-t4)/tau_r2), t2^2, t2, 1.0 ];
            // A(4,:) = [ exp(-(t3-t0)/tau_d), exp((t3-t4)/tau_r2), t3^2, t3, 1.0 ];
            // A(5,:) = [ exp(-(t4-t0)/tau_d),                 1.0, t4^2, t4, 1.0 ];
            a = new[,]
            {
                {                        1.0, Math.Exp((t0 - t4) / tauR2), t0 * t0, t0, 1.0},
                {Math.Exp(-(t1 - t0) / tauD), Math.Exp((t1 - t4) / tauR2), t1 * t1, t1, 1.0},
                {Math.Exp(-(t2 - t0) / tauD), Math.Exp((t2 - t4) / tauR2), t2 * t2, t2, 1.0},
                {Math.Exp(-(t3 - t0) / tauD), Math.Exp((t3 - t4) / tauR2), t3 * t3, t3, 1.0},
                {Math.Exp(-(t4 - t0) / tauD),                         1.0, t4 * t4, t4, 1.0}
            };

            // Form the R.H.S. of the linear system (pressure values at above times)
            // b = [ P_1; 0.0; -P_min2; 0.0; P_2 ];
            b = new[] { p1, 0.0, -pMin2, 0.0, p2 };
            // Solve for the coefficients
            // x = A\b;
            x = Numerics.LeftDivide(a, b);

            // Evaluate the expression for time <= arrival of second bubble pulse

            // t = time((time >= t0) & (time < t4));
            var time2 = time.Where(t => t >= t0 && t < t4).ToArray();

            // Pb = x(1)*exp(-(t-t0)/tau_d) + x(2)*exp((t-t4)/tau_r2) ...
            //                              + x(3)*t.^2 + x(4)*t + x(5);
            var Pb = new double[time2.Length];
            for (var i = 0; i < time2.Length; i++)
            {
                var t = time2[i];
                Pb[i] = x[0] * Math.Exp(-(t - t0) / tauD) + x[1] * Math.Exp((t - t4) / tauR2) + x[2] * t * t + x[3] * t + x[4];
            }

            // Pressure is simple exponential for time values beyond the second bubble pulse

            // t = time(time >= t4);
            var time3 = time.Where(t => t >= t4).ToArray();

            // Pc = P_2*exp(-(t-t4)/tau_d2);
            var Pc = new double[time3.Length];
            for (var i = 0; i < time3.Length; i++)
            {
                var t = time3[i];
                Pc[i] = p2 * Math.Exp(-(t - t4) / tauD2);
            }

            // Concatenate the pressure histories and we are done...

            // u = [ Pa ; Pb; Pc ];
            var u = new double[Pa.Length + Pb.Length + Pc.Length];
            Array.Copy(Pa, u, Pa.Length);
            Array.Copy(Pb, 0, u, Pa.Length, Pb.Length);
            Array.Copy(Pc, 0, u, Pa.Length + Pb.Length, Pc.Length);
            // that's all folks!!!
            return u;
            // ReSharper restore InconsistentNaming
        }
    }
}
