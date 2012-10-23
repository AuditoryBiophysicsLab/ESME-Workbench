function u = ts_chapman(z, w, R, fs, T)

% Author: Gopu Potty
% Organization: University of Rhode Island
% Date
% Function: Explosive source model to simulate the pressure time
% history for a high explosive (e.g. TNT) charge. The primary reference
% is: Chapman, JASA v78, no 2, 1985, p672-681
%
% Calling sequence:
%  Inputs:
%   z  - charge depth in m
%   w  - charge weight in kg
%   R  - range in m
%   fs - sample rate for output (Hz)
%   T  - total duration of output waveform
%  Output:
%   u  - waveform samples of the shock time series

% Change Log:
%
% March 30,2006 Haw-Jye Shyu did the following
% 1) convert Gopu's spectrum plot from the unit of erg/cm^2/Hz to uPa^2-sec/Hz
% 2) write out the wave file
% 3) write out the src.mat
%
% Major modifications to incorporate into delay-sum (channel convolution)
% Fri Jun 25 15:25:26 PDT 2010 jcp@hlsresearch.com made these changes:
% 1) Changed units associated with charge weight w to kilograms (MKS std)
% 2) Added sample rate and duration of waveform to the input arguments
% 3) Trimmed the output arguments down to just the waveform samples
% 4) Cosmetic tweaks to calculations of the pressures and time constants
% 5) Very major changes to calculation of the waveform (see comments)
% 6) Removed plotting
%
% $Id: $

% time samples
nsamples = round(fs * T);
time = (0:nsamples-1).' / fs;

z_0 = z + 10.1;                         % Account for one atm pressure at z=0

P_s = 5.04d+13*(w^(1/3)/R)^(1.13);      % Eq 1 (P_s is in muPa - Slifko)
tau_s = 8.12d-05*w^(1/3)*(w^(1/3)/R)^(-0.14);   % Eq 12 

T1 = 2.11*w^(1/3)*z_0^(-5/6);           % first bubble pulse period - eqn 6
T2 = 1.48*w^(1/3)*z_0^(-5/6);           % second bubble pulse period - eqn 6
theta_s = 0.194*w^(1/3)*z_0^(-5/6);     % pulse duration of shock wave - Eq 13

P_1 = 1.49d+12*(w^(1/3)/R)*z_0^(0.33);   % Eq 15 - first bubble pulse pressure 
P_2 = 3.93d+11*(w^(1/3)/R)*z_0^(0.28);   % Eq 16 - second bubble pulse pressure
P_min1 = 5.0d+10*(w^(1/3)/R)*z_0^(0.60); % Eq 17 - minimum pressure
P_min2 = 0.58*P_min1;                    % See Table III

tau_r = 1.36d-02*w^(1/3)*z_0^(-0.6);    % Eq 20 - first bubble pulse rise time 
tau_d = 0.87d-02*w^(1/3)*z_0^(-0.6);    % Eq 21 - first bubble pulse decay time

tau_r2 = 2.0*tau_r;
tau_d2 = 2.0*tau_d;

theta_1 = 0.45*w^(1/3)*z_0^(-5/6);      % Eq 22 - bubble pulse durations
theta_min1 = 1.64*w^(1/3)*z_0^(-5/6);   % Eq 23 - negative pressure duration
theta_min2 = 0.65*theta_min1;           % See Table VI

% For the time interval from 0 <= t < T1 (to peak of 1-st bubble pulse),
% we model the time series as linear combination of two exponentials and
% a general quadratic polynomial. The arrival times for the initial
% shock, the first bubble pulse, and the zero crossings from the bubble
% rarefaction are given by similitude expressions in Chapman. The time
% associated with peak negative pressure is not explicitly given by
% Chapman, but the exponential terms will have significantly decayed
% at that point, so we can just use the average of the zero crossing
% times. We form a linear system that is then solved to find the
% unknown coefficients.

% Time values where pressure is specified by Chapman via similitude expressions

t0 = 0.0;			% time of initial shock
t1 = theta_s;			% time of first zero crossing
t2 = t1 + theta_min1*0.5;	% time of peak negative pressure (approximate)
t3 = t1 + theta_min1;		% time of second zero crossing
t4 = T1;			% time of first bubble pulse

% The expression for the pressure as a function of time is;
% p(t) = x0*exp(-t/tau_s) + x1*exp((t-t4)/tau_r) + x2*t^2 + x3*t + x4
% We now form the coefficient matrix;

A = zeros(5, 5);
A(1,:) = [            1.0, exp((t0-t4)/tau_r),  0.0, 0.0, 1.0 ];
A(2,:) = [ exp(-t1/tau_s), exp((t1-t4)/tau_r), t1^2,  t1, 1.0 ];
A(3,:) = [ exp(-t2/tau_s), exp((t2-t4)/tau_r), t2^2,  t2, 1.0 ];
A(4,:) = [ exp(-t3/tau_s), exp((t3-t4)/tau_r), t3^2,  t3, 1.0 ];
A(5,:) = [ exp(-t4/tau_s),                1.0, t4^2,  t4, 1.0 ];

% Form the R.H.S. of the linear system (pressure values at above times)

b = [ P_s; 0.0; -P_min1; 0.0; P_1 ];

% Solve for the coefficients

x = A\b;

% Evaluate the expression for time < arrival of first bubble pulse

t = time(time < t4);

Pa = x(1)*exp(-t/tau_s) + x(2)*exp((t-t4)/tau_r) + x(3)*t.^2 + x(4)*t + x(5);

% For the time interval from the first to second bubble pulses, or more
% specifically, T1 <= t < (T1+T2) we basically repeat the same song and
% dance from above, substituting in the respective times, pressures

% Time values where pressure is specified by Chapman via similitude expressions

t0 = T1;				% time of first bubble pulse
t1 = theta_s + theta_min1 + theta_1;	% time of first zero crossing
t2 = t1 + theta_min2*0.5;	% time of peak negative pressure (approximate)
t3 = t1 + theta_min2;		% time of second zero crossing
t4 = T1 + T2;			% time of second bubble pulse

% The expression for the pressure as a function of time is;
% p(t) = x0*exp(-(t-t0)/tau_d) + x1*exp((t-t4)/tau_r2) + x2*t^2 + x3*t + x4
% We now form the coefficient matrix;

A = zeros(5, 5);
A(1,:) = [                 1.0, exp((t0-t4)/tau_r2), t0^2, t0, 1.0 ];
A(2,:) = [ exp(-(t1-t0)/tau_d), exp((t1-t4)/tau_r2), t1^2, t1, 1.0 ];
A(3,:) = [ exp(-(t2-t0)/tau_d), exp((t2-t4)/tau_r2), t2^2, t2, 1.0 ];
A(4,:) = [ exp(-(t3-t0)/tau_d), exp((t3-t4)/tau_r2), t3^2, t3, 1.0 ];
A(5,:) = [ exp(-(t4-t0)/tau_d),                 1.0, t4^2, t4, 1.0 ];

% Form the R.H.S. of the linear system (pressure values at above times)

b = [ P_1; 0.0; -P_min2; 0.0; P_2 ];

% Solve for the coefficients

x = A\b;

% Evaluate the expression for time <= arrival of first bubble pulse

t = time((time >= t0) & (time < t4));

Pb = x(1)*exp(-(t-t0)/tau_d) + x(2)*exp((t-t4)/tau_r2) ...
                             + x(3)*t.^2 + x(4)*t + x(5);

% Pressure is simple exponential for time values beyond the second bubble pulse

t = time(time >= t4);

Pc = P_2*exp(-(t-t4)/tau_d2);

% Concatenate the pressure histories and we are done...

u = [ Pa ; Pb; Pc ];

% that's all folks!!!

return;

% end of ts_chapman.m
