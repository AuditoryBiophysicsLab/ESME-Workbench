function u = ts_arons(z, w, R, fs, T)

% TS_ARONS - compute the time history of an explosives generated shock wave
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

% time samples
nsamples = round( fs * T );
t        = ( 0 : nsamples - 1 ).' / fs;
t        = t - .1;

z_0 = z + 10.1;                          % Account for one atm pressure at z=0

% peak pressure
P_s   = 5.04d+13 * ( w^(1/3) / R )^(1.13);       % muPa

% exponential decay constant
tau_s = 9.25d-05 * w^(1/3) * ( w^(1/3) / R ) ^(-0.22);

% waveform samples
u             = zeros( size( t ) );
ineg          = find( t >= 0 );                   % Hilbert Xform is earlier in time.
u( ineg )     = P_s * exp( -t( ineg ) / tau_s );

% end of ts_arons.m
