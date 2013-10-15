function [ rtsmat, rr, rd ] = NL_delaysum( ARRFIL, z, w, fs, T, model )

% NL_DELAYSUM - Incorporates nonlinear effects using similitude equations
%
% Usage: rtsmat = NL_delaysum(ARRFIL, z, w, fs, T, model)
%
%  Inputs:
%    ARRFIL - string containing name of (binary) BELLHOP arrivals file
%         z - charge depth (meters)
%         w - charge weight (kilograms)
%        fs - sample rate for output (Hz)
%         T - total duration of output waveform (seconds)
%     model - similitude time series model (either: 'arons' or 'chapman')
%
%  Output:
%    rtsmat - matrix of received time series samples (one column for each rcv)
%        rr - receiver ranges
%        rd - receiver depths
%
% Based on delaysum from the Acoustics Toolbox, which computes the receiver
% time series by summing up the echoes in the waveguide based on the
% amplitude and delay of each echo.
%
% Modified by J. Peterson to account for the nonlinear effects via
% similitude formulas. He also added the Arons formula for the explosive
% waveform
%
% Further work by L. Henderson fixing an issue due to the fact that the
% Hilbert transform is non-causal, i.e. starts before the original waveform
%
% $Id: $

if strcmp( upper( model ), 'ARONS' )
  ts_model = @ts_arons;
elseif strcmp( upper( model ), 'CHAPMAN' )
  ts_model = @ts_chapman;
else
  error('NL_delaysum: unrecognized time series model');
end;

% reduction velocity (should exceed fastest possible arrival)

c = 1537.0;
% c = 1500.0;        % For shallow-water case; c = 1482 -> 1485.

Narrmx = 100;
% Narrmx = 10;       % For shallow-water case at +_ 4 deg fan of rays.

isd = 1;	% which source (depth) to use

% read the BELLHOP arrivals file
[ Arr, Pos ] = read_arrivals_bin( ARRFIL, Narrmx );
disp( 'Done reading arrivals' );

ircv   = 1;
Tshift = 0.002;
%cljh Tshift = 0.0;          % This didn't do the trick; see ts_arons.m
nsamples = round( fs * T );

rr     = Pos.r.range;
rd     = Pos.r.depth;
nrr    = length( rr );
nrd    = length( rd );
rtsmat = zeros( nsamples, nrr, nrd );

timeMsec = [ 1 : nsamples ] .*1000 ./fs;
% figure(1); plot(timeMsec, 'w')
% hold on

tic
% loop over receiver depths
for ird = 1 : nrd

  % loop over receiver ranges
  for irr = 1 : nrr

     narr = Arr.Narr( irr, ird, isd );

     % determine the min, max of all arrival times at this receiver
     Arr_min = min( squeeze( Arr.delay( irr, 1 : narr, ird, isd ) ) );
     Arr_max = max( squeeze( Arr.delay( irr, 1 : narr, ird, isd ) ) );

     % compute a reasonable start time based on the arrival times
     tstart = Arr_min - Tshift;

     % initialize the time series
     rts = zeros( nsamples, 1 );
      
     % loop over arrivals at this receiver
     for iarr = 1 : narr

       % arrival time relative to start of rcv timeseries
       Tarr = Arr.delay( irr, iarr, ird, isd ) - tstart;

       % compute (guess-timate) of the ray path length to this receiver
       path_len = 1500.0 * Arr.delay( irr, iarr, ird, isd );
%      path_len = 1483.0 * Arr.delay( irr, iarr, ird, isd );   % for shallow-water case.

       % complex amplitude of this arrival, with spherical spreading removed
       camp = Arr.A( irr, iarr, ird ) * path_len;

       % evaluate the similitude expression for the computed path length
       u_shock = ts_model( z, w, path_len, fs, T );

       % compute the hilbert transform of the similitude waveform
       h_shock = hilbert( u_shock );

       % compute index in the rcv time series when this ray arrives
       it1  = 1 + round( fs * Tarr );

       % add the contribution from this arrival
       rts( it1 : end ) = rts( it1 : end ) + real( camp * h_shock( 1 : nsamples - it1 + 1 ) );

%        figure(1); pp = plot(timeMsec(it1:end),             u_shock(1:nsamples-it1+1) , 'r')  ;
%        set(pp,'LineWidth',3.);
%        hold on
%
%        figure(1); pp = plot(timeMsec(it1:end),imag(        h_shock(1:nsamples-it1+1) ) ) ;
%        set(pp,'LineWidth',3.);
%
%        figure(1); pp = plot(timeMsec(it1:end),real( camp * h_shock(1:nsamples-it1+1) ), 'g' );
%        set(pp,'LineWidth',3.);
%
%        legend('Pressure','Hilbert Transform','Complex Amplitude of the 4 Transformed Arrivals')
%        set(gca,'XLim',[80 140]);
%        set(gca,'YLim',[-8e10 8e10]);
%        xlabel('Time (msec)');
%        ylabel('Pressure (uPa)');

     end	% next arrival, iarr
      
    % store the received time series in the output matrix
    rtsmat( :, irr, ird ) = rts;

  end	% loop over receiver ranges

end	% loop over receiver depths
disp(toc);
return
