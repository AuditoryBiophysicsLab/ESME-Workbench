% First, run bellhop on your chosen environment, to produce an arrivals file.
% construct the full path to the bellhop executable

%!bellhop.exe( 'score' )

% Change the 10+ parameters at the top of the runREFMS, for your given run.
% Run by typing "runREFMS" from the Matlab window OR, from a terminal
% window, type "matlab -nodesktop"; "run_NL_delaysum".  (Without double quotes.)

% With new version of bellhop, you use socal.env, e.g., so can print this on plot neatly.
ARRFIL = 'score.arr';      % Arrival file you just made with bellhop.exe filename(.env)
H2Odep = 1200;             % Water depth (m) 
z      = 2500 / 3.2808;    % charge depth (meters)
w      = 300 * 0.45359237; % charge weight (kilograms)
fs     = 88200;            % sample rate for output (Hz)
T      = 0.25;             % total duration of output waveform (seconds)
model  = 'arons';          % similitude time series model: arons or chapman
Rdep   = 1000 / 3.2808;    % Desired receiver depth to plot (m) 
Rrg    = 1823 / 3.2808;    % Desired receiver range to plot (m) 

% Calculate the Pressure from the explosive source (SI units).

[ rtsmat, rr, rd ] = NL_delaysum( ARRFIL, z, w, fs, T, model );

timeMsec = [ 1 : length( rtsmat( :, 1, 1 ) ) ] .* 1000 ./ fs;
rtsSav   = rtsmat;          % 1/3-octave program requires SI units.

% Find the grid point that matches the Desired receiver point.

Rg_diff = abs( rr - Rrg );
[~, idxRg ] = min( Rg_diff );

Rd_diff = abs( rd - Rdep );
[~, idxRd ] = min( Rd_diff );


%%

% Plot the 1D spike of explosive pressure.

figure

pp = plot( timeMsec, rtsmat( :, idxRg, idxRd ) );         % 1D
set( pp, 'LineWidth', 3. );
grid;

% Text and labels:
% count number of characters in ARRFIL to start of .arr (only for certain versions of Bellhop)
nchars  = strfind( ARRFIL, '.arr' );   % find quotes
if ( nchars > 0 )
  ARRFIL = ARRFIL( 1 : nchars( 1 ) - 1 ); 
end

xx = xlabel( 'Time After First Arrival (ms)' );
set( xx, 'FontSize', 12 );
tt = title( 'Pressure vs. Time' );
set( tt, 'FontSize', 12 );

% Find limits to place text attractively
set( gca, 'YLim', [0 40] )
set( gca, 'XLim', [98 113] )
ytxt = get( gca, 'YLim' );
yincr= ( ytxt( 2 ) - ytxt( 1 ) ) / 100.;
xtxt = get( gca, 'XLim' );
xincr= ( xtxt( 2 ) - xtxt( 1 ) ) / 100.;

yy = ylabel( 'Pressure (\mu Pa)' );
set( yy, 'FontSize', 12 );
% set( gca, 'XLim', [100 115] );
set( gca, 'YLim', [-2 20e10] );
ytxt  = get( gca, 'YLim' );
yincr = ( ytxt( 2 ) - ytxt( 1 ) ) / 100.;

tt = text( mean( xtxt ), ytxt( 2 )-4*yincr,  upper( ARRFIL ) );
set( tt, 'FontWeight', 'bold' );

tt = text( mean( xtxt ), ytxt( 2 )-8*yincr,  ['Model: '  upper( model )] );
set( tt, 'FontWeight', 'bold' );

tt = text( mean( xtxt ), ytxt( 2 )-12*yincr, ['Yield = ' num2str( w ) ' kg '] );
set( tt, 'FontWeight', 'bold' );

tt = text( mean( xtxt ), ytxt( 2 )-16*yincr, ['Charge Depth: ' num2str( z ) ' m'] );
set( tt, 'FontWeight', 'bold' );

tt = text( mean( xtxt ), ytxt( 2 )-20*yincr, ['Range = ' num2str( rr( idxRg ) ) ' m' ] );
set( tt, 'FontWeight', 'bold' );

tt = text( mean( xtxt ), ytxt( 2 )-28*yincr, ['Receiver Depth = ' num2str( rd( idxRd ) ) ' m' ] );
set( tt, 'FontWeight', 'bold' );

tt = text( mean( xtxt ), ytxt( 2 )-32*yincr, ['Water Depth = ' num2str( H2Odep ) ' m' ] );
set( tt, 'FontWeight', 'bold' );

tt = text( mean( xtxt ), ytxt( 2 )-40*yincr, ['Peak Pressure = ' num2str( max( rtsmat( :, idxRg, idxRd ) )/1e+09 ) ' kPa' ] );
set( tt, 'FontWeight', 'bold' );
%%

% 2D color plot of all peaks in range and depth

figure;     
ss = size( rtsSav );

sm = zeros( ss( 3 ), ss( 2 ) );

for irg = 1:ss( 2 )
  for idep = 1:ss( 3 )
    sm( idep, irg ) = 20 * log10( max( abs( rtsSav( :, irg, idep ) )  ) );
  end
end
pcolor( sm );
shading interp
caxis( [200 250] );
colorbar;

tx = get( gca, 'XTick' );
ty = get( gca, 'YTick' );

set( gca, 'XTickLabel', round( rr( tx ) )  );
set( gca, 'YTickLabel', round( rd( ty ) )  );

title( 'Peak Pressure' );
xlabel( 'Range (m)' );
ylabel( 'Depth (m)' );

set( gca, 'YDir', 'reverse' )
%%

% Spectrum Plot %%%%%%%%%%

[ peak, maxE, EFD, Fc ] = ThirdOctave( rtsSav( :, idxRg, idxRd ), fs );
figure;
ss = semilogx( Fc, EFD );
set( ss, 'LineWidth', 3. );
set( gca, 'XLim', [1 10000] );

ylabel( '1/3-Octave-Band Energy Flux Density Level (dB)' );
xlabel( 'Frequency (Hz)' );

title( 'Energy Spectrum' );    %for SOCAL -- 39.32 N, 118.89 W -- Water Depth 3937 ft

ytxt  = get( gca, 'YLim' );
yincr = ( ytxt( 2 ) - ytxt( 1 ) ) / 100.;
tt = text( 30, mean( ytxt ),            ['Model is ' upper( model ) ] );
set( tt, 'FontWeight', 'bold' );

tt = text( 30, mean( ytxt )-4 * yincr,  ['Peak Sound Pressure = ' num2str( peak )] );
set( tt, 'FontWeight', 'bold' );

tt = text( 30, mean( ytxt )-8 * yincr,  ['Maximum Energy = ' num2str( maxE ) ' dB' ] );
set( tt, 'FontWeight', 'bold' );

tt = text( 30, mean( ytxt )-12 * yincr, ['Range = ' num2str( rr( idxRg ) ) ' m' ] );
set( tt, 'FontWeight', 'bold' );

tt = text( 30, mean( ytxt )-16 * yincr, ['Source Depth = ' num2str( z ) ' m' ] );
set( tt, 'FontWeight', 'bold' );

tt = text( 30, mean( ytxt )-20 * yincr, ['Receiver Depth = ' num2str( rd( idxRd ) ) ' m' ] );
set( tt, 'FontWeight', 'bold' );

tt = text( 30, mean( ytxt )-24 * yincr, ['Water Depth = ' num2str( H2Odep ) ' m' ] );
set( tt, 'FontWeight', 'bold' );

grid;

