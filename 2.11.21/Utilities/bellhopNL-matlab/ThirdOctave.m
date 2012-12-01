%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% This is from another Matlab routine I found.  Goes from 1.25 Hz to >>20,000 Hz
%
% to compute the 1/3-Octave band power spectral density, peak sound pressure,
% and sound energy flux density

function [peak, maxE, EFD, fc]=ThirdOctave(src_waveform,fs)

fft_len=2*fs;

peak=max(src_waveform);
peak=20*log10(peak);

%compute the lower, center and upper frequency of 48 1/3-Octave bands

fc = [ 1.26 1.58 2.00 2.51 3.16 3.98 5.01 6.31 7.94 10.0 12.59 ...
15.85 19.95 25.12 31.62 39.81 50.12 63.10 79.43 100.00 125.89 158.49 ...
199.53 251.19 316.23 398.11 501.19 630.96 794.33 1000.0 1258.9 1584.9 1995.3 ...
2511.9 3162.3 3981.1 5011.9 6309.6 7943.3 10000.0 12589.3 15848.9 19952.6 ...
29205 58410 116820 147184 185440]; 

n      = length(fc);
flow   = fc / 1.122;
fup    = fc * 1.122;
octave = zeros( n, 1 );

%integrate the power within the 1/3-Octave bands

[ Pxx, F ] = periodogram( src_waveform, [], fft_len, fs );

% Put results into the bins and integrate.
for i = 1 : n
  j = find( ( F >= flow( i ) ) & ( F <= fup( i ) ) );
  octave( i ) = octave( i ) + sum( Pxx( j ) );
end

EFD  = 10 * log10( octave ); % convert to (dB re 1 uPa x sec)
EFD  = 10 * log10( octave ./ 2 ); % convert to (dB re 1 uPa x sec)

maxE = max( EFD );

end
