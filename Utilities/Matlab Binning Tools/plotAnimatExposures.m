function plotAnimatExposures(filename, dutyCycle)
[spls, sels] = calculateStats(filename,dutyCycle);
save('alldata.mat','sels','spls');
end




