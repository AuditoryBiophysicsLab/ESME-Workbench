function plotAnimatExposures(filename)
[spls, sels] = calculateStats(filename,1/1000);
save('alldata.mat','sels','spls');
end




