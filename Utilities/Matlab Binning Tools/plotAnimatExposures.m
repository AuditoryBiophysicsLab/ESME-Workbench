function plotAnimatExposures(filename)
tstart = tic;
sels = calculateSEL(filename,1/1000);
sels_finished = toc(tstart);
save('sels.mat','sels')
disp(['finished calculating SELs in ',num2str(sels_finished), ' seconds']);
spls = calculatePeakSPL(filename);
save('spls.mat','spls')
spls_finished = toc(sels_finished);
disp(['finished calculating SPLs in ',num2str(spls_finished), ' seconds, ' num2str(toc(tstart)),'total.' ]);

save('alldata.mat','sels','spls');
if(length(sels) ~= length(spls))
    error('something has gone horribly awry');
end




