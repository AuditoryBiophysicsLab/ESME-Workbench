
%% read the entire log
function [log] = logread(filePath)
[logStruct,fid] = readLogFileHeader(filePath);
log = struct;
log.FileHeader=logStruct;
for i=1:length(logStruct.timeStepRecords),
    log.record=readTimeStepRecord(fid,logStruct.timeStepRecords(i).offsets);
end
end