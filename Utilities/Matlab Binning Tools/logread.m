
%% Reads the trailer and every log entry in a given log file.  Warning: very large log files may occupy a great deal of memory.
function [log] = logread(filePath)
[logStruct,fid] = readLogFileHeader(filePath);
log = struct;
log.FileHeader=logStruct;
for i=1:length(logStruct.timeStepRecords),
    log.record(i)=readTimeStepRecord(fid,logStruct.timeStepRecords(i).offsets);
end
end