%% LOGREAD Access to ESME Workbench simulation output.
%    S = logread('simulation.log') imports the entire result of an 
%    ESME Workbench simulation into Matlab and returns it as a struct S.
%
%    S contains:
%       - FileHeader   :  A struct that contains metadata about the simulation
%          and a list of record offsets.  Consult 'help readLogFileHeader' for
%          more detail.
%
%       - record	   : A struct that contains, for each time step, the position
%       and exposure records for every platform and animal.
function [log] = logread(filePath)
[logStruct,fid] = readLogFileHeader(filePath);
log = struct;
log.FileHeader=logStruct;
for i=1:length(logStruct.timeStepRecords),
    log.record(i)=readTimeStepRecord(fid,logStruct.timeStepRecords(i).offsets);
end
end