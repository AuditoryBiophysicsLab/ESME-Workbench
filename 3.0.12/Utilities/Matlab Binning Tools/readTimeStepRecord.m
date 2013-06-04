%% readTimeStepRecord Access to ESME Workbench simulation time step data
%    S = readTimeStepRecord(fid,offset) imports the data for a given time
%    during an ESME Workbench simulation and returns it as a struct S.
%       fid is a file handle obtained by fid = fopen('simulation.log')
%       offset is a valid offset in bytes obtained with readLogFileHeader.
%
%   S contains: 
%       - header                  : A struct that contains the start time, number of
%          total active animats and platforms, and its own offset.
%
%       - actorPositionRecords(i) : A list of structs containing the
%          latitude, longitude, and depth of the platform or animat.  The
%          index of the record corresponds to the ID of the platform or
%          animat
%
%       - exposureRecords(i)      : A list of structs containing all
%       recorded exposures at this time step.  Each exposure lists the ID
%       of the exposed animat, the ID of the exposing mode, the peak SPL
%       (in dB re 1 uPa) and the energy (in uPa*s).

%% read an individual time step record from the currently open log file, given its file ID and an offset, in bytes, from the beginning of the file.  Offsets are specified in the file trailer.
function timeStruct = readTimeStepRecord(fid,offset)
[header,fid]=readTimeStepHeader(fid,offset);
timeStruct.header = header;
actorPositionRecords = struct;
for i=1:header.actorCount,
    [lat,lon,depth] = readActorPositionRecord(fid);
    actorPositionRecords(i).lat = lat;
    actorPositionRecords(i).lon = lon;
    actorPositionRecords(i).depth = depth;
end
timeStruct.actorPositionRecords = actorPositionRecords;

exposureRecordCount = fread(fid,1,'int32');
exposureRecords = struct;
for i=1:exposureRecordCount
    [actorID,modeID,peakSPL,energy]= readActorExposureRecord(fid);
    exposureRecords(i).actorID = actorID;
    exposureRecords(i).modeID = modeID;
    exposureRecords(i).peakSPL = peakSPL;
    exposureRecords(i).energy = energy;
end
timeStruct.exposureRecords = exposureRecords;
end

function [header,fid] = readTimeStepHeader(fid,offset)
status = fseek(fid,offset,'bof');
assert(status ~= -1, ferror(fid));
magic = readuint64(fid);
assert(magic==hex2uint64('d3c603dd0d7a1ee6'),'Error reading time step header, magic number not seen at expected location');

header.startTime = ticks2timespan(readuint64(fid));
header.actorCount = fread(fid,1,'int32');
header.offset = offset;
end

%% helper functions to read ESME data classes into structs
function [lat,lon,depth] = readActorPositionRecord(fid)
lat= fread(fid,1,'single');
lon= fread(fid,1,'single');
depth= fread(fid,1,'single');
end

function [actorID, modeID,peakSPL,energy]=readActorExposureRecord(fid)
actorID =fread(fid,1,'int32');
modeID=fread(fid,1,'int32');
peakSPL=fread(fid,1,'single');
energy=fread(fid,1,'single');
end