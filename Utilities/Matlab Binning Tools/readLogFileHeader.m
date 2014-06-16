%% readLogFileHeader Access to ESME Workbench simulation metadata
%    S = readLogFileHeader('simulation.log') imports the metadata about
%    a given simulation and returns it as a struct S. 
%
%    S contains:        
%       - timeStepSize       : A string representation of the simulation time
%       sampling size
%
%       - startTime          : The date and time the simulation began running
%
%       - endTime            : The date and time the simulation completed
%
%       - creatingUser       : The name of the user who ran the simulation
%
%       - ScenarioRecord     : A struct containing the name and GUID of the
%           scenario that was simulated.
%
%       - platformRecords(i) : A list of structs containing the name, ID, and GUID of
%           each simulated platform
%
%       - modeRecords(i)     : A list of structs containing the name, ID, and GUID
%           of each simulated mode
%
%       - speciesRecords(i)  : A list of structs containing the name,
%           startID, count, and GUID of each simulated species
%
%       - timeStepRecords(i) : A list of structs containing the file offset
%          for the beginning of each time step record in the simulation log
%          in bytes.  Consult 'help readTimeStepRecord' for more detail

%% read the log file trailer of a given log file, and return metadata and a file handle.
function [logStruct,fid] = readLogFileHeader(filePath)
logStruct = struct;
%% open the file, or bail with an error
[fid, message] = fopen(filePath);
assert(fid ~= -1, message);

%% seek to the end and read the magic number and offset
status = fseek(fid,-16,'eof');
assert(status ~= -1,ferror(fid));
trailerOffset = fread(fid,1,'int64');
magic = readuint64(fid);
assert(magic==hex2uint64('a57d8ee659dc45ec'),'Error reading simulation log file, magic number not seen at expected location');


%% jump to the offset and read metadata
status = fseek(fid,trailerOffset,'bof');
assert(status~=-1,ferror(fid));

logStruct.timeStepSize = ticks2timespan(readuint64(fid)); % in ticks
logStruct.startTime =ticks2datetime(readuint64(fid)); % in ticks
logStruct.endTime = ticks2datetime(readuint64(fid)); % in ticks
logStruct.creatingUser = readString(fid);
logStruct.creatingComputer = readString(fid);
[name, guid]= readNameGuidRecord(fid);
logStruct.ScenarioRecord.name = name;
logStruct.ScenarioRecord.guid = guid;

% read in each platform record
numPlatformRecords = fread(fid,1,'int32');
platformRecords = struct;
for i= 1:numPlatformRecords,
    [name, actorID, guid] = readActorNameGuid(fid);
    platformRecords(i).name = name;
    platformRecords(i).actorID = actorID;
    platformRecords(i).guid = guid;
end
logStruct.platformRecords = platformRecords;

%read in each mode record
numModeRecords = fread(fid,1,'int32');
modeRecords = struct;
for i= 1:numModeRecords,
    [name, actorID, guid, platformGuid] = readModeNameGuid(fid);
    modeRecords(i).name = name;
    modeRecords(i).actorID = actorID;
    modeRecords(i).guid = guid;
    modeRecords(i).platformGuid = platformGuid;
end
logStruct.modeRecords = modeRecords;

%read in each species record
numSpeciesRecords = fread(fid,1,'int32');
speciesRecords = struct;
for i= 1:numSpeciesRecords,
    [name, startActorID,animatCount, guid] = readSpeciesNameGuid(fid);
    speciesRecords(i).name = name;
    speciesRecords(i).startActorID = startActorID;
    speciesRecords(i).animatCount = animatCount;
    speciesRecords(i).guid = guid;
end
logStruct.speciesRecords = speciesRecords;

%read in each timestep offset
numTimeStepOffsets = fread(fid,1,'int32');
timeStepRecords = struct;
for i = 1:numTimeStepOffsets,
    timeStepRecords(i).offsets=cast(readuint64(fid),'int64');
end
logStruct.timeStepRecords = timeStepRecords;
%% seek back to the beginning of the file.
status = fseek(fid,0,'bof');
assert(status~=-1,ferror(fid));
end

%% helper functions to read ESME data classes into structs
function [name, guid] = readNameGuidRecord(fid)
name = readString(fid);
guid = readguid(fid);
end

function [name, actorID, guid] = readActorNameGuid(fid)
actorID = fread(fid,1,'int32');
name = readString(fid);
guid = readguid(fid);
end

function [name, actorID, guid, platformGuid] = readModeNameGuid(fid)
actorID = fread(fid,1,'int32');
name = readString(fid);
guid = readguid(fid);
platformGuid = readguid(fid);
end

function [name,startActorID,animatCount,guid] = readSpeciesNameGuid(fid)
animatCount = fread(fid,1,'int32');
startActorID = fread(fid,1,'int32');
name = readString(fid);
guid = readguid(fid);
end

