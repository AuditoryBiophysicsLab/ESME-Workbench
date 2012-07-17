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

logStruct.timeStepSize = readuint64(fid); % in ticks
logStruct.startTime = readuint64(fid); % in ticks
logStruct.endTime = readuint64(fid); % in ticks
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
    [name, actorID, guid] = readActorNameGuid(fid);
    modeRecords(i).name = name;
    modeRecords(i).actorID = actorID;
    modeRecords(i).guid = guid;
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
    timeStepRecords(i).offsets=readuint64(fid);
end
logStruct.timeStepRecords = timeStepRecords;
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

function [name,startActorID,animatCount,guid] = readSpeciesNameGuid(fid)
animatCount = fread(fid,1,'int32');
startActorID = fread(fid,1,'int32');
name = readString(fid);
guid = readguid(fid);
end

