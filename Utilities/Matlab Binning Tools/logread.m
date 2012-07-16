function logStruct = logread(filePath)
logStruct = struct;

%open the file, or bail with an error
[fid, message] = fopen(filePath);
assert(fid~=-1, message);


end
