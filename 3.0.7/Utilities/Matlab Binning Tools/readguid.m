%% reads a 128-bit integer from a file, returns it as two 64-bit uints. 

function guid = readguid(fid)
guid = [readuint64(fid),readuint64(fid)];
end