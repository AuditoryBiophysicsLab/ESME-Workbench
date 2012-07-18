%% implements a .NET-like ReadString method to read a variable-length record from a file.
function s = readString(fid)
 length = cast(fread(fid,1,'ubit8'),'uint32');
 length = bitand(length,127);
 %length2 = cast(fread(fid,1,'ubit8'),'uint32');
 %length = length + (length2*128);
 s = fread(fid,length,'*char')'; 
end
