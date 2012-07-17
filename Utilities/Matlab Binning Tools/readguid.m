function guid = readguid(fid)
%gA = fread(fid,1,'bit64');
%gB = fread(fid,1,'bit64');
%guid = bin2dec(bitsll(gA,64)+gB); % maybe?
guid = [readuint64(fid),readuint64(fid)];
end