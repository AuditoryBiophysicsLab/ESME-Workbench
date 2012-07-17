function y = readuint64(fid)
x = fread(fid,8,'uint8');
v=zeros(1,16);
y=uint64(0);
byte =0;

for i=1:length(x)
    v=uint64(x(i));
    v=bitshift(v,byte);
    y=bitor(y,v);
    byte=byte+8;
end
end
