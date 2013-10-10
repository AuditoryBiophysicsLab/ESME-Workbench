function write_complex_vector(v, filename)

fid = fopen(filename, 'wt');
for e = v
    fprintf(fid, ' %24.16e %24.16e  %24.16e  %24.16e\n', real(e), imag(e), abs(e), phase(e));
end
fclose(fid);
