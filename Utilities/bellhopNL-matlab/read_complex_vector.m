function v = read_complex_vector(filename)

fid = fopen(filename);
v = [];
while ~feof(fid)
    data = fscanf(fid, '%g %g %g %g', 4);
    if (numel(data) == 4)
        c = complex(data(1), data(2));
        v = [v c];
    end
end
v = v';
 