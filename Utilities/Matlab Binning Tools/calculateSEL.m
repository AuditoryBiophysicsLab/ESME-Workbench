% Graham Voysey

%% calculateSEL Plot the cuumulative sound exposure for every animat
%
% filename = the path to the simulation.exposures file

function SELs=calculateSEL(fileName,dutyCycle)

[S,fid] = readLogFileHeader(fileName);
speciesCount = length(S.speciesRecords);
timestepCount = length(S.timeStepRecords);
SELs = struct();

%for each species
for i=1:speciesCount
    %make a vector to accumulate receive levels in
    animatPowers = zeros(1,S.speciesRecords(i).animatCount);
    startID = S.speciesRecords(i).startActorID;
    endID = startID + S.speciesRecords(i).animatCount;
    %for every time step record
    for j=1:timestepCount
        %read that timeStepRecord
        timeStepRecord = readTimeStepRecord(fid,S.timeStepRecords(i).offsets);
        %for each exposure record, accumulate the SEL. 
        for k = 1:length(timeStepRecord.exposureRecords)
            exposureRecord = timeStepRecord.exposureRecords(k);
            %check if this exposure record pertains to an animat of this species
            if((exposureRecord.actorID >= startID) && (exposureRecord.actorID < endID)) %maybe a fencepost error here
                %it does!  first, get the spl. 
                spl = exposureRecord.peakSPL;
                %from this, work back to linear power
                power = (10^(spl/20))^2*(dutyCycle);
                %and sum these powers for all time.
                animatPowers(exposureRecord.actorID-startID) = animatPowers(exposureRecord.actorID-startID) + power;
            end
        end
    end
    SELs(i).energies = 10*log10(animatPowers); % convert summed power into cuumulative SEL per animal
    SELs(i).speciesName = S.speciesRecords(i).name;
end