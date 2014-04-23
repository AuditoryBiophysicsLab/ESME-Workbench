% Graham Voysey

%% calculateSEL Plot the cuumulative sound exposure for every animat
%
% filename = the path to the simulation.exposures file

function SPls=calculatePeakSPL(fileName)

[S,fid] = readLogFileHeader(fileName);
speciesCount = length(S.speciesRecords);
timestepCount = length(S.timeStepRecords);
SPls = struct();

%for each species
for i=1:speciesCount
    %make a vector to accumulate receive levels in
    animatSPLs = zeros(1,S.speciesRecords(i).animatCount);
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
                %it does!  is the spl logged greater than what we've
                %already logged? if so, overwrite the old value.  
                if(animatSPLs(exposureRecord.actorID-startID) < exposureRecord.peakSPL)
                    animatSPLs(exposureRecord.actorID-startID) = exposureRecord.peakSPL;
                end
            end
        end
    end
    SPls(i).peakSPLs = animatSPLs; % convert summed power into cuumulative SEL per animal
    SPls(i).speciesName = S.speciesRecords(i).name;
end