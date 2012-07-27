% gvoysey
% 9 JUL 2012
%
% automated plotting and display of simulator output.  written to parse the
% XML output as of ESME rev 3316 (committed 8 JUL 2012).  
%
% returns the XML file as a struct
function s = plotSpeciesModeHistograms(filePath, generatePlots)
%read in the XML output
s = xml_read(filePath);
% break if all we want is the struct.
if(~generatePlots) 
    return;
else
%generate one histogram per species.
for i=1:length(s.AnimatSpecies.Species)
    %rename for easy access
    thisSpecies = s.AnimatSpecies.Species(i);
    
    numBins = length(thisSpecies.BinWidths.Bin);
    %get bin tick labels
    tickLabels = cell(1,numBins);
    for ii=1:numBins        
        tickLabels{ii} = num2str(cell2mat(thisSpecies.BinWidths.Bin(ii)));
    end
    
    %for each mode,
    numModes = length(thisSpecies.Modes.Mode);
    SPLexposures = zeros(numBins,numModes);
    Energyexposures = zeros(numBins,numModes);
    legendNames = cell(1,numModes);
    for j=1:numModes
        thisMode = thisSpecies.Modes.Mode(j);
        %build up a legend string
        legendNames{j}= thisMode.Name;
        %accumulate exposures
         SPLexposures(:,j) = cell2mat(thisMode.Exposure(1).Bin);
         Energyexposures(:,j) = cell2mat(thisMode.Exposure(2).Bin);
        %//todo once energy calculations are correct                
    end
    
    %plot a stacked histogram
    figure;hold on; set(gca,'XTickLabel',tickLabels,'XTick',[1:1:numBins],'YScale','log');    
    xlabel('Bins, dB SPL');ylabel('Bin Count');
    bar(SPLexposures,'grouped');    
    legend(legendNames);
    title(['Location: ', s.Location.Name, '| Scenario: ',s.Scenario.Name, '| Species: ', thisSpecies.Name]);
    
    figure;hold on; set(gca,'XTickLabel',tickLabels,'XTick',[1:1:numBins],'YScale','log');    
    xlabel('Bins, muPa^2 * s');ylabel('Bin Count');
    bar(Energyexposures,'grouped');    
    legend(legendNames);
    title(['Location: ', s.Location.Name, '| Scenario: ',s.Scenario.Name, '| Species: ', thisSpecies.Name]);
end
end
end
