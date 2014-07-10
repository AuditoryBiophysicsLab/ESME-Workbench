%resampleAndGraphAnimatStats(sels,spls,isImpulsive,samplePercentage,resampleCount,isSELOnly,scenarioName)
%   This function plots take estimates (based on values specified in
%   getSELThreshhold and getSPLThreshhold; currently these reflect the
%   draft SEL and SPL guidelines proposed by NMFS in May 2014 ) as a
%   labelled histogram.  Optionally, it can also perform a monte-carlo
%   resampling of large animat populations with replacement over a given
%   number of trials and produce histograms with variability estimates. 

function [species] = resampleAndGraphAnimatStats(sels, spls,isImpulsive,samplePercentage,resampleCount,isSELOnly,scenarioName)
% accumulate threshholds
selThreshholds = zeros(1,length(sels));
splThreshholds = zeros(1,length(spls));    
species = struct;
%% SELs
    %for each species        
    for i = 1:length(sels)
        %look up the threshhold for takes
        selThreshholds(i) = getSELThreshhold(sels(i).speciesName,isImpulsive);
        %resample and look up exposures
        resampledSELs = struct;
            for c = 1:resampleCount
            %generate a random sample from the total population
            animatIDs = sort(randsample(length(sels(i).energies),ceil(samplePercentage*length(sels(i).energies))));
            %write down the exposures
            vals = [];
            inds = [];
            takes = 0;
                for a = 1:length(animatIDs)
                    animatID = animatIDs(a);
                    value = sels(i).energies(animatID);
                        if(isfinite(value) && value > 0)
                            vals = [vals, value];
                            inds = [inds,animatID];
                                % and note the takes when they happen
                                if(value >=selThreshholds(i))
                                    takes = takes +1;
                                end
                        end
                end
            resampledSELs(c).values = vals;
            resampledSELs(c).inds = inds;
            resampledSELs(c).takes = takes;
            end
         %create a standard nested struct of data
         species(i).name = sels(i).speciesName;
         species(i).resampledSELs = resampledSELs;     
    end
    
%% SPLs   
    for i = 1:length(spls)
        %make sure the species are ordered the same for SEL and SPLs
        assert(strcmp(species(i).name,spls(i).speciesName))
        splThreshholds(i) = getSPLThreshhold(spls(i).speciesName);
        resampledSPLs = struct;
         for c = 1:resampleCount
            %generate a random sample from the total population
            animatIDs = sort(randsample(length(spls(i).peakSPLs),ceil(samplePercentage*length(spls(i).peakSPLs))));
            %plot line graph            
            vals = [];
            inds = [];
            takes = 0;
                for a = 1:length(animatIDs)
                    animatID = animatIDs(a);
                    value = spls(i).peakSPLs(animatID);
                        if(isfinite(value) && value > 0)
                            vals = [vals, value];
                            inds = [inds,animatID];
                                if(value >=splThreshholds(i))
                                    takes = takes +1;
                                end
                        end
                end
            resampledSPLs(c).values = vals;
            resampledSPLs(c).inds = inds;
            resampledSPLs(c).takes = takes;
         end         
         species(i).resampledSPLs = resampledSPLs;     
    end
     
    %% compute take statistics from resampled structs
   means = zeros(1,length(species));
   bars = means;
   causes = {};
   barlabels={};
   
   for i = 1:length(species)
        tempSEL = species(i).resampledSELs;
        tempSPL = species(i).resampledSPLs;
        meanSEL = mean([tempSEL.takes]);
        stderrSEL = std([tempSEL.takes])/sqrt(length(tempSEL));
        meanSPL = mean([tempSPL.takes]);
        stderrSPL = std([tempSPL.takes])/sqrt(length(tempSPL));
        
        if(meanSPL > meanSEL && ~isSELOnly)
            causes{i} = [species(i).name,': SPL-driven takes'];
            barlabels{i} = [species(i).name,' (',num2str(splThreshholds(i)),' dB SPL)'];
            means(i) = meanSPL;
            bars(i) = stderrSPL;
        else
            causes{i} = [species(i).name,': SEL-driven takes'];
            barlabels{i} = [species(i).name,' (',num2str(selThreshholds(i)),' dB SEL)'];
            means(i) = meanSEL;
            bars(i) = stderrSEL;
        end
        
%         if(meanSEL >= meanSPL)
%             causes{i} = [species(i).name,': SEL-driven takes'];
%             barlabels{i} = [species(i).name,' (',num2str(selThreshholds(i)),' dB SEL)'];
%             means(i) = meanSEL;
%             bars(i) = stderrSEL;
%         else
%             causes{i} = [species(i).name,': SPL-driven takes'];
%             barlabels{i} = [species(i).name,' (',num2str(splThreshholds(i)),' dB SPL)'];
%             means(i) = meanSPL;
%             bars(i) = stderrSPL;
%         end        
   end
   
   figure(1); hold on; 
   barwitherr(bars,means);
    for x = 1:numel(means)
        if(means(x)~=0)
            text(x,means(x),num2str(means(x)),'HorizontalAlignment','center','VerticalAlignment','bottom','FontSize',14);
        end
    end
    
    if(max(means) < 10 )
            ylim([0,10]);
        else
            ylim([0,floor(max(means)+.1*max(means))])
    end
   
   set(gca,'XTick',[1:length(species)],'XTickLabel',barlabels,'FontSize',14);  
   fix_xticklabels(gca,0.1,{'FontSize',14});
   tt = title(['Estimated Takes (',scenarioName,'):',num2str(resampleCount),' resamplings at ',num2str(samplePercentage*100),'% total population']); set(tt,'FontSize',16);
   yy = ylabel('Take count'); set(yy,'FontSize',16);
   if(~isSELOnly)
    th = annotation(1,'textbox',[.688 .848 .211 .067],'String',causes);
   end
   set(1,'PaperPosition',[0 0 12 6]);
   %set (1, 'Units', 'normalized', 'Position', [0,0,1,1]);
   
   saveas(1,'takes.fig');            
   saveas(1,'takes.png');  
   close(1);
end