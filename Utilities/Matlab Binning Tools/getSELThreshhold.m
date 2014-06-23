function threshholdValue = getSELThreshhold(speciesName,isImpulsive)
   impulsiveKeys = {'LF_CETACEAN','MF_CETACEAN','HF_CETACEAN','PHOCID','ORARIID'};
   impulsiveValues = {193,204,180,192,215};
   impulsiveMap = containers.Map(impulsiveKeys,impulsiveValues);
   
   nonImpulsiveKeys = {'LF_CETACEAN','MF_CETACEAN','HF_CETACEAN','PHOCID','ORARIID'};
   nonImpulsiveValues = {204, 215, 297, 197, 220};
   nonImpulsiveMap = containers.Map(nonImpulsiveKeys,nonImpulsiveValues);
   %% return the threshhold value for the given species
    switch speciesName
        case 'Balaenoptera musculus'
            if(isImpulsive)
                threshholdValue = impulsiveMap('MF_CETACEAN');
            else
                threshholdValue = nonImpulsiveMap('MF_CETACEAN');
            end            
        case 'Balaenoptera physalus'
            if(isImpulsive)
                threshholdValue = impulsiveMap('MF_CETACEAN');
            else
                threshholdValue = nonImpulsiveMap('MF_CETACEAN');
            end
        case 'Generic mysticete'
            if(isImpulsive)
                threshholdValue = impulsiveMap('LF_CETACEAN');
            else
                threshholdValue = nonImpulsiveMap('LF_CETACEAN');
            end
        case 'Generic odontocete'
            if(isImpulsive)
                threshholdValue = impulsiveMap('MF_CETACEAN');
            else
                threshholdValue = nonImpulsiveMap('MF_CETACEAN');
            end
        case 'Mesoplodon densirostris'
            if(isImpulsive)
                threshholdValue = impulsiveMap('MF_CETACEAN');
            else
                threshholdValue = nonImpulsiveMap('MF_CETACEAN');
            end
        case 'Mirounga angustirostris (adult male)'
            if(isImpulsive)
                threshholdValue = impulsiveMap('PHOCID');
            else
                threshholdValue = nonImpulsiveMap('PHOCID');
            end
        case 'Mirounga angustirostris (female)'
            if(isImpulsive)
                threshholdValue = impulsiveMap('PHOCID');
            else
                threshholdValue = nonImpulsiveMap('PHOCID');
            end
        case 'Phoca vitulina'
            if(isImpulsive)
                threshholdValue = impulsiveMap('PHOCID');
            else
                threshholdValue = nonImpulsiveMap('PHOCID');
            end
        case 'Phocoena phocoena (Atlantic)'
            if(isImpulsive)
                threshholdValue = impulsiveMap('HF_CETACEAN');
            else
                threshholdValue = nonImpulsiveMap('HF_CETACEAN');
            end
        case 'Phocoena phocoena (Pacific)'
            if(isImpulsive)
                threshholdValue = impulsiveMap('HF_CETACEAN');
            else
                threshholdValue = nonImpulsiveMap('HF_CETACEAN');
            end
        case 'Physeter macrocephalus (Atlantic)'
            if(isImpulsive)
                threshholdValue = impulsiveMap('MF_CETACEAN');
            else
                threshholdValue = nonImpulsiveMap('MF_CETACEAN');
            end
        case 'Physeter macrocephalus (Gulf of Mexico)'
            if(isImpulsive)
                threshholdValue = impulsiveMap('MF_CETACEAN');
            else
                threshholdValue = nonImpulsiveMap('MF_CETACEAN');
            end
        case 'Physeter macrocephalus (Pacific)'
            if(isImpulsive)
                threshholdValue = impulsiveMap('MF_CETACEAN');
            else
                threshholdValue = nonImpulsiveMap('MF_CETACEAN');
            end
        case 'Tursiops truncatus (coastal_Atlantic)'
            if(isImpulsive)
                threshholdValue = impulsiveMap('MF_CETACEAN');
            else
                threshholdValue = nonImpulsiveMap('MF_CETACEAN');
            end
        case 'Tursiops truncatus (coastal_Pacific)'
            if(isImpulsive)
                threshholdValue = impulsiveMap('MF_CETACEAN');
            else
                threshholdValue = nonImpulsiveMap('MF_CETACEAN');
            end
        case 'Zalophus californianus (female)'
            if(isImpulsive)
                threshholdValue = impulsiveMap('ORARIID');
            else
                threshholdValue = nonImpulsiveMap('ORARIID');
            end
        case 'Zalophus californianus (male)'
            if(isImpulsive)
                threshholdValue = impulsiveMap('ORARIID');
            else
                threshholdValue = nonImpulsiveMap('ORARIID');
            end
        case 'Ziphius cavirostris'
            if(isImpulsive)
                threshholdValue = impulsiveMap('MF_CETACEAN');
            else
                threshholdValue = nonImpulsiveMap('MF_CETACEAN');
            end
        otherwise
            error([speciesName,' is not a recognized species; no threshhold can be assigned']);            
    end   
end
