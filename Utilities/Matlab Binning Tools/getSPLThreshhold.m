function threshholdValue = getSPLThreshhold(speciesName)
   keys = {'CETACEAN','PINNIPED'};
   values = {180,190};
   threshholdMap = containers.Map(keys,values);   
   switch speciesName
        case 'Balaenoptera musculus'
            threshholdValue = threshholdMap('CETACEAN');
        case 'Balaenoptera physalus'
            threshholdValue = threshholdMap('CETACEAN');
        case 'Generic mysticete'
            threshholdValue = threshholdMap('CETACEAN');
        case 'Generic odontocete'
            threshholdValue = threshholdMap('CETACEAN');
        case 'Mesoplodon densirostris'
            threshholdValue = threshholdMap('CETACEAN');
        case 'Mirounga angustirostris (adult male)'
            threshholdValue = threshholdMap('PINNIPED');
        case 'Mirounga angustirostris (female)'
            threshholdValue = threshholdMap('PINNIPED');
        case 'Phoca vitulina'
            threshholdValue = threshholdMap('PINNIPED');
        case 'Phocoena phocoena (Atlantic)'
            threshholdValue = threshholdMap('CETACEAN');
        case 'Phocoena phocoena (Pacific)'
            threshholdValue = threshholdMap('CETACEAN');
        case 'Physeter macrocephalus (Atlantic)'
            threshholdValue = threshholdMap('CETACEAN');
        case 'Physeter macrocephalus (Gulf of Mexico)'
            threshholdValue = threshholdMap('CETACEAN');
        case 'Physeter macrocephalus (Pacific)'
            threshholdValue = threshholdMap('CETACEAN');
        case 'Tursiops truncatus (coastal_Atlantic)'
            threshholdValue = threshholdMap('CETACEAN');
        case 'Tursiops truncatus (coastal_Pacific)'
            threshholdValue = threshholdMap('CETACEAN');
        case 'Zalophus californianus (female)'
            threshholdValue = threshholdMap('PINNIPED');
        case 'Zalophus californianus (male)'
            threshholdValue = threshholdMap('PINNIPED');
        case 'Ziphius cavirostris'
            threshholdValue = threshholdMap('CETACEAN');
        otherwise
            error([speciesName,' is not a recognized species; no threshhold can be assigned']);            
   end   
end