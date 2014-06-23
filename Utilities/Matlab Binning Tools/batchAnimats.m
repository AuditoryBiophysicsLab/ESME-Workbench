function batchAnimats(name)
clear sels; 
clear spls;
load alldata;
graphAnimatStats(sels,spls);
resampleAndGraphAnimatStats(sels,spls,1,1,1,1,name);
end