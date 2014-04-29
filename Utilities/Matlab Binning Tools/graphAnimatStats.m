function graphAnimatStats(sels, spls)
% plot SELs
    for i = 1:length(sels)        
        %plot line graph
        energyVal = sels(i).energies(isfinite(sels(i).energies));
        energyInd = find(isfinite(sels(i).energies));
        figure(i);plot(energyInd,energyVal,'bs-.','MarkerSize',8,'MarkerFaceColor','b','LineWidth',1);set(gca,'FontSize',16);
        set (i, 'Units', 'normalized', 'Position', [0,0,1,1]);
        x = xlabel(['Simulated Animal ID: (',sels(i).speciesName,')']); set(x,'FontSize',16);
        y = ylabel('SEL, dB re 1 \mu Pa^2s','Interpreter','TeX');set(y,'FontSize',16);
        t = title('Cumulative SEL, 24 hours. SL 258 dB re 1\mu Pa, CF 250 Hz, duty cycle 0.1s/10s','Interpreter','TeX');
        set(t,'FontSize',16);        
        saveas(i,['SEL line graph ',sels(i).speciesName],'fig');
        close(i);
        %plot histogram
        figure(i);hist(energyVal,15); set(gca,'FontSize',16);
        set (i, 'Units', 'normalized', 'Position', [0,0,1,1]);
        xx = xlabel('Binned SEL, dB re 1 \mu Pa^2s','Interpreter','TeX'); set(xx,'FontSize',16);
        yy= ylabel('Count');set(yy,'FontSize',16);
        tt = title(['Cumulative SEL Histogram ',sels(i).speciesName]);set(tt,'FontSize',16);
        saveas(i,['SEL histogram ',sels(i).speciesName],'fig');
        close(i);
    end
    
    for i = 1:length(spls)
        % plot line graph
        vals = nonzeros(spls(i).peakSPLs);
        inds = find(spls(i).peakSPLs);      
        figure(i);plot(inds,vals,'bs-.','MarkerSize',8,'MarkerFaceColor','b','LineWidth',1);set(gca,'FontSize',16);
        set (i, 'Units', 'normalized', 'Position', [0,0,1,1]);
        x = xlabel(['Simulated Animal ID: (',spls(i).speciesName,')']);set(x,'FontSize',16);
        y = ylabel('RMS SPL, dB re 1 \mu Pa','Interpreter','TeX');set(y,'FontSize',16);
        t = title('RMS SPL, 24 hours.  SL 258 dB re 1\mu Pa, CF 250 Hz, duty cycle 0.1s/10s','Interpreter','TeX');set(t,'FontSize',16);
        saveas(i,['SPL line graph ',sels(i).speciesName],'fig');
        close(i);        
        % plot histogram
        figure(i);hist(vals,15);set(gca,'FontSize',16);
        set (i, 'Units', 'normalized', 'Position', [0,0,1,1]);
        xx = xlabel('Binned SPL, dB re 1 \mu Pa','Interpreter','TeX');set(xx,'FontSize',16);
        yy = ylabel('Count');set(yy,'FontSize',16);
        tt = title(['Cumulative SEL Histogram ',sels(i).speciesName]);   set(tt,'FontSize',16);
        saveas(i,['SPL histogram ',sels(i).speciesName],'fig');
        close(i); 
    end

end
