INSERT INTO Transmission_Loss.TransmissionLoss 	(
	idTransmissionLoss, Latitude, Longitude, 
	Depth_meters,VerticalBeamWidth_degrees, VerticalLookAngle_degrees, 
	LowFrequency_Hz, HighFrequency_Hz, BinaryFileName, 
	CalculationStarted, CalculationFinished, RadialCount, 
	Radius_meters, idRangeAxis, idDepthAxis 
) VALUES ( 
	@id, @lat, @lon, @dep, @vbw, @vla, @lof, @hif, @fil, @sta, 
	@fin, @cnt, @rad, @rid, @did
) ON DUPLICATE KEY UPDATE
	Latitude=@lat, Longitude=@lon, Depth_meters=@dep,
	VerticalBeamWidth_degrees=@vbw, VerticalLookAngle_degrees=@vla, LowFrequency_Hz=@lof, 
	HighFrequency_Hz=@hif, BinaryFileName=@fil, CalculationStarted=@sta, 
	CalculationFinished=@fin, RadialCount=@cnt, Radius_meters=@rad,
	idRangeAxis=@rid, idDepthAxis=@did;
	