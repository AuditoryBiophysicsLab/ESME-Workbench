%% parses the ticks of a .NET TimeSpan object and returns a sensibly-formatted string representing the time.
function timestamp = ticks2datetime(ticks)
    serialTime =  double(ticks) * 1e-7/86400 + 367;
    timestamp = datestr(datenum(serialTime));
end