%% parses the ticks of a .NET TimeSpan object and returns a sensibly-formatted string representing the time.
function timestamp = ticks2timespan(ticks)
ticksPerMs = cast(10000,'uint64');
ticksPerSec = ticksPerMs * 1000;
ticksPerMin = ticksPerSec * 60;
ticksPerHour = ticksPerMin * 60;
ticksPerDay = ticksPerHour * 24;

days = uint64(ticks/ticksPerDay);
ticks = ticks - days*ticksPerDay;

hours = uint64(ticks/ ticksPerHour);
ticks = ticks - hours * ticksPerHour;

minutes = uint64(ticks/ticksPerMin);
ticks = ticks - minutes * ticksPerMin;

seconds = uint64(ticks/ticksPerSec);
ticks = ticks - seconds * ticksPerSec;

if(days > 0)
timestamp = sprintf('%02.2d-%02.2d:%02.2d:%02.2d.%d',days,hours,minutes,seconds,ticks);
else
timestamp = sprintf('%02.2d:%02.2d:%02.2d.%d',hours,minutes,seconds,ticks);
end
end