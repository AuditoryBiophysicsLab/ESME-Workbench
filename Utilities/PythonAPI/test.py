from pysim import PySim

sim = PySim("""C:\Users\Graham Voysey\Desktop\simulation.exposures""")
record = sim.ReadTimeStepRecord(sim.TimeStepRecordOffsets[0])
record5 = sim.ReadTimeStepRecord(sim.TimeStepRecordOffsets[5])
recordLast = sim.ReadTimeStepRecord(sim.TimeStepRecordOffsets[-1])
print "record 1 had {0} actors and began at {1}".format(record.ActorCount, record.StartTime)
print "record 5 had {0} actors and began at {1}".format(record5.ActorCount, record5.StartTime)
print "the last record had {0} actors and began at {1}".format(recordLast.ActorCount, recordLast.StartTime)