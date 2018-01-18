from esme.__main__ import EsmeLog

sim = EsmeLog().FromFileName("""C:\Users\Graham Voysey\Desktop\simulation.exposures""")
record = sim.ReadTimeStepRecord(sim.TimeStepRecordOffsets[0])
record5 = sim.ReadTimeStepRecord(sim.TimeStepRecordOffsets[5])
recordLast = sim.ReadTimeStepRecord(sim.TimeStepRecordOffsets[-1])
print(f"record 1 had {record.ActorCount} actors and began at {record.StartTime}")
print(f"record 5 had {record5.ActorCount} actors and began at {record5.StartTime}")
print(f"the last record had {recordLast.ActorCount} actors and began at {recordLast.StartTime}")

#start_time = time.clock()
#sim = EsmeLog("""C:\Users\Graham Voysey\Desktop\simulation.exposures""")
#exposures = []
#for i in xrange(0, len(sim.TimeStepRecordOffsets)):
#    sim.ReadTimeStepRecord(sim.TimeStepRecordOffsets[i])
#print time.clock() - start_time , "seconds"