from esme.__main__ import EsmeLog

sim = EsmeLog("""C:\\Users\\Graham Voysey\\Desktop\\simulation.exposures""")
record = sim.timestep_record(sim.timestep_record_offsets[0])
record5 = sim.timestep_record(sim.timestep_record_offsets[5])
recordLast = sim.timestep_record(sim.timestep_record_offsets[-1])
print(f"record 1 had {record.actor_count} actors and began at {record.start_time}")
print(f"record 5 had {record5.actor_count} actors and began at {record5.start_time}")
print(f"the last record had {recordLast.actor_count} actors and began at {recordLast.start_time}")

#start_time = time.clock()
#sim = EsmeLog("""C:\Users\Graham Voysey\Desktop\simulation.exposures""")
#exposures = []
#for i in xrange(0, len(sim.TimeStepRecordOffsets)):
#    sim.timestep_record(sim.TimeStepRecordOffsets[i])
#print time.clock() - start_time , "seconds"