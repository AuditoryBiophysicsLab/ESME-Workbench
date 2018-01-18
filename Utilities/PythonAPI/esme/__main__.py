import datetime
import struct
import uuid

import attr

MAGIC_FOOTER = int("a57d8ee659dc45ec", 16)
MAGIC_TIMESTEP_RECORD = int("d3c603dd0d7a1ee6", 16)

@attr.s
class EsmeLog(object):
    filename = attr.ib()
    trailer_offset = attr.ib(default=None)
    timestep_size = attr.ib(default=None)
    start_time = attr.ib(default=None)
    end_time = attr.ib(default=None)
    creating_user = attr.ib(default=None)
    creating_computer = attr.ib(default=None)
    scenario_record = attr.ib(default=None)
    platform_records = attr.ib(default=None)
    mode_records = attr.ib(default=None)
    species_records = attr.ib(default=None)
    timestep_record_offsets = attr.ib(default=None)

    def __attrs_post_init__(self):
        """Read the footer from the log file and populate values"""
        with open(self.filename, "rb") as l:
            #  preliminary work; read offset and magic number or die.
            b = BinaryStream(l)
            b.base_stream.seek(-16, 2)
            self.trailer_offset = b.readUInt64()
            magic = int(b.readUInt64())
            if magic != MAGIC_FOOTER:
                raise IOError('magic number not seen at expected location')

            #  read payload of the footer
            b.base_stream.seek(self.trailer_offset, 0)
            self.timestep_size = datetime.timedelta(microseconds=b.readUInt64() / 10)
            self.start_time = datetime.datetime(1, 1, 1) + datetime.timedelta(microseconds=int(b.readUInt64()) / 10)
            self.end_time = datetime.datetime(1, 1, 1) + datetime.timedelta(microseconds=int(b.readUInt64()) / 10)
            self.creating_user = b.readString()
            self.creating_computer = b.readString()
            self.scenario_record = b.readString(), uuid.UUID(bytes=b.readBytes(16))

            platformCount = b.readInt32()
            self.platform_records = []
            for i in range(0, platformCount):
                self.platform_records.append(PlatformRecord(b.readInt32(), b.readString(), uuid.UUID(bytes=b.readBytes(16))))

            modeCount = b.readInt32()
            self.mode_records = []
            for i in range(0, modeCount):
                self.mode_records.append(ModeRecord(b.readInt32(), b.readString(), uuid.UUID(bytes=b.readBytes(16)), uuid.UUID(bytes=b.readBytes(16))))

            speciesCount = b.readInt32()
            self.species_records = []
            for i in range(0, speciesCount):
                self.species_records.append(SpeciesRecord(b.readInt32(), b.readInt32(), b.readString(), uuid.UUID(bytes=b.readBytes(16))))

            offsetCount = b.readInt32()
            self.timestep_record_offsets = []
            for i in range(0, offsetCount):
                self.timestep_record_offsets.append(b.readUInt64())

    def ReadTimeStepRecord(self, offset):
        """

        @param offset: One element of a EsmeLog.TimeStepRecordOffset list.
        @return: A TimeStepRecord object, containing the starting time of this collection of exposures relative to simulation start,
                the number of actors logged in this time step, a list of actor positions (latitude, longitude, depth), and a list of actor exposure records.
        """
        with open(self.filename, "rb") as l:
            b = BinaryStream(l)
            # jump to the beginning and read the header.
            b.base_stream.seek(offset, 0)
            if int(b.readUInt64()) != MAGIC_TIMESTEP_RECORD:
                raise IOError('magic number not seen at expected location')
            result = TimeStepRecord(datetime.datetime(1, 1, 1) + datetime.timedelta(microseconds=int(b.readUInt64()) / 10), b.readInt32())
            for i in range(0, result.ActorCount):
                result.ActorPositionRecords.append(ActorPositionRecord(b.readFloat(), b.readFloat(), b.readFloat()))
            exposureCount = b.readInt32()
            for i in range(0, exposureCount):
                result.ActorExposureRecords.append(ActorExposureRecord(b.readInt32(), b.readInt32(), b.readFloat(), b.readFloat()))
            return result


class BinaryStream:
    """Helper methods for reading values out of the log file at different precisions."""
    def __init__(self, base_stream):
        self.base_stream = base_stream

    # def readByte(self):
    #     return self.base_stream.read(1)

    def readBytes(self, length):
        return self.base_stream.read(length)

    # def readChar(self):
    #     return self.unpack('b')

    # def readUChar(self):
    #     return self.unpack('B')

    # def readBool(self):
    #     return self.unpack('?')

    # def readInt16(self):
    #     return self.unpack('h', 2)

    # def readUInt16(self):
    #     return self.unpack('H', 2)

    def readInt32(self):
        return self.unpack('i', 4)

    # def readUInt32(self):
    #     return self.unpack('I', 4)

    # def readInt64(self):
    #     return self.unpack('q', 8)

    def readUInt64(self):
        return self.unpack('Q', 8)

    def readFloat(self):
        return self.unpack('f', 4)

    # def readDouble(self):
    #     return self.unpack('d', 8)

    def readString(self):
        length = self.LEB128()
        return self.unpack(str(length) + 's', length)

    def LEB128(self):
        result = 0
        shift = 0
        size = 0
        while True:
            b = ord(self.base_stream.read(1))
            size += 1
            result |= (b & 0x7f) << shift
            if b & 0x80 == 0:
                break
            shift += 7
        return result

    # def readStringLength(self):
    #     byte = 0x80
    #     byteArr = []
    #     result = 0
    #     while byte & 0x80:
    #         byte = ord(self.base_stream.read(1))
    #         byteArr.append(byte)
    #     for i in range(0, len(byteArr)):
    #         result |= (byteArr[i] & 0x7F) << (len(byteArr) - 1 - i) * 7
    #     return result

    def unpack(self, fmt, length=1):
        return struct.unpack(fmt, self.readBytes(length))[0]



@attr.s
class PlatformRecord:
    actorID = attr.ib()
    name = attr.ib()
    guid = attr.ib()

@attr.s
class ModeRecord:
    name = attr.ib()
    actorID = attr.ib()
    guid = attr.ib()
    platformGuid = attr.ib()

@attr.s
class SpeciesRecord:
    animatCount = attr.ib()
    startActorID = attr.ib()
    name = attr.ib()
    guid = attr.ib()

@attr.s
class ActorPositionRecord:
    latitude = attr.ib(validator=attr.validators.instance_of(float))
    longitude = attr.ib(validator=attr.validators.instance_of(float))
    depth = attr.ib(validator=attr.validators.instance_of(float))

@attr.s
class ActorExposureRecord:
    actor_id = attr.ib(validator=attr.validators.instance_of(int))
    mode_id= attr.ib(validator=attr.validators.instance_of(int))
    peak_SPL = attr.ib(validator=attr.validators.instance_of(float))
    energy = attr.ib(validator=attr.validators.instance_of(float))

@attr.s
class TimeStepRecord:
    start_time = attr.ib()
    actor_count = attr.ib()
    actor_position_records = attr.ib(default=None)
    actor_exposure_records = attr.ib(default=None)