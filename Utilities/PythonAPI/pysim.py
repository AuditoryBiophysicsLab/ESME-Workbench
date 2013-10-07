import datetime
from struct import *
import uuid
import os.path


__author__ = 'Graham Voysey'


class PySim(object):
    __magicFooter = long("a57d8ee659dc45ec", 16)
    __magicTimeStepRecord = long("d3c603dd0d7a1ee6", 16)

    def __init__(self):
        self.Filename = None
        self.__trailerOffset = None
        self.TimeStepSize = None
        self.StartTime = None
        self.EndTime = None
        self.CreatingUser = None
        self.CreatingComputer = None
        self.ScenarioRecord = None
        self.PlatformRecords = None
        self.ModeRecords = None
        self.SpeciesRecords = None
        self.TimeStepRecordOffsets = None

    def FromFileName(self, filename):
        if not os.path.exists(filename):
            raise RuntimeError('Simulation file not found')
        self.Filename = filename
        self.__readFooter()
        return self

    def __readFooter(self):
        """
        @rtype : self
        @return : a PySim object containing valid metadata.
        @summary : reads the footer of a simulation.exposures file and populates class attributes appropriately.
        """
        with open(self.Filename, "rb") as l:
            #  preliminary work; read offset and magic number or die.
            b = BinaryStream(l)
            b.base_stream.seek(-16, 2)
            self.__trailerOffset = b.readUInt64()
            magic = long(b.readUInt64())
            if magic != self.__magicFooter:
                raise IOError('magic number not seen at expected location')

            #  read payload of the footer
            b.base_stream.seek(self.__trailerOffset, 0)
            self.TimeStepSize = datetime.timedelta(microseconds=b.readUInt64() / 10)
            self.StartTime = datetime.datetime(1, 1, 1) + datetime.timedelta(microseconds=long(b.readUInt64()) / 10)
            self.EndTime = datetime.datetime(1, 1, 1) + datetime.timedelta(microseconds=long(b.readUInt64()) / 10)
            self.CreatingUser = b.readString()
            self.CreatingComputer = b.readString()
            self.ScenarioRecord = b.readString(), uuid.UUID(bytes=b.readBytes(16))

            platformCount = b.readInt32()
            self.PlatformRecords = []
            for i in xrange(0, platformCount):
                self.PlatformRecords.append(PlatformRecord(b.readInt32(), b.readString(), uuid.UUID(bytes=b.readBytes(16))))

            modeCount = b.readInt32()
            self.ModeRecords = []
            for i in xrange(0, modeCount):
                self.ModeRecords.append(ModeRecord(b.readInt32(), b.readString(), uuid.UUID(bytes=b.readBytes(16)), uuid.UUID(bytes=b.readBytes(16))))

            speciesCount = b.readInt32()
            self.SpeciesRecords = []
            for i in xrange(0, speciesCount):
                self.SpeciesRecords.append(SpeciesRecord(b.readInt32(), b.readInt32(), b.readString(), uuid.UUID(bytes=b.readBytes(16))))

            offsetCount = b.readInt32()
            self.TimeStepRecordOffsets = []
            for i in xrange(0, offsetCount):
                self.TimeStepRecordOffsets.append(b.readUInt64())

    def ReadTimeStepRecord(self, offset):
        """

        @param offset: One element of a PySim.TimeStepRecordOffset list.
        @return: A TimeStepRecord object, containing the starting time of this collection of exposures relative to simulation start,
                the number of actors logged in this time step, a list of actor positions (latitude, longitude, depth), and a list of actor exposure records.
        """
        with open(self.Filename, "rb") as l:
            b = BinaryStream(l)
            # jump to the beginning and read the header.
            b.base_stream.seek(offset, 0)
            if long(b.readUInt64()) != self.__magicTimeStepRecord:
                raise IOError('magic number not seen at expected location')
            result = TimeStepRecord(datetime.datetime(1, 1, 1) + datetime.timedelta(microseconds=long(b.readUInt64()) / 10), b.readInt32())
            for i in xrange(0, result.ActorCount):
                result.ActorPositionRecords.append(ActorPositionRecord(b.readFloat(), b.readFloat(), b.readFloat()))
            exposureCount = b.readInt32()
            for i in xrange(0, exposureCount):
                result.ActorExposureRecords.append(ActorExposureRecord(b.readInt32(), b.readInt32(), b.readFloat(), b.readFloat()))
            return result

    def GetTypeRecord(self, actorID):
        pass

    def GetAssociatedModes(self, platformID):
        pass


class PlatformRecord(object):
    def __init__(self, actorID, name, guid):
        self.actorID = actorID
        self.name = name
        self.guid = guid


class ModeRecord(object):
    def __init__(self, actorID, name, guid, platformGuid):
        self.name = name
        self.actorID = actorID
        self.guid = guid
        self.platformGuid = platformGuid


class SpeciesRecord(object):
    def __init__(self, animatCount, startID, name, guid):
        self.animatCount = animatCount
        self.startActorID = startID
        self.name = name
        self.guid = guid


class BinaryStream:
    def __init__(self, base_stream):
        self.base_stream = base_stream

    def readByte(self):
        return self.base_stream.read(1)

    def readBytes(self, length):
        return self.base_stream.read(length)

    def readChar(self):
        return self.unpack('b')

    def readUChar(self):
        return self.unpack('B')

    def readBool(self):
        return self.unpack('?')

    def readInt16(self):
        return self.unpack('h', 2)

    def readUInt16(self):
        return self.unpack('H', 2)

    def readInt32(self):
        return self.unpack('i', 4)

    def readUInt32(self):
        return self.unpack('I', 4)

    def readInt64(self):
        return self.unpack('q', 8)

    def readUInt64(self):
        return self.unpack('Q', 8)

    def readFloat(self):
        return self.unpack('f', 4)

    def readDouble(self):
        return self.unpack('d', 8)

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

    def readStringLength(self):
        byte = 0x80
        byteArr = []
        result = 0
        while byte & 0x80:
            byte = ord(self.base_stream.read(1))
            byteArr.append(byte)
        for i in xrange(0, len(byteArr)):
            result |= (byteArr[i] & 0x7F) << (len(byteArr) - 1 - i) * 7
        return result

    def writeBytes(self, value):
        self.base_stream.write(value)

    def writeChar(self, value):
        self.pack('c', value)

    def writeUChar(self, value):
        self.pack('C', value)

    def writeBool(self, value):
        self.pack('?', value)

    def writeInt16(self, value):
        self.pack('h', value)

    def writeUInt16(self, value):
        self.pack('H', value)

    def writeInt32(self, value):
        self.pack('i', value)

    def writeUInt32(self, value):
        self.pack('I', value)

    def writeInt64(self, value):
        self.pack('q', value)

    def writeUInt64(self, value):
        self.pack('Q', value)

    def writeFloat(self, value):
        self.pack('f', value)

    def writeDouble(self, value):
        self.pack('d', value)

    #
    # def writeString(self, value):
    #     length = len(value)
    #     self.writeUInt16(length)
    #     self.pack(str(length) + 's', value)
    #

    def pack(self, fmt, data):
        return self.writeBytes(pack(fmt, data))

    def unpack(self, fmt, length=1):
        return unpack(fmt, self.readBytes(length))[0]


class ActorPositionRecord(object):
    def __init__(self, lat, lon, depth):
        self.Latitude = lat
        self.Longitude = lon
        self.Depth = depth


class ActorExposureRecord(object):
    def __init__(self, actorID, modeID, peak, energy):
        self.ActorID = actorID
        self.ModeID = modeID
        self.PeakSPL = peak
        self.Energy = energy


class TimeStepRecord(object):
    def __init__(self, start, count):
        self.StartTime = start
        self.ActorCount = count
        self.ActorPositionRecords = []
        self.ActorExposureRecords = []