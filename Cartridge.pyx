# distutils: language = c++
# cython: language_level=3
# cython: profile=True

import io

from typing import *
from enum import IntEnum, Enum, auto
from pyOlcNES import uint8_t, uint16_t
from Mapper import *

# iNES Format Header
class sHeader:
    name: bytes
    prg_rom_chunks: uint8_t
    chr_rom_chunks: uint8_t
    mapper1: uint8_t
    mapper2: uint8_t
    prg_ram_size: uint8_t
    tv_system1: uint8_t
    tv_system2: uint8_t
    unused: bytes

    def __init__(self, name, prg_rom_chunks, chr_rom_chunks, mapper1, mapper2, prg_ram_size, tv_system1, tv_system2, unused):
        self.name = name
        self.prg_rom_chunks = prg_rom_chunks
        self.chr_rom_chunks = chr_rom_chunks
        self.mapper1 = mapper1
        self.mapper2 = mapper2
        self.prg_ram_size = prg_ram_size
        self.tv_system1 = tv_system1
        self.tv_system2 = tv_system2
        self.unused = unused

    @classmethod
    def unpack(self, fd):
        header = io.BytesIO(fd.read(16))

        name = header.read(4)
        prg_rom_chunks = uint8_t(header.read(1)[0])
        chr_rom_chunks = uint8_t(header.read(1)[0])
        mapper1 = uint8_t(header.read(1)[0])
        mapper2 = uint8_t(header.read(1)[0])
        prg_ram_size = uint8_t(header.read(1)[0])
        tv_system1 = uint8_t(header.read(1)[0])
        tv_system2 = uint8_t(header.read(1)[0])
        unused = header.read(5)
        return sHeader(name, prg_rom_chunks, chr_rom_chunks, mapper1, mapper2, prg_ram_size, tv_system1, tv_system2, unused)


ctypedef public enum MIRROR:
    M_HORIZONTAL = 0,
    M_VERTICAL,
    M_ONESCREEN_LO,
    M_ONESCREEN_HI,

cdef struct ret_result:
    unsigned int res
    unsigned int data

cdef class Cartridge:
    cdef public unsigned int bImageValid

    cdef public unsigned int nMapperID
    cdef public unsigned int nPRGBanks
    cdef public unsigned int nCHRBanks

    cdef public bytes vPRGMemory
    cdef public bytes vCHRMemory

    cdef public object pMapper
    cdef public unsigned int mirror
    cdef public unsigned int nFileType

    def __init__(self, sFileName):
        self.bImageValid = False
        self.nMapperID = 0
        self.nPRGBanks = 0
        self.nCHRBanks = 0
        self.mirror = M_HORIZONTAL

        fd = None
        try:
            fd = open(sFileName, "rb")
        except OSError:
            pass

        if fd:
            # Read file header
            header = sHeader.unpack(fd)

            # If a "trainer" exists we just need to read past
            # it before we get to the good stuff
            if (header.mapper1 & 0x04):
                fd.seek(512)

            # Determine Mapper ID
            self.nMapperID = uint8_t(((header.mapper2 >> 4) << 4) | (header.mapper1 >> 4))
            self.mirror = M_VERTICAL if (header.mapper1 & 0x01) else M_HORIZONTAL

            # "Discover" File Format
            self.nFileType = 1

            if (self.nFileType == 0):
                pass

            if (self.nFileType == 1):
                self.nPRGBanks = header.prg_rom_chunks
                self.vPRGMemory = fd.read(self.nPRGBanks * 16384)

                self.nCHRBanks = header.chr_rom_chunks;
                self.vCHRMemory = fd.read(self.nCHRBanks * 8192)

            if (self.nFileType == 2):
                pass

            # Load appropriate mapper
            if self.nMapperID == 0:
                self.pMapper = Mapper_000(self.nPRGBanks, self.nCHRBanks)
            else:
                raise RuntimeError("Unsupported mapperID %d" % self.nMapperID)

            self.bImageValid = True;
            fd.close()

    def imageValid(self) -> bool:
        return self.bImageValid

    cpdef public ret_result cpuRead(self, unsigned int addr, unsigned int data):
        cdef ret_result ret

        ret = self.pMapper.cpuMapRead(addr)

        if (ret.res > 0):
            ret.data = self.vPRGMemory[ret.data]
        else:
            ret.data = 0

        return ret

    def cpuWrite(self, addr: uint16_t, data: uint8_t) -> bool:
        res, mapped_addr = self.pMapper.cpuMapWrite(addr)
        if (res):
            self.vPRGMemory[mapped_addr] = data
            return True
        else:
            return False

    def ppuRead(self, addr: uint16_t, data: uint8_t) -> Tuple[bool, bytes]:
        res, mapped_addr = self.pMapper.ppuMapRead(addr)
        if (res):
            return (True, self.vCHRMemory[mapped_addr])
        else:
            return (False, bytes())

    def ppuWrite(self, addr: uint16_t, data: uint8_t) -> bool:
        res, mapped_addr = self.pMapper.ppuMapWrite(addr)
        if (res):
            self.vCHRMemory[mapped_addr] = data
            return True
        else:
            return False