from typing import *

cdef class Mapper:
    cdef public unsigned int nPRGBanks
    cdef public unsigned int nCHRBanks

    def __init__(self, nPRGBanks, nCHRBanks):
        self.nPRGBanks = nPRGBanks
        self.nCHRBanks = nCHRBanks

    cpdef public unsigned int cpuMapRead(self, unsigned int addr):
        pass

    cpdef public unsigned int cpuMapWrite(self, unsigned int addr):
        pass

    cpdef public unsigned int ppuMapRead(self, unsigned int addr):
        pass

    cpdef public unsigned int ppuMapWrite(self, unsigned int addr):
        pass

cdef class Mapper_000:
    cdef public unsigned int nPRGBanks
    cdef public unsigned int nCHRBanks

    def __init__(self, nPRGBanks, nCHRBanks):
        self.nPRGBanks = nPRGBanks
        self.nCHRBanks = nCHRBanks

    cpdef public unsigned int cpuMapRead(self, unsigned int addr):
        # if PRGROM is 16KB
        #     CPU Address Bus          PRG ROM
        #     0x8000 -> 0xBFFF: Map    0x0000 -> 0x3FFF
        #     0xC000 -> 0xFFFF: Mirror 0x0000 -> 0x3FFF
        # if PRGROM is 32KB
        #     CPU Address Bus          PRG ROM
        #     0x8000 -> 0xFFFF: Map    0x0000 -> 0x7FFF
        if (addr >= 0x8000 and addr <= 0xFFFF):
            mapped_addr = addr & (0x7FFF if self.nPRGBanks > 1 else 0x3FFF)
            return mapped_addr

        return 0xdeadbeef

    cpdef public unsigned int cpuMapWrite(self, unsigned int addr):
        if (addr >= 0x8000 and addr <= 0xFFFF):
            mapped_addr = addr & (0x7FFF if self.nPRGBanks > 1 else 0x3FFF)
            return mapped_addr

        return 0xdeadbeef

    cpdef public unsigned int ppuMapRead(self, unsigned int addr):
        # There is no mapping required for PPU
        # PPU Address Bus          CHR ROM
        # 0x0000 -> 0x1FFF: Map    0x0000 -> 0x1FFF
        if (addr >= 0x0000 and addr <= 0x1FFF):
            mapped_addr = addr
            return mapped_addr

        return 0xdeadbeef

    cpdef public unsigned int ppuMapWrite(self, unsigned int addr):
        if (addr >= 0x0000 and addr <= 0x1FFF):
            if (self.nCHRBanks == 0):
                # Treat as RAM
                mapped_addr = addr
                return (True, mapped_addr)

        return 0xdeadbeef