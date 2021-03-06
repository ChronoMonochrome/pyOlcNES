# distutils: language = c++
# cython: language_level=3
# cython: profile=True

from typing import *

uint32_t = uint16_t = uint8_t = int

cdef struct ret_result:
    unsigned int res
    unsigned int data

cdef class Mapper:
    cdef public unsigned int nPRGBanks
    cdef public unsigned int nCHRBanks

    def __init__(self, nPRGBanks, nCHRBanks):
        self.nPRGBanks = nPRGBanks
        self.nCHRBanks = nCHRBanks

    def cpuMapRead(self, addr: uint16_t):
        pass

    def cpuMapWrite(self, addr: uint16_t):
        pass

    def ppuMapRead(self, addr: uint16_t):
        pass

    def ppuMapWrite(self, addr: uint16_t):
        pass

cdef class Mapper_000:
    cdef unsigned int nPRGBanks
    cdef unsigned int nCHRBanks

    def __init__(self, nPRGBanks, nCHRBanks):
        self.nPRGBanks = nPRGBanks
        self.nCHRBanks = nCHRBanks

    cpdef public ret_result cpuMapRead(self, unsigned int addr):
        cdef ret_result ret
        # if PRGROM is 16KB
        #     CPU Address Bus          PRG ROM
        #     0x8000 -> 0xBFFF: Map    0x0000 -> 0x3FFF
        #     0xC000 -> 0xFFFF: Mirror 0x0000 -> 0x3FFF
        # if PRGROM is 32KB
        #     CPU Address Bus          PRG ROM
        #     0x8000 -> 0xFFFF: Map    0x0000 -> 0x7FFF
        if (addr >= 0x8000 and addr <= 0xFFFF):
            ret.res = 1
            #ret.data = addr & 0x3FFF
            ret.data = addr & (0x7FFF if self.nPRGBanks > 1 else 0x3FFF)
            return ret

        ret.res = 0
        ret.data = 0
        return ret

    def cpuMapWrite(self, addr: uint16_t) -> Tuple[bool, uint32_t]:
        if (addr >= 0x8000 and addr <= 0xFFFF):
            mapped_addr = addr & (0x7FFF if self.nPRGBanks > 1 else 0x3FFF)
            return (True, mapped_addr)

        return (False, 0)

    def ppuMapRead(self, addr: uint16_t) -> Tuple[bool, uint32_t]:
        # There is no mapping required for PPU
        # PPU Address Bus          CHR ROM
        # 0x0000 -> 0x1FFF: Map    0x0000 -> 0x1FFF
        if (addr >= 0x0000 and addr <= 0x1FFF):
            mapped_addr = uint32_t(addr)
            return (True, mapped_addr)

        return (False, 0)

    def ppuMapWrite(self, addr: uint16_t) -> Tuple[bool, uint32_t]:
        if (addr >= 0x0000 and addr <= 0x1FFF):
            if (self.nCHRBanks == 0):
                # Treat as RAM
                mapped_addr = addr
                return (True, mapped_addr)

        return (False, 0)