# distutils: language = c++
# cython: language_level=3

"""
    Copyright 2018-2019 OneLoneCoder.com
    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:
    1. Redistributions or derivations of source code must retain the above
    copyright notice, this list of conditions and the following disclaimer.
    2. Redistributions or derivative works in binary form must reproduce
    the above copyright notice. This list of conditions and the following
    disclaimer must be reproduced in the documentation and/or other
    materials provided with the distribution.
    3. Neither the name of the copyright holder nor the names of its
    contributors may be used to endorse or promote products derived
    from this software without specific prior written permission.
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES LOSS OF USE,
    DATA, OR PROFITS OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
    David Barr, aka javidx9, ©OneLoneCoder 2019
"""

import io
import struct
import olc

from abc import ABC, abstractmethod
from typing import *
from enum import IntEnum, Enum, auto
from dataclasses import dataclass, field
from random import randint

uint8_t = uint16_t = uint32_t = int16_t = int

DEBUG = 0
VERBOSE_DEBUG = 0

class FLAGS6502(IntEnum):
    C = (1 << 0) # Carry bit
    Z = (1 << 1) # Zero
    I = (1 << 2) # Disable Interrupts
    D = (1 << 3) # Decimal mode
    B = (1 << 4) # Break
    U = (1 << 5) # Unused
    V = (1 << 6) # Overflow
    N = (1 << 7) # Negative

import sys
sys.path.append(r"D:\dev\python\nes\tests")

from Py6502 import *

class Mapper(ABC):
    nPRGBanks: uint8_t = 0
    nCHRBanks: uint8_t = 0

    def __init__(self, nPRGBanks, nCHRBanks):
        self.nPRGBanks = nPRGBanks
        self.nCHRBanks = nCHRBanks

    @abstractmethod
    def cpuMapRead(self, addr: uint16_t):
        pass

    @abstractmethod
    def cpuMapWrite(self, addr: uint16_t):
        pass

    @abstractmethod
    def ppuMapRead(self, addr: uint16_t):
        pass

    @abstractmethod
    def ppuMapWrite(self, addr: uint16_t):
        pass

class Mapper_000(Mapper):
    def __init__(self, nPRGBanks, nCHRBanks):
        Mapper.__init__(self, nPRGBanks, nCHRBanks)

    def cpuMapRead(self, addr: uint16_t) -> Tuple[bool, uint32_t]:
        # if PRGROM is 16KB
        #     CPU Address Bus          PRG ROM
        #     0x8000 -> 0xBFFF: Map    0x0000 -> 0x3FFF
        #     0xC000 -> 0xFFFF: Mirror 0x0000 -> 0x3FFF
        # if PRGROM is 32KB
        #     CPU Address Bus          PRG ROM
        #     0x8000 -> 0xFFFF: Map    0x0000 -> 0x7FFF
        if (addr >= 0x8000 and addr <= 0xFFFF):
            mapped_addr = addr & (0x7FFF if self.nPRGBanks > 1 else 0x3FFF)
            return (True, mapped_addr)

        return (False, 0)

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

class MIRROR(IntEnum):
    HORIZONTAL = auto()
    VERTICAL = auto()
    ONESCREEN_LO = auto()
    ONESCREEN_HI = auto()

class Cartridge:
    bImageValid: bool

    nMapperID: uint8_t
    nPRGBanks: uint8_t
    nCHRBanks: uint8_t

    vPRGMemory: bytes
    vCHRMemory: bytes

    pMapper: Mapper
    mirror: MIRROR = MIRROR.HORIZONTAL

    def __init__(self, sFileName):
        self.bImageValid = False
        self.nMapperID = 0
        self.nPRGBanks = 0
        self.nCHRBanks = 0

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
            self.mirror = MIRROR.VERTICAL if (header.mapper1 & 0x01) else MIRROR.HORIZONTAL

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

    def cpuRead(self, addr: uint16_t, data: uint8_t) -> Tuple[bool, bytes]:
        res, mapped_addr = self.pMapper.cpuMapRead(addr)
        if (res):
            return (True, self.vPRGMemory[mapped_addr])
        else:
            return (False, bytes())

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


class Py2C02:
    cart: Cartridge
    tblName: List[List[uint8_t]]
    tblPattern: List[List[uint8_t]]
    tblPalette: List[uint8_t]
    palScreen: List[olc.Pixel]
    sprScreen: olc.Sprite
    sprNameTable: List[olc.Sprite]
    sprPatternTable: List[olc.Sprite]
    frame_complete: bool
    scanline: int16_t
    cycle: int16_t

    def __init__(self):
        self.tblName = [[0] * 1024] * 2
        self.tblPattern = [[0] * 4096] * 2
        self.tblPalette = [0] * 32
        palScreen = [None] * 0x40
        palScreen[0x00] = olc.Pixel(84, 84, 84)
        palScreen[0x01] = olc.Pixel(0, 30, 116)
        palScreen[0x02] = olc.Pixel(8, 16, 144)
        palScreen[0x03] = olc.Pixel(48, 0, 136)
        palScreen[0x04] = olc.Pixel(68, 0, 100)
        palScreen[0x05] = olc.Pixel(92, 0, 48)
        palScreen[0x06] = olc.Pixel(84, 4, 0)
        palScreen[0x07] = olc.Pixel(60, 24, 0)
        palScreen[0x08] = olc.Pixel(32, 42, 0)
        palScreen[0x09] = olc.Pixel(8, 58, 0)
        palScreen[0x0A] = olc.Pixel(0, 64, 0)
        palScreen[0x0B] = olc.Pixel(0, 60, 0)
        palScreen[0x0C] = olc.Pixel(0, 50, 60)
        palScreen[0x0D] = olc.Pixel(0, 0, 0)
        palScreen[0x0E] = olc.Pixel(0, 0, 0)
        palScreen[0x0F] = olc.Pixel(0, 0, 0)

        palScreen[0x10] = olc.Pixel(152, 150, 152)
        palScreen[0x11] = olc.Pixel(8, 76, 196)
        palScreen[0x12] = olc.Pixel(48, 50, 236)
        palScreen[0x13] = olc.Pixel(92, 30, 228)
        palScreen[0x14] = olc.Pixel(136, 20, 176)
        palScreen[0x15] = olc.Pixel(160, 20, 100)
        palScreen[0x16] = olc.Pixel(152, 34, 32)
        palScreen[0x17] = olc.Pixel(120, 60, 0)
        palScreen[0x18] = olc.Pixel(84, 90, 0)
        palScreen[0x19] = olc.Pixel(40, 114, 0)
        palScreen[0x1A] = olc.Pixel(8, 124, 0)
        palScreen[0x1B] = olc.Pixel(0, 118, 40)
        palScreen[0x1C] = olc.Pixel(0, 102, 120)
        palScreen[0x1D] = olc.Pixel(0, 0, 0)
        palScreen[0x1E] = olc.Pixel(0, 0, 0)
        palScreen[0x1F] = olc.Pixel(0, 0, 0)

        palScreen[0x20] = olc.Pixel(236, 238, 236)
        palScreen[0x21] = olc.Pixel(76, 154, 236)
        palScreen[0x22] = olc.Pixel(120, 124, 236)
        palScreen[0x23] = olc.Pixel(176, 98, 236)
        palScreen[0x24] = olc.Pixel(228, 84, 236)
        palScreen[0x25] = olc.Pixel(236, 88, 180)
        palScreen[0x26] = olc.Pixel(236, 106, 100)
        palScreen[0x27] = olc.Pixel(212, 136, 32)
        palScreen[0x28] = olc.Pixel(160, 170, 0)
        palScreen[0x29] = olc.Pixel(116, 196, 0)
        palScreen[0x2A] = olc.Pixel(76, 208, 32)
        palScreen[0x2B] = olc.Pixel(56, 204, 108)
        palScreen[0x2C] = olc.Pixel(56, 180, 204)
        palScreen[0x2D] = olc.Pixel(60, 60, 60)
        palScreen[0x2E] = olc.Pixel(0, 0, 0)
        palScreen[0x2F] = olc.Pixel(0, 0, 0)

        palScreen[0x30] = olc.Pixel(236, 238, 236)
        palScreen[0x31] = olc.Pixel(168, 204, 236)
        palScreen[0x32] = olc.Pixel(188, 188, 236)
        palScreen[0x33] = olc.Pixel(212, 178, 236)
        palScreen[0x34] = olc.Pixel(236, 174, 236)
        palScreen[0x35] = olc.Pixel(236, 174, 212)
        palScreen[0x36] = olc.Pixel(236, 180, 176)
        palScreen[0x37] = olc.Pixel(228, 196, 144)
        palScreen[0x38] = olc.Pixel(204, 210, 120)
        palScreen[0x39] = olc.Pixel(180, 222, 120)
        palScreen[0x3A] = olc.Pixel(168, 226, 144)
        palScreen[0x3B] = olc.Pixel(152, 226, 180)
        palScreen[0x3C] = olc.Pixel(160, 214, 228)
        palScreen[0x3D] = olc.Pixel(160, 162, 160)
        palScreen[0x3E] = olc.Pixel(0, 0, 0)
        palScreen[0x3F] = olc.Pixel(0, 0, 0)

        self.palScreen = palScreen
        self.sprScreen = olc.Sprite(256, 240)
        self.sprNameTable = [olc.Sprite(256, 240), olc.Sprite(256, 240)]
        self.sprPatternTable = [olc.Sprite(128, 128), olc.Sprite(128, 128)]

        self.frame_complete = False
        self.scanline = 0
        self.cycle = 0

    def getScreen(self) -> olc.Sprite:
        return self.sprScreen

    def getNameTable(self, i: uint8_t) -> olc.Sprite:
        return self.sprNameTable[i]

    def getPatternTable(self, i: uint8_t) -> olc.Sprite:
        return self.sprPatternTable[i]

    def connectCartridge(self, cart: Cartridge) -> None:
        self.cart = cart

    def cpuRead(self, addr: uint16_t, rdonly: Optional[bool] = False) -> uint8_t:
        data: uint8_t = 0x00

        if addr == 0x0000: # Control
            pass
        elif addr == 0x0001: # Mask
            pass
        elif addr == 0x0002: # Status
            pass
        elif addr ==  0x0003: # OAM Address
            pass
        elif addr ==  0x0004: # OAM Data
            pass
        elif addr ==  0x0005: # Scroll
            pass
        elif addr ==  0x0006: # PPU Address
            pass
        elif addr ==  0x0007: # PPU Data
            pass

        return data

    def cpuWrite(self, addr: uint16_t, data: uint8_t) -> None:
        if addr == 0x0000: # Control
            pass
        elif addr == 0x0001: # Mask
            pass
        elif addr == 0x0002: # Status
            pass
        elif addr ==  0x0003: # OAM Address
            pass
        elif addr ==  0x0004: # OAM Data
            pass
        elif addr ==  0x0005: # Scroll
            pass
        elif addr ==  0x0006: # PPU Address
            pass
        elif addr ==  0x0007: # PPU Data
            pass

    def ppuRead(self, addr: uint16_t, rdonly: Optional[bool] = False) -> uint8_t:
        data: uint8_t = 0x00
        addr &= 0x3FFF

        res, data = self.cart.ppuRead(addr, data)

        if (res):
            pass

        return data

    def ppuWrite(self, addr: uint16_t, data: uint8_t) -> None:
        addr &= 0x3FFF

        if (self.cart.ppuWrite(addr, data)):
            pass

class Bus:
    cpu: Any = None
    ppu: Py2C02
    cart: Cartridge = None
    cpuRam: List[uint8_t]

    nSystemClockCounter: uint32_t
    def __init__(self):
        self.cpu = Py6502()
        self.ppu = Py2C02()
        self.cpu.connectBus(self)
        self.cpuRam = [0] * (2 * 1024)

    def cpuWrite(self, addr: uint16_t, data: uint8_t) -> None:
        res = False
        if self.cart:
            res = self.cart.cpuWrite(addr, data)

        if (res):
            # The cartridge "sees all" and has the facility to veto
            # the propagation of the bus transaction if it requires.
            # This allows the cartridge to map any address to some
            # other data, including the facility to divert transactions
            # with other physical devices. The NES does not do this
            # but I figured it might be quite a flexible way of adding
            # "custom" hardware to the NES in the future!
            pass
        elif (addr >= 0x0000 and addr < 0x1FFF):
            # System RAM Address Range. The range covers 8KB, though
            # there is only 2KB available. That 2KB is "mirrored"
            # through this address range. Using bitwise AND to mask
            # the bottom 11 bits is the same as addr % 2048.
            self.cpuRam[addr & 0x07FF] = data
        elif (addr >= 0x2000 and addr < 0x3FFF):
            # PPU Address range. The PPU only has 8 primary registers
            # and these are repeated throughout this range. We can
            # use bitwise AND operation to mask the bottom 3 bits,
            # which is the equivalent of addr % 8.
            self.ppu.cpuWrite(addr & 0x0007, data)

    def cpuRead(self, addr: uint16_t, bReadOnly: Optional[bool] = False) -> uint8_t:
        data: uint8_t = 0
        res = False
        if self.cart:
            res, cartData = self.cart.cpuRead(addr, data)

        if (res):
            # Cartridge Address Range
            data = cartData
        elif (addr >= 0x0000 and addr < 0x1FFF):
            # System RAM Address Range, mirrored every 2048
            data = self.cpuRam[addr & 0x07FF]
        elif (addr >= 0x2000 and addr < 0x3FFF):
            # PPU Address range, mirrored every 8
            data = self.ppu.cpuRead(addr & 0x0007, bReadOnly)
        return data

    def insertCartridge(self, cartridge: Cartridge) -> None:
        # Connects cartridge to both Main Bus and CPU Bus
        self.cart = cartridge
        self.ppu.connectCartridge(cartridge)

    def reset(self) -> None:
        self.cpu.reset()
        self.nSystemClockCounter = 0

from random import shuffle   
scanline_contents = [0x30] * (261 * 341 // 2)  + [0x3F] * (261 * 341 // 2)

def emulate_frame(bus):
    scanline = 0
    cycle = 0
    #global scanline_contents
    #shuffle(scanline_contents)
    for scanline in range(0, 262):
        #bus.ppu.scanline = scanline
        #bus.ppu.cycle = 0
        for cycle in range(0, 342):
            #bus.ppu.cycle = cycle
            #bus.ppu.sprScreen.SetPixel(cycle, scanline, bus.ppu.palScreen[scanline_contents[(scanline * 341 + cycle)%89000]])
            if (cycle % 3 == 0):
                bus.cpu.clock()
            #bus.nSystemClockCounter+=1
    bus.ppu.scanline = -1
    bus.ppu.frame_complete = True