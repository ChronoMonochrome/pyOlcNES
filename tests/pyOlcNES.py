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

import struct
import olc

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

from Mapper import *

from Cartridge import *


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

from Bus import *

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