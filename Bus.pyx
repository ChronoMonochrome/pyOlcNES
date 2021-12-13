# distutils: language = c++
# cython: language_level=3
# cython: profile=True

from typing import *
from pyOlcNES import Py6502, Py2C02, Cartridge

from libc.string cimport memset

uint32_t = uint16_t = uint8_t = int

cdef class Bus:
    cdef public object cpu
    cdef public object ppu
    cdef public object cart
    #cdef public unsigned char cpuRam[2048]
    cdef public object cpuRam
    cdef public unsigned int nSystemClockCounter

    def __init__(self):
        self.cpu = Py6502()
        self.ppu = Py2C02()
        self.cpu.connectBus(self)
        self.cart = None
        self.cpuRam = [0] * 2048
        #memset(self.cpuRam, 0, 2048)

    cpdef public void cpuWrite(self, unsigned int addr, unsigned char data):
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

    cpdef public unsigned char cpuRead(self, unsigned int addr, unsigned char bReadOnly):
        cdef unsigned char data = 0
        cdef unsigned char res = 0
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
    
cpdef public void emulate_frame(object bus):
    # Clocking. The heart and soul of an emulator. The running
    # frequency is controlled by whatever calls this function.
    # So here we "divide" the clock as necessary and call
    # the peripheral devices clock() function at the correct
    # times.

    # The fastest clock frequency the digital system cares
    # about is equivalent to the PPU clock. So the PPU is clocked
    # each time this function is called.

    # Advance renderer - it never stops, it's relentless
    cdef unsigned int scanline = 0
    cdef unsigned int cycle = 0
    cdef unsigned int cpu_cycles = 0
    
    #shuffle(scanline_contents)
    for scanline in range(0, 262):
        #bus.ppu.scanline = scanline
        #bus.ppu.cycle = 0
        for cycle in range(0, 342):
            #bus.ppu.cycle = cycle
            #bus.ppu.sprScreen.SetPixel(cycle, scanline, bus.ppu.palScreen[scanline_contents[(scanline * 341 + cycle) % 89000]])
            if (cpu_cycles == 0):
                cpu_cycles = bus.cpu.clock_instruction()

            if (cycle // cpu_cycles == 3):
                cpu_cycles = bus.cpu.clock_instruction()
            #bus.nSystemClockCounter+=1
    bus.ppu.scanline = -1
    bus.ppu.frame_complete = True