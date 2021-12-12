from typing import *
from pyOlcNES import Py6502, Py2C02, Cartridge

uint32_t = uint16_t = uint8_t = int

class Bus:
    def __init__(self):
        self.cpu = Py6502()
        self.ppu = Py2C02()
        self.cpu.connectBus(self)
        self.cart = None
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