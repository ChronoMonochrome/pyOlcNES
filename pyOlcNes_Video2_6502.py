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
"""

import time
import olc
from pyOlcNES import Bus, Cartridge, emulate_frame
from Py6502 import *
from itertools import dropwhile

class Demo_olc6502(olc.PixelGameEngine):
    def __init__(self):
        olc.PixelGameEngine.__init__(self)
        self.sAppName = "olc6502 Demonstration"
        self.cart = None
        self.bEmulationRun = True
        self.fResidualTime = 0.0
        
        self.updateCalls = 0
        self.t1 = 0
        self._w = 0
        self._h = 0

    def DrawRam(self, x, y, nAddr, nRows, nColumns):
        nRamX = x
        nRamY = y
        for row in range(nRows):
            sOffset = "$" + hex(nAddr)[2:] + ":"
            for col in range(nColumns):
                sOffset += " " + hex(self.nes.cpuRead(nAddr, True))
                nAddr += 1

            self.DrawString(nRamX, nRamY, sOffset)
            nRamY += 10

    def DrawCpu(self, x, y):
        status = "STATUS: "
        self.DrawString(x , y , "STATUS:", olc.Pixel.WHITE)
        self.DrawString(x  + 64, y, "N", olc.Pixel.GREEN if self.nes.cpu.status & FLAGS6502_N else olc.Pixel.RED)
        self.DrawString(x  + 80, y , "V", olc.Pixel.GREEN if self.nes.cpu.status & FLAGS6502_V else olc.Pixel.RED)
        self.DrawString(x  + 96, y , "-", olc.Pixel.GREEN if self.nes.cpu.status & FLAGS6502_U else olc.Pixel.RED)
        self.DrawString(x  + 112, y , "B", olc.Pixel.GREEN if self.nes.cpu.status & FLAGS6502_B else olc.Pixel.RED)
        self.DrawString(x  + 128, y , "D", olc.Pixel.GREEN if self.nes.cpu.status & FLAGS6502_D else olc.Pixel.RED)
        self.DrawString(x  + 144, y , "I", olc.Pixel.GREEN if self.nes.cpu.status & FLAGS6502_I else olc.Pixel.RED)
        self.DrawString(x  + 160, y , "Z", olc.Pixel.GREEN if self.nes.cpu.status & FLAGS6502_Z else olc.Pixel.RED)
        self.DrawString(x  + 178, y , "C", olc.Pixel.GREEN if self.nes.cpu.status & FLAGS6502_C else olc.Pixel.RED)
        self.DrawString(x , y + 10, "PC: $" + hex(self.nes.cpu.pc))
        self.DrawString(x , y + 20, "A: $" +  hex(self.nes.cpu.a) + "  [" + str(self.nes.cpu.a) + "]")
        self.DrawString(x , y + 30, "X: $" +  hex(self.nes.cpu.x) + "  [" + str(self.nes.cpu.x) + "]")
        self.DrawString(x , y + 40, "Y: $" +  hex(self.nes.cpu.y) + "  [" + str(self.nes.cpu.y) + "]")
        self.DrawString(x , y + 50, "Stack P: $" + hex(self.nes.cpu.stkp))

    def DrawCode(self, x, y, nLines):
        it_a = (_ for _ in dropwhile(lambda addr: addr != self.nes.cpu.pc, self.mapAsm))
        nLineY = (nLines >> 1) * 10 + y
        line = ""
        try:
            addr = it_a.__next__()
            line = self.mapAsm[addr]
        except StopIteration:
            pass

        if line:
            self.DrawString(x, nLineY, line, olc.Pixel.CYAN)
            line = ""
            while (nLineY < (nLines * 10) + y):
                nLineY += 10
                try:
                    addr = it_a.__next__()
                    line = self.mapAsm[addr]
                except StopIteration:
                    pass

                if line:
                    self.DrawString(x, nLineY, line)

        reversed_dict = dict(zip(list(self.mapAsm.keys())[::-1], list(self.mapAsm.values())[::-1]))
        it_a = (_ for _ in dropwhile(lambda addr: addr != self.nes.cpu.pc, reversed_dict))
        nLineY = (nLines >> 1) * 10 + y
        line = ""
        try:
            addr = it_a.__next__()
            line = self.mapAsm[addr]
        except StopIteration:
            pass
        if line:
            while (nLineY > y):
                nLineY -= 10
                line = ""
                try:
                    addr = it_a.__next__()
                    line = self.mapAsm[addr]
                except StopIteration:
                    pass
                if line:
                    self.DrawString(x, nLineY, line)

    def OnUserCreate(self):
        self._w = self.ScreenWidth()
        self._h = self.ScreenHeight()

        self.nes = Bus()
        self.cart = Cartridge(r"C:\Users\chrono\Downloads\nestest.nes")
        if (not self.cart.bImageValid):
            return False

        self.nes.insertCartridge(self.cart)
        self.mapAsm = self.nes.cpu.disassemble(0x0000, 0x1FFF)

        self.nes.reset()
        return True

    def OnUserUpdate(self, fElapsedTime):
        print("call number: %d" % self.updateCalls)
        self.updateCalls += 1
        self.Clear(olc.Pixel.DARK_BLUE)

        if (self.bEmulationRun):
            #self.t1 = time.time()
            #while True:
            #   self.nes.clock()
            #   if self.nes.ppu.frame_complete:
            #       break
            #self.nes.ppu.frame_complete = False
            #self.t1 = time.time() - self.t1
            #print("time to draw a frame: %f" % self.t1)
            if (self.fResidualTime > 0.0):
               self.fResidualTime -= fElapsedTime
            else:
               self.fResidualTime += (1.0 / 60.0) - fElapsedTime
               emulate_frame(self.nes)
               self.nes.ppu.frame_complete = False
        else:
            # Emulate code step-by-step
            if (self.GetKey(olc.C).bPressed):
                # Clock enough times to execute a whole CPU instruction
                while True:
                    self.nes.clock()
                    if self.nes.cpu.complete():
                        break
                # CPU clock runs slower than system clock, so it may be
                # complete for additional system clock cycles. Drain
                # those out
                while True:
                    self.nes.clock()
                    if not self.nes.cpu.complete():
                        break

            # Emulate one whole frame
            if (self.GetKey(olc.F).bPressed):
                # Clock enough times to draw a single frame
                while True:
                    self.nes.clock()
                    if self.nes.ppu.frame_complete:
                        break
                # Use residual clock cycles to complete current instruction
                while True:
                    self.nes.clock()
                    if self.nes.cpu.complete():
                        break
                # Reset frame completion flag
                self.nes.ppu.frame_complete = False

        if (self.GetKey(olc.SPACE).bPressed):
            self.bEmulationRun = not self.bEmulationRun
            print("emulationRun: %d" % self.bEmulationRun)
        if (self.GetKey(olc.R).bPressed):
            self.nes.reset()

        self.DrawCpu(516, 2)
        self.DrawCode(516, 72, 26)
        self.DrawSprite(0, 0, self.nes.ppu.getScreen())

        return True

    @staticmethod
    def run():
        demo = Demo_olc6502()
        if demo.Construct(780, 480, 2, 2):
            started = demo.Start()


if __name__ == '__main__':
    Demo_olc6502.run()
