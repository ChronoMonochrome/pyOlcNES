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

class INSTRUCTION:
    name: str
    operate: Callable
    addrmode: Callable
    cycles: uint8_t

    def __init__(self, name, operate, addrmode, cycles):
        self.name = name
        self.operate = operate
        self.addrmode = addrmode
        self.cycles = cycles

class Py6502:
    flags: FLAGS6502

    # CPU Core registers, exposed as public here for ease of access from external
    # examinors. This is all the 6502 has.
    a: uint8_t      # Accumulator Register
    x: uint8_t      # X Register
    y: uint8_t      # Y Register
    stkp: uint8_t   # Stack Pointer (points to location on bus)
    pc: uint16_t    # Program Counter
    status: uint8_t # Status Register

    # Assisstive variables to facilitate emulation
    fetched: uint8_t        # Represents the working input value to the ALU
    temp: uint16_t          # A convenience variable used everywhere
    addr_abs: uint16_t      # All used memory addresses end up in here
    addr_rel: uint16_t      # Represents absolute address following a branch
    opcode: uint8_t         # Is the instruction byte
    cycles: uint8_t         # Counts how many cycles the instruction has remaining
    clock_count: uint32_t   # A global accumulation of the number of clocks
    lookup: List[INSTRUCTION]

    def __init__(self):
        self.a = 0
        self.x = 0
        self.y = 0
        self.stkp = 0
        self.pc = uint16_t(0)
        self.status = 0
        self.fetched = 0
        self.temp = uint16_t(0)
        self.addr_abs = uint16_t(0)
        self.addr_rel = uint16_t(0)
        self.opcode = 0
        self.cycles = 0
        self.clock_count = 0

        s = self
        I = INSTRUCTION

        # Assembles the translation table. It's big, it's ugly, but it yields a convenient way
        # to emulate the 6502. I'm certain there are some "code-golf" strategies to reduce this
        # but I've deliberately kept it verbose for study and alteration

        # It is 16x16 entries. This gives 256 instructions. It is arranged to that the bottom
        # 4 bits of the instruction choose the column, and the top 4 bits choose the row.

        # For convenience to get function pointers to members of this class, I'm using this
        # or else it will be much much larger :D


        s.lookup = [
            I( "BRK", s.BRK, s.IMM, 7 ),I(  "ORA", s.ORA, s.IZX, 6 ),I(  "???", s.XXX, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 8 ),I(  "???", s.NOP, s.IMP, 3 ),I(  "ORA", s.ORA, s.ZP0, 3 ),I(  "ASL", s.ASL, s.ZP0, 5 ),I(  "???", s.XXX, s.IMP, 5 ),I(  "PHP", s.PHP, s.IMP, 3 ),I(  "ORA", s.ORA, s.IMM, 2 ),I(  "ASL", s.ASL, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 2 ),I(  "???", s.NOP, s.IMP, 4 ),I(  "ORA", s.ORA, s.ABS, 4 ),I(  "ASL", s.ASL, s.ABS, 6 ),I(  "???", s.XXX, s.IMP, 6 ),
            I( "BPL", s.BPL, s.REL, 2 ),I(  "ORA", s.ORA, s.IZY, 5 ),I(  "???", s.XXX, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 8 ),I(  "???", s.NOP, s.IMP, 4 ),I(  "ORA", s.ORA, s.ZPX, 4 ),I(  "ASL", s.ASL, s.ZPX, 6 ),I(  "???", s.XXX, s.IMP, 6 ),I(  "CLC", s.CLC, s.IMP, 2 ),I(  "ORA", s.ORA, s.ABY, 4 ),I(  "???", s.NOP, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 7 ),I(  "???", s.NOP, s.IMP, 4 ),I(  "ORA", s.ORA, s.ABX, 4 ),I(  "ASL", s.ASL, s.ABX, 7 ),I(  "???", s.XXX, s.IMP, 7 ),
            I( "JSR", s.JSR, s.ABS, 6 ),I(  "AND", s.AND, s.IZX, 6 ),I(  "???", s.XXX, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 8 ),I(  "BIT", s.BIT, s.ZP0, 3 ),I(  "AND", s.AND, s.ZP0, 3 ),I(  "ROL", s.ROL, s.ZP0, 5 ),I(  "???", s.XXX, s.IMP, 5 ),I(  "PLP", s.PLP, s.IMP, 4 ),I(  "AND", s.AND, s.IMM, 2 ),I(  "ROL", s.ROL, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 2 ),I(  "BIT", s.BIT, s.ABS, 4 ),I(  "AND", s.AND, s.ABS, 4 ),I(  "ROL", s.ROL, s.ABS, 6 ),I(  "???", s.XXX, s.IMP, 6 ),
            I( "BMI", s.BMI, s.REL, 2 ),I(  "AND", s.AND, s.IZY, 5 ),I(  "???", s.XXX, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 8 ),I(  "???", s.NOP, s.IMP, 4 ),I(  "AND", s.AND, s.ZPX, 4 ),I(  "ROL", s.ROL, s.ZPX, 6 ),I(  "???", s.XXX, s.IMP, 6 ),I(  "SEC", s.SEC, s.IMP, 2 ),I(  "AND", s.AND, s.ABY, 4 ),I(  "???", s.NOP, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 7 ),I(  "???", s.NOP, s.IMP, 4 ),I(  "AND", s.AND, s.ABX, 4 ),I(  "ROL", s.ROL, s.ABX, 7 ),I(  "???", s.XXX, s.IMP, 7 ),
            I( "RTI", s.RTI, s.IMP, 6 ),I(  "EOR", s.EOR, s.IZX, 6 ),I(  "???", s.XXX, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 8 ),I(  "???", s.NOP, s.IMP, 3 ),I(  "EOR", s.EOR, s.ZP0, 3 ),I(  "LSR", s.LSR, s.ZP0, 5 ),I(  "???", s.XXX, s.IMP, 5 ),I(  "PHA", s.PHA, s.IMP, 3 ),I(  "EOR", s.EOR, s.IMM, 2 ),I(  "LSR", s.LSR, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 2 ),I(  "JMP", s.JMP, s.ABS, 3 ),I(  "EOR", s.EOR, s.ABS, 4 ),I(  "LSR", s.LSR, s.ABS, 6 ),I(  "???", s.XXX, s.IMP, 6 ),
            I( "BVC", s.BVC, s.REL, 2 ),I(  "EOR", s.EOR, s.IZY, 5 ),I(  "???", s.XXX, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 8 ),I(  "???", s.NOP, s.IMP, 4 ),I(  "EOR", s.EOR, s.ZPX, 4 ),I(  "LSR", s.LSR, s.ZPX, 6 ),I(  "???", s.XXX, s.IMP, 6 ),I(  "CLI", s.CLI, s.IMP, 2 ),I(  "EOR", s.EOR, s.ABY, 4 ),I(  "???", s.NOP, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 7 ),I(  "???", s.NOP, s.IMP, 4 ),I(  "EOR", s.EOR, s.ABX, 4 ),I(  "LSR", s.LSR, s.ABX, 7 ),I(  "???", s.XXX, s.IMP, 7 ),
            I( "RTS", s.RTS, s.IMP, 6 ),I(  "ADC", s.ADC, s.IZX, 6 ),I(  "???", s.XXX, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 8 ),I(  "???", s.NOP, s.IMP, 3 ),I(  "ADC", s.ADC, s.ZP0, 3 ),I(  "ROR", s.ROR, s.ZP0, 5 ),I(  "???", s.XXX, s.IMP, 5 ),I(  "PLA", s.PLA, s.IMP, 4 ),I(  "ADC", s.ADC, s.IMM, 2 ),I(  "ROR", s.ROR, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 2 ),I(  "JMP", s.JMP, s.IND, 5 ),I(  "ADC", s.ADC, s.ABS, 4 ),I(  "ROR", s.ROR, s.ABS, 6 ),I(  "???", s.XXX, s.IMP, 6 ),
            I( "BVS", s.BVS, s.REL, 2 ),I(  "ADC", s.ADC, s.IZY, 5 ),I(  "???", s.XXX, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 8 ),I(  "???", s.NOP, s.IMP, 4 ),I(  "ADC", s.ADC, s.ZPX, 4 ),I(  "ROR", s.ROR, s.ZPX, 6 ),I(  "???", s.XXX, s.IMP, 6 ),I(  "SEI", s.SEI, s.IMP, 2 ),I(  "ADC", s.ADC, s.ABY, 4 ),I(  "???", s.NOP, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 7 ),I(  "???", s.NOP, s.IMP, 4 ),I(  "ADC", s.ADC, s.ABX, 4 ),I(  "ROR", s.ROR, s.ABX, 7 ),I(  "???", s.XXX, s.IMP, 7 ),
            I( "???", s.NOP, s.IMP, 2 ),I(  "STA", s.STA, s.IZX, 6 ),I(  "???", s.NOP, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 6 ),I(  "STY", s.STY, s.ZP0, 3 ),I(  "STA", s.STA, s.ZP0, 3 ),I(  "STX", s.STX, s.ZP0, 3 ),I(  "???", s.XXX, s.IMP, 3 ),I(  "DEY", s.DEY, s.IMP, 2 ),I(  "???", s.NOP, s.IMP, 2 ),I(  "TXA", s.TXA, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 2 ),I(  "STY", s.STY, s.ABS, 4 ),I(  "STA", s.STA, s.ABS, 4 ),I(  "STX", s.STX, s.ABS, 4 ),I(  "???", s.XXX, s.IMP, 4 ),
            I( "BCC", s.BCC, s.REL, 2 ),I(  "STA", s.STA, s.IZY, 6 ),I(  "???", s.XXX, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 6 ),I(  "STY", s.STY, s.ZPX, 4 ),I(  "STA", s.STA, s.ZPX, 4 ),I(  "STX", s.STX, s.ZPY, 4 ),I(  "???", s.XXX, s.IMP, 4 ),I(  "TYA", s.TYA, s.IMP, 2 ),I(  "STA", s.STA, s.ABY, 5 ),I(  "TXS", s.TXS, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 5 ),I(  "???", s.NOP, s.IMP, 5 ),I(  "STA", s.STA, s.ABX, 5 ),I(  "???", s.XXX, s.IMP, 5 ),I(  "???", s.XXX, s.IMP, 5 ),
            I( "LDY", s.LDY, s.IMM, 2 ),I(  "LDA", s.LDA, s.IZX, 6 ),I(  "LDX", s.LDX, s.IMM, 2 ),I(  "???", s.XXX, s.IMP, 6 ),I(  "LDY", s.LDY, s.ZP0, 3 ),I(  "LDA", s.LDA, s.ZP0, 3 ),I(  "LDX", s.LDX, s.ZP0, 3 ),I(  "???", s.XXX, s.IMP, 3 ),I(  "TAY", s.TAY, s.IMP, 2 ),I(  "LDA", s.LDA, s.IMM, 2 ),I(  "TAX", s.TAX, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 2 ),I(  "LDY", s.LDY, s.ABS, 4 ),I(  "LDA", s.LDA, s.ABS, 4 ),I(  "LDX", s.LDX, s.ABS, 4 ),I(  "???", s.XXX, s.IMP, 4 ),
            I( "BCS", s.BCS, s.REL, 2 ),I(  "LDA", s.LDA, s.IZY, 5 ),I(  "???", s.XXX, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 5 ),I(  "LDY", s.LDY, s.ZPX, 4 ),I(  "LDA", s.LDA, s.ZPX, 4 ),I(  "LDX", s.LDX, s.ZPY, 4 ),I(  "???", s.XXX, s.IMP, 4 ),I(  "CLV", s.CLV, s.IMP, 2 ),I(  "LDA", s.LDA, s.ABY, 4 ),I(  "TSX", s.TSX, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 4 ),I(  "LDY", s.LDY, s.ABX, 4 ),I(  "LDA", s.LDA, s.ABX, 4 ),I(  "LDX", s.LDX, s.ABY, 4 ),I(  "???", s.XXX, s.IMP, 4 ),
            I( "CPY", s.CPY, s.IMM, 2 ),I(  "CMP", s.CMP, s.IZX, 6 ),I(  "???", s.NOP, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 8 ),I(  "CPY", s.CPY, s.ZP0, 3 ),I(  "CMP", s.CMP, s.ZP0, 3 ),I(  "DEC", s.DEC, s.ZP0, 5 ),I(  "???", s.XXX, s.IMP, 5 ),I(  "INY", s.INY, s.IMP, 2 ),I(  "CMP", s.CMP, s.IMM, 2 ),I(  "DEX", s.DEX, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 2 ),I(  "CPY", s.CPY, s.ABS, 4 ),I(  "CMP", s.CMP, s.ABS, 4 ),I(  "DEC", s.DEC, s.ABS, 6 ),I(  "???", s.XXX, s.IMP, 6 ),
            I( "BNE", s.BNE, s.REL, 2 ),I(  "CMP", s.CMP, s.IZY, 5 ),I(  "???", s.XXX, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 8 ),I(  "???", s.NOP, s.IMP, 4 ),I(  "CMP", s.CMP, s.ZPX, 4 ),I(  "DEC", s.DEC, s.ZPX, 6 ),I(  "???", s.XXX, s.IMP, 6 ),I(  "CLD", s.CLD, s.IMP, 2 ),I(  "CMP", s.CMP, s.ABY, 4 ),I(  "NOP", s.NOP, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 7 ),I(  "???", s.NOP, s.IMP, 4 ),I(  "CMP", s.CMP, s.ABX, 4 ),I(  "DEC", s.DEC, s.ABX, 7 ),I(  "???", s.XXX, s.IMP, 7 ),
            I( "CPX", s.CPX, s.IMM, 2 ),I(  "SBC", s.SBC, s.IZX, 6 ),I(  "???", s.NOP, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 8 ),I(  "CPX", s.CPX, s.ZP0, 3 ),I(  "SBC", s.SBC, s.ZP0, 3 ),I(  "INC", s.INC, s.ZP0, 5 ),I(  "???", s.XXX, s.IMP, 5 ),I(  "INX", s.INX, s.IMP, 2 ),I(  "SBC", s.SBC, s.IMM, 2 ),I(  "NOP", s.NOP, s.IMP, 2 ),I(  "???", s.SBC, s.IMP, 2 ),I(  "CPX", s.CPX, s.ABS, 4 ),I(  "SBC", s.SBC, s.ABS, 4 ),I(  "INC", s.INC, s.ABS, 6 ),I(  "???", s.XXX, s.IMP, 6 ),
            I( "BEQ", s.BEQ, s.REL, 2 ),I(  "SBC", s.SBC, s.IZY, 5 ),I(  "???", s.XXX, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 8 ),I(  "???", s.NOP, s.IMP, 4 ),I(  "SBC", s.SBC, s.ZPX, 4 ),I(  "INC", s.INC, s.ZPX, 6 ),I(  "???", s.XXX, s.IMP, 6 ),I(  "SED", s.SED, s.IMP, 2 ),I(  "SBC", s.SBC, s.ABY, 4 ),I(  "NOP", s.NOP, s.IMP, 2 ),I(  "???", s.XXX, s.IMP, 7 ),I(  "???", s.NOP, s.IMP, 4 ),I(  "SBC", s.SBC, s.ABX, 4 ),I(  "INC", s.INC, s.ABX, 7 ),I(  "???", s.XXX, s.IMP, 7 ),
        ]

        if __debug__:
            self.logfile = None

    def connectBus(self, bus) -> None:
        self.bus = bus
        self.bus.cpu = self

    def read(self, addr: uint16_t, bReadOnly: Optional[bool] = False) -> uint8_t:
        val = self.bus.cpuRead(addr, bReadOnly)
        if VERBOSE_DEBUG:
            print("cpu.cpuRead(%x) ==> %x" % (addr, val))
        return val

    def write(self, addr: uint16_t, data: uint8_t) -> None:
        self.bus.cpuWrite(addr, data)

    ###############################################################################
    # EXTERNAL INPUTS

    # Forces the 6502 into a known state. This is hard-wired inside the CPU. The
    # registers are set to 0x00, the status register is cleared except for unused
    # bit which remains at 1. An absolute address is read from location 0xFFFC
    # which contains a second address that the program counter is set to. This
    # allows the programmer to jump to a known and programmable location in the
    # memory to start executing from. Typically the programmer would set the value
    # at location 0xFFFC at compile time.
    def reset(self) -> None:
        # Get address to set program counter to
        self.addr_abs = uint16_t(0xFFFC)
        lo: uint16_t = self.read(self.addr_abs + uint16_t(0))
        hi: uint16_t = self.read(self.addr_abs + uint16_t(1))

        # Set it
        self.pc = uint16_t((hi << 8) | lo)

        # Reset internal registers
        self.a = 0
        self.x = 0
        self.y = 0
        self.stkp = uint8_t(0xFD)
        self.status = uint8_t(0x00 | FLAGS6502.U)

        # Clear internal helper variables
        self.addr_rel = uint16_t(0x0000)
        self.addr_abs = uint16_t(0x0000)
        self.fetched = uint8_t(0x00)

        # Reset takes time
        self.cycles = uint8_t(8)


    # Interrupt requests are a complex operation and only happen if the
    # "disable interrupt" flag is 0. IRQs can happen at any time, but
    # you dont want them to be destructive to the operation of the running
    # program. Therefore the current instruction is allowed to finish
    # (which I facilitate by doing the whole thing when cycles == 0) and
    # then the current program counter is stored on the stack. Then the
    # current status register is stored on the stack. When the routine
    # that services the interrupt has finished, the status register
    # and program counter can be restored to how they where before it
    # occurred. This is impemented by the "RTI" instruction. Once the IRQ
    # has happened, in a similar way to a reset, a programmable address
    # is read form hard coded location 0xFFFE, which is subsequently
    # set to the program counter.
    def irq(self) -> None:
        # If interrupts are allowed
        if (self.getFlag(FLAGS6502.I) == 0):
            # Push the program counter to the stack. It's 16-bits dont
            # forget so that takes two pushes
            self.write(0x0100 + self.stkp, (self.pc >> 8) & 0x00FF)
            self.stkp-=1
            self.write(0x0100 + self.stkp, self.pc & 0x00FF)
            self.stkp-=1

            # Then Push the status register to the stack
            self.setFlag(FLAGS6502.B, False)
            self.setFlag(FLAGS6502.U, True)
            self.setFlag(FLAGS6502.I, True)
            self.write(0x0100 + self.stkp, self.status)
            self.stkp-=1

            # Read new program counter location from fixed address
            self.addr_abs = uint16_t(0xFFFE)
            lo: uint16_t = self.read(self.addr_abs + 0)
            hi: uint16_t = self.read(self.addr_abs + 1)
            self.pc = uint16_t((hi << 8) | lo)

            # IRQs take time
            self.cycles = uint8_t(7)


    # A Non-Maskable Interrupt cannot be ignored. It behaves in exactly the
    # same way as a regular IRQ, but reads the new program counter address
    # form location 0xFFFA.
    def nmi(self) -> None:
        self.write(0x0100 + self.stkp, (self.pc >> 8) & 0x00FF)
        self.stkp-=1
        self.write(0x0100 + self.stkp, self.pc & 0x00FF)
        self.stkp-=1

        self.setFlag(FLAGS6502.B, False)
        self.setFlag(FLAGS6502.U, True)
        self.setFlag(FLAGS6502.I, True)
        self.write(0x0100 + self.stkp, self.status)
        self.stkp-=1

        self.addr_abs = uint16_t(0xFFFA)
        lo: uint16_t = self.read(self.addr_abs + 0)
        hi: uint16_t = self.read(self.addr_abs + 1)
        self.pc = uint16_t((hi << 8) | lo)

        self.cycles = uint8_t(8)

    # Perform one clock cycles worth of emulation
    def clock(self) -> None:
        # Each instruction requires a variable number of clock cycles to execute.
        # In my emulation, I only care about the final result and so I perform
        # the entire computation in one hit. In hardware, each clock cycle would
        # perform "microcode" style transformations of the CPUs state.
        #
        # To remain compliant with connected devices, it's important that the
        # emulation also takes "time" in order to execute instructions, so I
        # implement that delay by simply counting down the cycles required by
        # the instruction. When it reaches 0, the instruction is complete, and
        # the next one is ready to be executed.
        if VERBOSE_DEBUG:
            print("clock: %d, pc = %x, type=%s" % (self.cycles, self.pc, type(self.pc)))
        if (self.cycles == 0):
            # Read next instruction byte. This 8-bit value is used to index
            # the translation table to get the relevant information about
            # how to implement the instruction
            self.opcode = self.read(self.pc)

            if DEBUG:
                log_pc: uint16_t = self.pc

            # Always set the unused status flag bit to 1
            self.setFlag(FLAGS6502.U, True)

            # Increment program counter, we read the opcode byte
            self.pc = (self.pc + 1) & 0xffff

            # Get Starting number of cycles
            self.cycles = self.lookup[self.opcode].cycles

            # Perform fetch of intermmediate data using the
            # required addressing mode
            additional_cycle1: uint8_t = (self.lookup[self.opcode].addrmode)()

            if VERBOSE_DEBUG:
                print(self.lookup[self.opcode].name)

            # Perform operation
            additional_cycle2: uint8_t = (self.lookup[self.opcode].operate)()

            # The addressmode and opcode may have altered the number
            # of cycles this instruction requires before its completed
            self.cycles += (additional_cycle1 & additional_cycle2)

            # Always set the unused status flag bit to 1
            self.setFlag(FLAGS6502.U, True)

            if DEBUG:
                # This logger dumps every cycle the entire processor state for analysis.
                # This can be used for debugging the emulation, but has little utility
                # during emulation. Its also very slow, so only use if you have to.
                if (self.logfile == None):    self.logfile = open("olc6502.txt", "w")
                if (self.logfile != None):
                    self.logfile.write("%10d:%02d PC:%04X %s A:%02X X:%02X Y:%02X %s%s%s%s%s%s%s%s STKP:%02X\n" %
                        (self.clock_count, 0, log_pc, "XXX", self.a, self.x, self.y,
                        ("N" if self.getFlag(FLAGS6502.N) else "."), ("V" if self.getFlag(FLAGS6502.V) else "."), ("U" if self.getFlag(FLAGS6502.U) else "."),
                        ("B" if self.getFlag(FLAGS6502.B) else "."), ("D" if self.getFlag(FLAGS6502.D) else "."), ("I" if self.getFlag(FLAGS6502.I) else "."),
                        ("Z" if self.getFlag(FLAGS6502.Z) else "."), ("C" if self.getFlag(FLAGS6502.C) else "."), self.stkp))

        # Increment global clock count - This is actually unused unless logging is enabled
        # but I've kept it in because its a handy watch variable for debugging
        self.clock_count+=1

        # Decrement the number of cycles remaining for this instruction
        self.cycles-=1

    # Convenience functions to access status register
    def getFlag(self, f: FLAGS6502) -> uint8_t:
        return 1 if ((self.status & f) > 0) else 0

    def setFlag(self, f: FLAGS6502, v: bool) -> None:
        if (v):
            self.status |= uint8_t(f)
        else:
            self.status &= ~uint8_t(f)

    ##############################################################################
    # ADDRESSING MODES

    # The 6502 can address between 0x0000 - 0xFFFF. The high byte is often referred
    # to as the "page", and the low byte is the offset into that page. This implies
    # there are 256 pages, each containing 256 bytes.
    #
    # Several addressing modes have the potential to require an additional clock
    # cycle if they cross a page boundary. This is combined with several instructions
    # that enable this additional clock cycle. So each addressing function returns
    # a flag saying it has potential, as does each instruction. If both instruction
    # and address function return 1, then an additional clock cycle is required.


    # Address Mode: Implied
    # There is no additional data required for this instruction. The instruction
    # does something very simple like like sets a status bit. However, we will
    # target the accumulator, for instructions like PHA
    def IMP(self) -> uint8_t:
        self.fetched = self.a
        return 0


    # Address Mode: Immediate
    # The instruction expects the next byte to be used as a value, so we'll prep
    # the read address to point to the next byte
    def IMM(self) -> uint8_t:
        self.addr_abs = self.pc
        self.pc = uint16_t(self.pc + 1)
        return 0

    # Address Mode: Zero Page
    # To save program bytes, zero page addressing allows you to absolutely address
    # a location in first 0xFF bytes of address range. Clearly this only requires
    # one byte instead of the usual two.
    def ZP0(self) -> uint8_t:
        self.addr_abs = self.read(self.pc)
        self.pc = (self.pc + 1) & 0xffff
        self.addr_abs &= uint16_t(0x00FF)
        return 0

    # Address Mode: Zero Page with X Offset
    # Fundamentally the same as Zero Page addressing, but the contents of the X Register
    # is added to the supplied single byte address. This is useful for iterating through
    # ranges within the first page.
    def ZPX(self) -> uint8_t:
        self.addr_abs = (self.read(self.pc) + self.x)
        self.pc = (self.pc + 1) & 0xffff
        self.addr_abs &= uint16_t(0x00FF)
        return 0

    # Address Mode: Zero Page with Y Offset
    # Same as above but uses Y Register for offset
    def ZPY(self) -> uint8_t:
        self.addr_abs = (self.read(self.pc) + self.y)
        self.pc = (self.pc + 1) & 0xffff
        self.addr_abs &= uint16_t(0x00FF)
        return 0

    # Address Mode: Relative
    # This address mode is exclusive to branch instructions. The address
    # must reside within -128 to +127 of the branch instruction, i.e.
    # you cant directly branch to any address in the addressable range.
    def REL(self) -> uint8_t:
        self.addr_rel = self.read(self.pc)
        self.pc = (self.pc + 1) & 0xffff
        if (self.addr_rel & 0x80):
            self.addr_rel |= uint16_t(0xFF00)
        return 0

    # Address Mode: Absolute
    # A full 16-bit address is loaded and used
    def ABS(self) -> uint8_t:
        lo: uint16_t = self.read(self.pc)
        self.pc = (self.pc + 1) & 0xffff
        hi: uint16_t = self.read(self.pc)
        self.pc = (self.pc + 1) & 0xffff

        self.addr_abs = uint16_t((hi << 8) | lo)

        return 0

    # Address Mode: Absolute with X Offset
    # Fundamentally the same as absolute addressing, but the contents of the X Register
    # is added to the supplied two byte address. If the resulting address changes
    # the page, an additional clock cycle is required
    def ABX(self) -> uint8_t:
        lo: uint16_t = self.read(self.pc)
        self.pc = (self.pc + 1) & 0xffff
        hi: uint16_t = self.read(self.pc)
        self.pc = (self.pc + 1) & 0xffff

        self.addr_abs = uint16_t((hi << 8) | lo)
        self.addr_abs += self.x

        if ((self.addr_abs & 0xFF00) != (hi << 8)):
            return 1
        else:
            return 0


    # Address Mode: Absolute with Y Offset
    # Fundamentally the same as absolute addressing, but the contents of the Y Register
    # is added to the supplied two byte address. If the resulting address changes
    # the page, an additional clock cycle is required
    def ABY(self) -> uint8_t:
        lo: uint16_t = self.read(self.pc)
        self.pc = (self.pc + 1) & 0xffff
        hi: uint16_t = self.read(self.pc)
        self.pc = (self.pc + 1) & 0xffff

        self.addr_abs = uint16_t((hi << 8) | lo)
        self.addr_abs += self.y

        if ((self.addr_abs & 0xFF00) != (hi << 8)):
            return 1
        else:
            return 0

    # Note: The next 3 address modes use indirection (aka Pointers!)

    # Address Mode: Indirect
    # The supplied 16-bit address is read to get the actual 16-bit address. This is
    # instruction is unusual in that it has a bug in the hardware! To emulate its
    # function accurately, we also need to emulate this bug. If the low byte of the
    # supplied address is 0xFF, then to read the high byte of the actual address
    # we need to cross a page boundary. This doesnt actually work on the chip as
    # designed, instead it wraps back around in the same page, yielding an
    # invalid actual address
    def IND(self) -> uint8_t:
        ptr_lo: uint16_t = self.read(self.pc)
        self.pc = (self.pc + 1) & 0xffff
        ptr_hi: uint16_t = self.read(self.pc)
        self.pc = (self.pc + 1) & 0xffff

        ptr: uint16_t = uint16_t((ptr_hi << 8) | ptr_lo)

        if (ptr_lo == 0x00FF): # Simulate page boundary hardware bug
            self.addr_abs = (self.read(ptr & 0xFF00) << 8) | self.read(ptr + 0)
        else: # Behave normally
            self.addr_abs = (self.read(ptr + 1) << 8) | self.read(ptr + 0)

        return 0


    # Address Mode: Indirect X
    # The supplied 8-bit address is offset by X Register to index
    # a location in page 0x00. The actual 16-bit address is read
    # from this location
    def IZX(self) -> uint8_t:
        t: uint16_t = self.read(self.pc)
        self.pc = (self.pc + 1) & 0xffff

        lo: uint16_t = self.read((t + self.x) & 0x00FF)
        hi: uint16_t = self.read((t + self.x + 1) & 0x00FF)

        self.addr_abs = uint16_t((hi << 8) | lo)

        return 0

    # Address Mode: Indirect Y
    # The supplied 8-bit address indexes a location in page 0x00. From
    # here the actual 16-bit address is read, and the contents of
    # Y Register is added to it to offset it. If the offset causes a
    # change in page then an additional clock cycle is required.
    def IZY(self) -> uint8_t:
        t: uint16_t = self.read(self.pc)
        self.pc = (self.pc + 1) & 0xffff

        lo: uint16_t = self.read(t & 0x00FF)
        hi: uint16_t = self.read((t + 1) & 0x00FF)

        self.addr_abs = uint16_t((hi << 8) | lo)
        self.addr_abs += self.y

        if ((self.addr_abs & 0xFF00) != (hi << 8)):
            return 1
        else:
            return 0

    # This function sources the data used by the instruction into
    # a convenient numeric variable. Some instructions dont have to
    # fetch data as the source is implied by the instruction. For example
    # "INX" increments the X register. There is no additional data
    # required. For all other addressing modes, the data resides at
    # the location held within addr_abs, so it is read from there.
    # Immediate adress mode exploits this slightly, as that has
    # set addr_abs = pc + 1, so it fetches the data from the
    # next byte for example "LDA $FF" just loads the accumulator with
    # 256, i.e. no far reaching memory fetch is required. "fetched"
    # is a variable global to the CPU, and is set by calling this
    # function. It also returns it for convenience.
    def fetch(self) -> uint8_t:
        if (self.lookup[self.opcode].addrmode != self.IMP):
            self.fetched = self.read(self.addr_abs)
        if VERBOSE_DEBUG:
            print("fetch(%x) = %x" % (self.addr_abs, self.fetched))

        return self.fetched





    ################################################################################
    # INSTRUCTION IMPLEMENTATIONS

    # Note: Ive started with the two most complicated instructions to emulate, which
    # ironically is addition and subtraction! Ive tried to include a detailed
    # explanation as to why they are so complex, yet so fundamental. Im also NOT
    # going to do this through the explanation of 1 and 2's complement.

    # Instruction: Add with Carry In
    # Function:    A = A + M + C
    # Flags Out:   C, V, N, Z
    #
    # Explanation:
    # The purpose of this function is to add a value to the accumulator and a carry bit. If
    # the result is > 255 there is an overflow setting the carry bit. Ths allows you to
    # chain together ADC instructions to add numbers larger than 8-bits. This in itself is
    # simple, however the 6502 supports the concepts of Negativity/Positivity and Signed Overflow.
    #
    # 10000100 = 128 + 4 = 132 in normal circumstances, we know this as unsigned and it allows
    # us to represent numbers between 0 and 255 (given 8 bits). The 6502 can also interpret
    # this word as something else if we assume those 8 bits represent the range -128 to +127,
    # i.e. it has become signed.
    #
    # Since 132 > 127, it effectively wraps around, through -128, to -124. This wraparound is
    # called overflow, and this is a useful to know as it indicates that the calculation has
    # gone outside the permissable range, and therefore no longer makes numeric sense.
    #
    # Note the implementation of ADD is the same in binary, this is just about how the numbers
    # are represented, so the word 10000100 can be both -124 and 132 depending upon the
    # context the programming is using it in. We can prove this!
    #
    #  10000100 =  132  or  -124
    # +00010001 = + 17      + 17
    #  ========    ===       ===     See, both are valid additions, but our interpretation of
    #  10010101 =  149  or  -107     the context changes the value, not the hardware!
    #
    # In principle under the -128 to 127 range:
    # 10000000 = -128, 11111111 = -1, 00000000 = 0, 00000000 = +1, 01111111 = +127
    # therefore negative numbers have the most significant set, positive numbers do not
    #
    # To assist us, the 6502 can set the overflow flag, if the result of the addition has
    # wrapped around. V <- ~(A^M) & A^(A+M+C) :D lol, let's work out why!
    #
    # Let's suppose we have A = 30, M = 10 and C = 0
    #          A = 30 = 00011110
    #          M = 10 = 00001010+
    #     RESULT = 40 = 00101000
    #
    # Here we have not gone out of range. The resulting significant bit has not changed.
    # So let's make a truth table to understand when overflow has occurred. Here I take
    # the MSB of each component, where R is RESULT.
    #
    # A  M  R | V | A^R | A^M |~(A^M) |
    # 0  0  0 | 0 |  0  |  0  |   1   |
    # 0  0  1 | 1 |  1  |  0  |   1   |
    # 0  1  0 | 0 |  0  |  1  |   0   |
    # 0  1  1 | 0 |  1  |  1  |   0   |  so V = ~(A^M) & (A^R)
    # 1  0  0 | 0 |  1  |  1  |   0   |
    # 1  0  1 | 0 |  0  |  1  |   0   |
    # 1  1  0 | 1 |  1  |  0  |   1   |
    # 1  1  1 | 0 |  0  |  0  |   1   |
    #
    # We can see how the above equation calculates V, based on A, M and R. V was chosen
    # based on the following hypothesis:
    #       Positive Number + Positive Number = Negative Result -> Overflow
    #       Negative Number + Negative Number = Positive Result -> Overflow
    #       Positive Number + Negative Number = Either Result -> Cannot Overflow
    #       Positive Number + Positive Number = Positive Result -> OK! No Overflow
    #       Negative Number + Negative Number = Negative Result -> OK! NO Overflow

    def ADC(self) -> uint8_t:
        # Grab the data that we are adding to the accumulator
        self.fetch()

        # Add is performed in 16-bit domain for emulation to capture any
        # carry bit, which will exist in bit 8 of the 16-bit word
        self.temp = uint16_t(self.a) + uint16_t(self.fetched) + uint16_t(self.getFlag(FLAGS6502.C))

        # The carry flag out exists in the high byte bit 0
        self.setFlag(FLAGS6502.C, self.temp > 255)

        # The Zero flag is set if the result is 0
        self.setFlag(FLAGS6502.Z, (self.temp & 0x00FF) == 0)

        # The signed Overflow flag is set based on all that up there! :D
        self.setFlag(FLAGS6502.V, (~(uint16_t(self.a) ^ uint16_t(self.fetched)) & (uint16_t(self.a) ^ uint16_t(self.temp))) & uint16_t(0x0080))

        # The negative flag is set to the most significant bit of the result
        self.setFlag(FLAGS6502.N, self.temp & 0x80)

        # Load the result into the accumulator (it's 8-bit dont forget!)
        self.a = uint8_t(self.temp & 0x00FF)

        # This instruction has the potential to require an additional clock cycle
        return 1


    # Instruction: Subtraction with Borrow In
    # Function:    A = A - M - (1 - C)
    # Flags Out:   C, V, N, Z
    #
    # Explanation:
    # Given the explanation for ADC above, we can reorganise our data
    # to use the same computation for addition, for subtraction by multiplying
    # the data by -1, i.e. make it negative
    #
    # A = A - M - (1 - C)  ->  A = A + -1 * (M - (1 - C))  ->  A = A + (-M + 1 + C)
    #
    # To make a signed positive number negative, we can invert the bits and add 1
    # (OK, I lied, a little bit of 1 and 2s complement :P)
    #
    #  5 = 00000101
    # -5 = 11111010 + 00000001 = 11111011 (or 251 in our 0 to 255 range)
    #
    # The range is actually unimportant, because if I take the value 15, and add 251
    # to it, given we wrap around at 256, the result is 10, so it has effectively
    # subtracted 5, which was the original intention. (15 + 251) % 256 = 10
    #
    # Note that the equation above used (1-C), but this got converted to + 1 + C.
    # This means we already have the +1, so all we need to do is invert the bits
    # of M, the data(!) therfore we can simply add, exactly the same way we did
    # before.

    def SBC(self) -> uint8_t:
        self.fetch()

        # Operating in 16-bit domain to capture carry out

        # We can invert the bottom 8 bits with bitwise xor
        value: uint16_t = (self.fetched) ^ 0x00FF

        # Notice this is exactly the same as addition from here!
        self.temp = uint16_t(self.a) + uint16_t(self.fetched) + uint16_t(self.getFlag(FLAGS6502.C))
        self.setFlag(FLAGS6502.C, self.temp & 0xFF00)
        self.setFlag(FLAGS6502.Z, ((self.temp & 0x00FF) == 0))
        self.setFlag(FLAGS6502.V, (self.temp ^ self.a) & (self.temp ^ value) & 0x0080)
        self.setFlag(FLAGS6502.N, self.temp & 0x0080)
        self.a = uint8_t(self.temp & 0x00FF)
        return 1

    # OK! Complicated operations are done! the following are much simpler
    # and conventional. The typical order of events is:
    # 1) Fetch the data you are working with
    # 2) Perform calculation
    # 3) Store the result in desired place
    # 4) Set Flags of the status register
    # 5) Return if instruction has potential to require additional
    #    clock cycle


    # Instruction: Bitwise Logic AND
    # Function:    A = A & M
    # Flags Out:   N, Z
    def AND(self) -> uint8_t:
        self.fetch()
        self.a = self.a & self.fetched
        self.setFlag(FLAGS6502.Z, self.a == 0x00)
        self.setFlag(FLAGS6502.N, self.a & 0x80)
        return 1


    # Instruction: Arithmetic Shift Left
    # Function:    A = C <- (A << 1) <- 0
    # Flags Out:   N, Z, C
    def ASL(self) -> uint8_t:
        self.fetch()
        self.temp = self.fetched << 1
        self.setFlag(FLAGS6502.C, (self.temp & 0xFF00) > 0)
        self.setFlag(FLAGS6502.Z, (self.temp & 0x00FF) == 0x00)
        self.setFlag(FLAGS6502.N, self.temp & 0x80)
        if (self.lookup[self.opcode].addrmode == self.IMP):
            self.a = uint8_t(self.temp & 0x00FF)
        else:
            self.write(self.addr_abs, uint8_t(self.temp & 0x00FF))
        return 0


    # Instruction: Branch if Carry Clear
    # Function:    if(C == 0) pc = address
    def BCC(self) -> uint8_t:
        if (self.getFlag(FLAGS6502.C) == 0):
            self.cycles+=1
            self.addr_abs = (self.pc + self.addr_rel) & 0xffff

            if((self.addr_abs & 0xFF00) != (self.pc & 0xFF00)):
                self.cycles+=1

            self.pc = self.addr_abs
        return 0


    # Instruction: Branch if Carry Set
    # Function:    if(C == 1) pc = address
    def BCS(self) -> uint8_t:
        if (self.getFlag(FLAGS6502.C) == 1):
            self.cycles+=1
            self.addr_abs = (self.pc + self.addr_rel) & 0xffff

            if ((self.addr_abs & 0xFF00) != (self.pc & 0xFF00)):
                self.cycles+=1

            self.pc = self.addr_abs
        return 0


    # Instruction: Branch if Equal
    # Function:    if(Z == 1) pc = address
    def BEQ(self) -> uint8_t:
        if (self.getFlag(FLAGS6502.Z) == 1):
            self.cycles+=1
            self.addr_abs = (self.pc + self.addr_rel) & 0xffff

            if ((self.addr_abs & 0xFF00) != (self.pc & 0xFF00)):
                self.cycles+=1

            self.pc = self.addr_abs
        return 0

    def BIT(self) -> uint8_t:
        self.fetch()
        self.temp = self.a & self.fetched
        self.setFlag(FLAGS6502.Z, (self.temp & 0x00FF) == 0x00)
        self.setFlag(FLAGS6502.N, self.fetched & (1 << 7))
        self.setFlag(FLAGS6502.V, self.fetched & (1 << 6))
        return 0


    # Instruction: Branch if Negative
    # Function:    if(N == 1) pc = address
    def BMI(self) -> uint8_t:
        if (self.getFlag(FLAGS6502.N) == 1):
            self.cycles+=1
            self.addr_abs = (self.pc + self.addr_rel) & 0xffff

            if ((self.addr_abs & 0xFF00) != (self.pc & 0xFF00)):
                self.cycles+=1

            self.pc = self.addr_abs
        return 0


    # Instruction: Branch if Not Equal
    # Function:    if(Z == 0) pc = address
    def BNE(self) -> uint8_t:
        if (self.getFlag(FLAGS6502.Z) == 0):
            self.cycles+=1
            self.addr_abs = (self.pc + self.addr_rel) & 0xffff

            if ((self.addr_abs & 0xFF00) != (self.pc & 0xFF00)):
                self.cycles+=1

            self.pc = self.addr_abs
        return 0


    # Instruction: Branch if Positive
    # Function:    if(N == 0) pc = address
    def BPL(self) -> uint8_t:
        if (self.getFlag(FLAGS6502.N) == 0):
            self.cycles+=1
            self.addr_abs = (self.pc + self.addr_rel) & 0xffff

            if ((self.addr_abs & 0xFF00) != (self.pc & 0xFF00)):
                self.cycles+=1

            self.pc = self.addr_abs
        return 0

    # Instruction: Break
    # Function:    Program Sourced Interrupt
    def BRK(self) -> uint8_t:
        self.pc = self.pc + uint16_t(1)

        self.setFlag(FLAGS6502.I, True)
        self.write(0x0100 + self.stkp, (self.pc >> 8) & 0x00FF)
        self.stkp-=1
        self.write(0x0100 + self.stkp, self.pc & 0x00FF)
        self.stkp-=1

        self.setFlag(FLAGS6502.B, True)
        self.write(0x0100 + self.stkp, self.status)
        self.stkp-=1
        self.setFlag(FLAGS6502.B, False)

        self.pc = self.read(0xFFFE) | (self.read(0xFFFF) << 8)
        return 0


    # Instruction: Branch if Overflow Clear
    # Function:    if(V == 0) pc = address
    def BVC(self) -> uint8_t:
        if (self.getFlag(FLAGS6502.V) == 0):
            self.cycles+=1
            self.addr_abs = (self.pc + self.addr_rel) & 0xffff

            if ((self.addr_abs & 0xFF00) != (self.pc & 0xFF00)):
                self.cycles+=1

            self.pc = self.addr_abs
        return 0


    # Instruction: Branch if Overflow Set
    # Function:    if(V == 1) pc = address
    def BVS(self) -> uint8_t:
        if (self.getFlag(FLAGS6502.V) == 1):
            self.cycles+=1
            self.addr_abs = (self.pc + self.addr_rel) & 0xffff

            if ((self.addr_abs & 0xFF00) != (self.pc & 0xFF00)):
                self.cycles+=1

            self.pc = self.addr_abs
        return 0


    # Instruction: Clear Carry Flag
    # Function:    C = 0
    def CLC(self) -> uint8_t:
        self.setFlag(FLAGS6502.C, False)
        return 0


    # Instruction: Clear Decimal Flag
    # Function:    D = 0
    def CLD(self) -> uint8_t:
        self.setFlag(FLAGS6502.D, False)
        return 0


    # Instruction: Disable Interrupts / Clear Interrupt Flag
    # Function:    I = 0
    def CLI(self) -> uint8_t:
        self.setFlag(FLAGS6502.I, False)
        return 0


    # Instruction: Clear Overflow Flag
    # Function:    V = 0
    def CLV(self) -> uint8_t:
        self.setFlag(FLAGS6502.V, False)
        return 0

    # Instruction: Compare Accumulator
    # Function:    C <- A >= M      Z <- (A - M) == 0
    # Flags Out:   N, C, Z
    def CMP(self) -> uint8_t:
        self.fetch()
        self.temp = self.a - self.fetched
        self.setFlag(FLAGS6502.C, self.a >= self.fetched)
        self.setFlag(FLAGS6502.Z, (self.temp & 0x00FF) == 0x0000)
        self.setFlag(FLAGS6502.N, self.temp & 0x0080)
        return 1


    # Instruction: Compare X Register
    # Function:    C <- X >= M      Z <- (X - M) == 0
    # Flags Out:   N, C, Z
    def CPX(self) -> uint8_t:
        self.fetch()
        self.temp = self.x - self.fetched
        self.setFlag(FLAGS6502.C, self.x >= self.fetched)
        self.setFlag(FLAGS6502.Z, (self.temp & 0x00FF) == 0x0000)
        self.setFlag(FLAGS6502.N, self.temp & 0x0080)
        return 0


    # Instruction: Compare Y Register
    # Function:    C <- Y >= M      Z <- (Y - M) == 0
    # Flags Out:   N, C, Z
    def CPY(self) -> uint8_t:
        self.fetch()
        self.temp = self.y - self.fetched
        self.setFlag(FLAGS6502.C, self.y >= self.fetched)
        self.setFlag(FLAGS6502.Z, (self.temp & 0x00FF) == 0x0000)
        self.setFlag(FLAGS6502.N, self.temp & 0x0080)
        return 0


    # Instruction: Decrement Value at Memory Location
    # Function:    M = M - 1
    # Flags Out:   N, Z
    def DEC(self) -> uint8_t:
        self.fetch()
        self.temp = self.fetched - 1
        self.write(self.addr_abs, self.temp & 0x00FF)
        self.setFlag(FLAGS6502.Z, (self.temp & 0x00FF) == 0x0000)
        self.setFlag(FLAGS6502.N, self.temp & 0x0080)
        return 0


    # Instruction: Decrement X Register
    # Function:    X = X - 1
    # Flags Out:   N, Z
    def DEX(self) -> uint8_t:
        self.x-=1
        self.setFlag(FLAGS6502.Z, self.x == 0x00)
        self.setFlag(FLAGS6502.N, self.x & 0x80)
        return 0


    # Instruction: Decrement Y Register
    # Function:    Y = Y - 1
    # Flags Out:   N, Z
    def DEY(self) -> uint8_t:
        self.y-=1
        self.setFlag(FLAGS6502.Z, self.y == 0x00)
        self.setFlag(FLAGS6502.N, self.y & 0x80)
        return 0


    # Instruction: Bitwise Logic XOR
    # Function:    A = A xor M
    # Flags Out:   N, Z
    def EOR(self) -> uint8_t:
        self.fetch()
        self.a = self.a ^ self.fetched
        self.setFlag(FLAGS6502.Z, self.a == 0x00)
        self.setFlag(FLAGS6502.N, self.a & 0x80)
        return 1


    # Instruction: Increment Value at Memory Location
    # Function:    M = M + 1
    # Flags Out:   N, Z
    def INC(self) -> uint8_t:
        self.fetch()
        self.temp = self.fetched + 1
        self.write(self.addr_abs, self.temp & 0x00FF)
        self.setFlag(FLAGS6502.Z, (self.temp & 0x00FF) == 0x0000)
        self.setFlag(FLAGS6502.N, self.temp & 0x0080)
        return 0


    # Instruction: Increment X Register
    # Function:    X = X + 1
    # Flags Out:   N, Z
    def INX(self) -> uint8_t:
        self.x+=1
        self.setFlag(FLAGS6502.Z, self.x == 0x00)
        self.setFlag(FLAGS6502.N, self.x & 0x80)
        return 0


    # Instruction: Increment Y Register
    # Function:    Y = Y + 1
    # Flags Out:   N, Z
    def INY(self) -> uint8_t:
        self.y+=1
        self.setFlag(FLAGS6502.Z, self.y == 0x00)
        self.setFlag(FLAGS6502.N, self.y & 0x80)
        return 0


    # Instruction: Jump To Location
    # Function:    pc = address
    def JMP(self) -> uint8_t:
        self.pc = self.addr_abs
        return 0


    # Instruction: Jump To Sub-Routine
    # Function:    Push current pc to stack, pc = address
    def JSR(self) -> uint8_t:
        self.pc-=1

        self.write(0x0100 + self.stkp, (self.pc >> 8) & 0x00FF)
        self.stkp-=1
        self.write(0x0100 + self.stkp, self.pc & 0x00FF)
        self.stkp-=1

        self.pc = self.addr_abs
        return 0


    # Instruction: Load The Accumulator
    # Function:    A = M
    # Flags Out:   N, Z
    def LDA(self) -> uint8_t:
        self.fetch()
        self.a = self.fetched
        self.setFlag(FLAGS6502.Z, self.a == 0x00)
        self.setFlag(FLAGS6502.N, self.a & 0x80)
        return 1


    # Instruction: Load The X Register
    # Function:    X = M
    # Flags Out:   N, Z
    def LDX(self) -> uint8_t:
        self.fetch()
        self.x = self.fetched
        self.setFlag(FLAGS6502.Z, self.x == 0x00)
        self.setFlag(FLAGS6502.N, self.x & 0x80)
        if VERBOSE_DEBUG:
            print("LDX: x=%x" % self.x)
        return 1


    # Instruction: Load The Y Register
    # Function:    Y = M
    # Flags Out:   N, Z
    def LDY(self) -> uint8_t:
        self.fetch()
        self.y = self.fetched
        self.setFlag(FLAGS6502.Z, self.y == 0x00)
        self.setFlag(FLAGS6502.N, self.y & 0x80)
        return 1

    def LSR(self) -> uint8_t:
        self.fetch()
        self.setFlag(FLAGS6502.C, self.fetched & 0x0001)
        self.temp = self.fetched >> 1
        self.setFlag(FLAGS6502.Z, (self.temp & 0x00FF) == 0x0000)
        self.setFlag(FLAGS6502.N, self.temp & 0x0080)
        if (self.lookup[self.opcode].addrmode == self.IMP):
            self.a = uint8_t(self.temp & 0x00FF)
        else:
            self.write(self.addr_abs, self.temp & 0x00FF)
        return 0

    def NOP(self) -> uint8_t:
        # Sadly not all NOPs are equal, Ive added a few here
        # based on https:#wiki.nesdev.com/w/index.php/CPU_unofficial_opcodes
        # and will add more based on game compatibility, and ultimately
        # I'd like to cover all illegal opcodes too
        if self.opcode in [0x1C, 0x3C, 0x5C, 0x7C, 0xDC, 0xFC]:
            return 1

        return 0


    # Instruction: Bitwise Logic OR
    # Function:    A = A | M
    # Flags Out:   N, Z
    def ORA(self) -> uint8_t:
        self.fetch()
        self.a = self.a | self.fetched
        self.setFlag(FLAGS6502.Z, self.a == 0x00)
        self.setFlag(FLAGS6502.N, self.a & 0x80)
        return 1


    # Instruction: Push Accumulator to Stack
    # Function:    A -> stack
    def PHA(self) -> uint8_t:
        self.write(0x0100 + self.stkp, self.a)
        self.stkp-=1
        return 0


    # Instruction: Push Status Register to Stack
    # Function:    status -> stack
    # Note:        Break flag is set to 1 before push
    def PHP(self) -> uint8_t:
        self.write(0x0100 + self.stkp, self.status | FLAGS6502.B | FLAGS6502.U)
        self.setFlag(FLAGS6502.B, False)
        self.setFlag(FLAGS6502.U, False)
        self.stkp-=1
        return 0


    # Instruction: Pop Accumulator off Stack
    # Function:    A <- stack
    # Flags Out:   N, Z
    def PLA(self) -> uint8_t:
        self.stkp+=1
        self.a = self.read(0x0100 + self.stkp)
        self.setFlag(FLAGS6502.Z, self.a == 0x00)
        self.setFlag(FLAGS6502.N, self.a & 0x80)
        return 0


    # Instruction: Pop Status Register off Stack
    # Function:    Status <- stack
    def PLP(self) -> uint8_t:
        self.stkp+=1
        self.status = self.read(0x0100 + self.stkp)
        self.setFlag(FLAGS6502.U, True)
        return 0

    def ROL(self) -> uint8_t:
        self.fetch()
        self.temp = (self.fetched << 1) | self.getFlag(FLAGS6502.C)
        self.setFlag(FLAGS6502.C, self.temp & 0xFF00)
        self.setFlag(FLAGS6502.Z, (self.temp & 0x00FF) == 0x0000)
        self.setFlag(FLAGS6502.N, self.temp & 0x0080)
        if (self.lookup[self.opcode].addrmode == self.IMP):
            self.a = self.temp & 0x00FF
        else:
            self.write(self.addr_abs, self.temp & 0x00FF)
        return 0

    def ROR(self) -> uint8_t:
        self.fetch()
        self.temp = (self.getFlag(FLAGS6502.C) << 7) | (self.fetched >> 1)
        self.setFlag(FLAGS6502.C, self.fetched & 0x01)
        self.setFlag(FLAGS6502.Z, (self.temp & 0x00FF) == 0x00)
        self.setFlag(FLAGS6502.N, self.temp & 0x0080)
        if (self.lookup[self.opcode].addrmode == self.IMP):
            self.a = self.temp & 0x00FF
        else:
            self.write(self.addr_abs, self.temp & 0x00FF)
        return 0

    def RTI(self) -> uint8_t:
        self.stkp+=1
        self.status = self.read(0x0100 + self.stkp)
        self.status &= ~FLAGS6502.B
        self.status &= ~FLAGS6502.U

        self.stkp+=1
        self.pc = self.read(0x0100 + self.stkp)
        self.stkp+=1
        self.pc |= self.read(0x0100 + self.stkp) << 8
        return 0

    def RTS(self) -> uint8_t:
        self.stkp+=1
        self.pc = self.read(0x0100 + self.stkp)
        self.stkp+=1
        self.pc |= self.read(0x0100 + self.stkp) << 8

        self.pc = (self.pc + 1) & 0xffff
        return 0




    # Instruction: Set Carry Flag
    # Function:    C = 1
    def SEC(self) -> uint8_t:
        self.setFlag(FLAGS6502.C, True)
        return 0


    # Instruction: Set Decimal Flag
    # Function:    D = 1
    def SED(self) -> uint8_t:
        self.setFlag(FLAGS6502.D, True)
        return 0


    # Instruction: Set Interrupt Flag / Enable Interrupts
    # Function:    I = 1
    def SEI(self) -> uint8_t:
        self.setFlag(FLAGS6502.I, True)
        return 0


    # Instruction: Store Accumulator at Address
    # Function:    M = A
    def STA(self) -> uint8_t:
        self.write(self.addr_abs, self.a)
        return 0


    # Instruction: Store X Register at Address
    # Function:    M = X
    def STX(self) -> uint8_t:
        self.write(self.addr_abs, self.x)
        return 0


    # Instruction: Store Y Register at Address
    # Function:    M = Y
    def STY(self) -> uint8_t:
        self.write(self.addr_abs, self.y)
        return 0


    # Instruction: Transfer Accumulator to X Register
    # Function:    X = A
    # Flags Out:   N, Z
    def TAX(self) -> uint8_t:
        self.x = self.a
        self.setFlag(FLAGS6502.Z, self.x == 0x00)
        self.setFlag(FLAGS6502.N, self.x & 0x80)
        return 0


    # Instruction: Transfer Accumulator to Y Register
    # Function:    Y = A
    # Flags Out:   N, Z
    def TAY(self) -> uint8_t:
        self.y = self.a
        self.setFlag(FLAGS6502.Z, self.y == 0x00)
        self.setFlag(FLAGS6502.N, self.y & 0x80)
        return 0


    # Instruction: Transfer Stack Pointer to X Register
    # Function:    X = stack pointer
    # Flags Out:   N, Z
    def TSX(self) -> uint8_t:
        self.x = self.stkp
        self.setFlag(FLAGS6502.Z, self.x == 0x00)
        self.setFlag(FLAGS6502.N, self.x & 0x80)
        return 0


    # Instruction: Transfer X Register to Accumulator
    # Function:    A = X
    # Flags Out:   N, Z
    def TXA(self) -> uint8_t:
        self.a = self.x
        self.setFlag(FLAGS6502.Z, self.a == 0x00)
        self.setFlag(FLAGS6502.N, self.a & 0x80)
        return 0


    # Instruction: Transfer X Register to Stack Pointer
    # Function:    stack pointer = X
    def TXS(self) -> uint8_t:
        self.stkp = self.x
        return 0


    # Instruction: Transfer Y Register to Accumulator
    # Function:    A = Y
    # Flags Out:   N, Z
    def TYA(self) -> uint8_t:
        self.a = self.y
        self.setFlag(FLAGS6502.Z, self.a == 0x00)
        self.setFlag(FLAGS6502.N, self.a & 0x80)
        return 0


    # This function captures illegal opcodes
    def XXX(self) -> uint8_t:
        return 0





    #######################################/
    # HELPER FUNCTIONS

    def complete(self) -> bool:
        return self.cycles == 0

    # This is the disassembly function. Its workings are not required for emulation.
    # It is merely a convenience function to turn the binary instruction code into
    # human readable form. Its included as part of the emulator because it can take
    # advantage of many of the CPUs internal operations to do this.
    def disassemble(self, nStart: uint16_t, nStop: uint16_t) -> Dict[uint16_t, str]:
        addr: uint32_t = nStart
        value: uint8_t = 0x00
        lo: uint8_t = 0x00
        hi: uint8_t = 0x00
        mapLines: Dict[uint16_t, str] = dict()
        line_addr: uint16_t = 0

        # Starting at the specified address we read an instruction
        # byte, which in turn yields information from the lookup table
        # as to how many additional bytes we need to read and what the
        # addressing mode is. I need this info to assemble human readable
        # syntax, which is different depending upon the addressing mode

        # As the instruction is decoded, a std::string is assembled
        # with the readable output
        while (addr <= nStop):
            line_addr = addr

            # Prefix line with instruction address
            sInst: str = "$" + hex(addr)[2:] + ": "

            # Read instruction, and get its readable name
            opcode: uint8_t = self.bus.cpuRead(addr, True)
            addr+=1
            sInst += self.lookup[opcode].name + " "

            # Get oprands from desired locations, and form the
            # instruction based upon its addressing mode. These
            # routines mimmick the actual fetch routine of the
            # 6502 in order to get accurate data as part of the
            # instruction
            if (self.lookup[opcode].addrmode == self.IMP):
                sInst += " {IMP}"
            elif (self.lookup[opcode].addrmode == self.IMM):
                value = self.bus.cpuRead(addr, True)
                if addr >= 0x8000 and addr <=0x8020:
                    print("[dis] addr=%x value=%x" % (addr, value))

                addr+=1
                sInst += "#$" + hex(value) + " {IMM}"
            elif (self.lookup[opcode].addrmode == self.ZP0):
                lo = self.bus.cpuRead(addr, True)
                addr+=1
                hi = 0x00
                sInst += "$" + hex(lo) + " {ZP0}"
            elif (self.lookup[opcode].addrmode == self.ZPX):
                lo = self.bus.cpuRead(addr, True)
                addr+=1
                hi = 0x00
                sInst += "$" + hex(lo) + ", X {ZPX}"
            elif (self.lookup[opcode].addrmode == self.ZPY):
                lo = self.bus.read(addr, True)
                addr+=1
                hi = 0x00
                sInst += "$" + hex(lo) + ", Y {ZPY}"
            elif (self.lookup[opcode].addrmode == self.IZX):
                lo = self.bus.cpuRead(addr, True)
                addr+=1
                hi = 0x00
                sInst += "($" + hex(lo) + ", X) {IZX}"
            elif (self.lookup[opcode].addrmode == self.IZY):
                lo = self.bus.cpuRead(addr, True)
                addr+=1
                hi = 0x00
                sInst += "($" + hex(lo) + "), Y {IZY}"
            elif (self.lookup[opcode].addrmode == self.ABS):
                lo = self.bus.cpuRead(addr, True)
                addr+=1
                hi = self.bus.cpuRead(addr, True)
                addr+=1
                sInst += "$" + hex((hi << 8) | lo) + " {ABS}"
            elif (self.lookup[opcode].addrmode == self.ABX):
                lo = self.bus.cpuRead(addr, True)
                addr+=1
                hi = self.bus.cpuRead(addr, True)
                addr+=1
                sInst += "$" + hex((hi << 8) | lo) + ", X {ABX}"
            elif (self.lookup[opcode].addrmode == self.ABY):
                lo = self.bus.cpuRead(addr, True)
                addr+=1
                hi = self.bus.cpuRead(addr, True)
                addr+=1
                sInst += "$" + hex((hi << 8) | lo) + ", Y {ABY}"
            elif (self.lookup[opcode].addrmode == self.IND):
                lo = self.bus.cpuRead(addr, True)
                addr+=1
                hi = self.bus.cpuRead(addr, True)
                addr+=1
                sInst += "($" + hex((hi << 8) | lo) + ") {IND}"
            elif (self.lookup[opcode].addrmode == self.REL):
                value = self.bus.cpuRead(addr, True)
                addr+=1
                sInst += "$" + hex(value) + " [$" + hex(addr + value) + "] {REL}"

            # Add the formed string to a std::map, using the instruction's
            # address as the key. This makes it convenient to look for later
            # as the instructions are variable in length, so a straight up
            # incremental index is not sufficient.
            mapLines[line_addr] = sInst

        return mapLines

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
    cpu: Py6502
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

def emulate_frame(bus):
    # Clocking. The heart and soul of an emulator. The running
    # frequency is controlled by whatever calls this function.
    # So here we "divide" the clock as necessary and call
    # the peripheral devices clock() function at the correct
    # times.

    # The fastest clock frequency the digital system cares
    # about is equivalent to the PPU clock. So the PPU is clocked
    # each time this function is called.

    # Advance renderer - it never stops, it's relentless
    scanline = 0
    cycle = 0
    for scanline in range(0, 262):
        bus.ppu.scanline = scanline
        bus.ppu.cycle = 0
        for cycle in range(0, 342):
            # Fake some noise for now
            bus.ppu.cycle = cycle
            bus.ppu.sprScreen.SetPixel(bus.ppu.cycle - 1, bus.ppu.scanline, bus.ppu.palScreen[0x3F if randint(0, 1) else 0x30])
            # The CPU runs 3 times slower than the PPU so we only call its
            # clock() function every 3 times this function is called. We
            # have a global counter to keep track of this.
            if (bus.nSystemClockCounter % 3 == 0):
                bus.cpu.clock()
            bus.nSystemClockCounter+=1
    bus.ppu.scanline = -1
    bus.ppu.frame_complete = True