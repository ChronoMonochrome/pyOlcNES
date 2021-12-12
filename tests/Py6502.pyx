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

uint32_t = uint16_t = uint8_t = int

from typing import *
from pyOlcNES import FLAGS6502
from lookup import get_lookup_table

lookup = []

cdef class Py6502:
    # CPU Core registers, exposed as public here for ease of access from external
    # examinors. This is all the 6502 has.
    cdef public int a         # Accumulator Register
    cdef public int x         # X Register
    cdef public int y         # Y Register
    cdef public int stkp      # Stack Pointer (points to location on bus)
    cdef public int pc        # Program Counter
    cdef public int status    # Status Register

    # Assisstive variables to facilitate emulation
    cdef public int fetched     # Represents the working input value to the ALU
    cdef public int temp        # A convenience variable used everywhere
    cdef public int addr_abs    # All used memory addresses end up in here
    cdef public int addr_rel    # Represents absolute address following a branch
    cdef public int opcode      # Is the instruction byte
    cdef public int cycles      # Counts how many cycles the instruction has remaining
    cdef public int clock_count # A global accumulation of the number of clocks
	
    cdef public object bus

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
        
        global lookup
        lookup = get_lookup_table(self)

    def connectBus(self, bus) -> None:
        self.bus = bus
        self.bus.cpu = self

    def read(self, addr: uint16_t, bReadOnly: Optional[bool] = False) -> uint8_t:
        val = self.bus.cpuRead(addr, bReadOnly)
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
        if (self.cycles == 0):
            # Read next instruction byte. This 8-bit value is used to index
            # the translation table to get the relevant information about
            # how to implement the instruction
            self.opcode = self.read(self.pc)

            # Always set the unused status flag bit to 1
            #self.setFlag(FLAGS6502.U, True)

            # Increment program counter, we read the opcode byte
            self.pc = (self.pc + 1) & 0xffff

            # Get Starting number of cycles
            self.cycles = lookup[self.opcode].cycles

            # Perform fetch of intermmediate data using the
            # required addressing mode
            additional_cycle1: uint8_t = (lookup[self.opcode].addrmode)()

            # Perform operation
            additional_cycle2: uint8_t = (lookup[self.opcode].operate)()

            # The addressmode and opcode may have altered the number
            # of cycles this instruction requires before its completed
            self.cycles += (additional_cycle1 & additional_cycle2)

            # Always set the unused status flag bit to 1
            #self.setFlag(FLAGS6502.U, True)

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
        if (lookup[self.opcode].addrmode == self.IMP):
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
        if (lookup[self.opcode].addrmode == self.IMP):
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
        if (lookup[self.opcode].addrmode == self.IMP):
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
        if (lookup[self.opcode].addrmode == self.IMP):
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
        if (lookup[self.opcode].addrmode != self.IMP):
            self.fetched = self.read(self.addr_abs)

        return self.fetched


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
            sInst += lookup[opcode].name + " "

            # Get oprands from desired locations, and form the
            # instruction based upon its addressing mode. These
            # routines mimmick the actual fetch routine of the
            # 6502 in order to get accurate data as part of the
            # instruction
            if (lookup[opcode].addrmode == self.IMP):
                sInst += " {IMP}"
            elif (lookup[opcode].addrmode == self.IMM):
                value = self.bus.cpuRead(addr, True)
                if addr >= 0x8000 and addr <=0x8020:
                    print("[dis] addr=%x value=%x" % (addr, value))

                addr+=1
                sInst += "#$" + hex(value) + " {IMM}"
            elif (lookup[opcode].addrmode == self.ZP0):
                lo = self.bus.cpuRead(addr, True)
                addr+=1
                hi = 0x00
                sInst += "$" + hex(lo) + " {ZP0}"
            elif (lookup[opcode].addrmode == self.ZPX):
                lo = self.bus.cpuRead(addr, True)
                addr+=1
                hi = 0x00
                sInst += "$" + hex(lo) + ", X {ZPX}"
            elif (lookup[opcode].addrmode == self.ZPY):
                lo = self.bus.read(addr, True)
                addr+=1
                hi = 0x00
                sInst += "$" + hex(lo) + ", Y {ZPY}"
            elif (lookup[opcode].addrmode == self.IZX):
                lo = self.bus.cpuRead(addr, True)
                addr+=1
                hi = 0x00
                sInst += "($" + hex(lo) + ", X) {IZX}"
            elif (lookup[opcode].addrmode == self.IZY):
                lo = self.bus.cpuRead(addr, True)
                addr+=1
                hi = 0x00
                sInst += "($" + hex(lo) + "), Y {IZY}"
            elif (lookup[opcode].addrmode == self.ABS):
                lo = self.bus.cpuRead(addr, True)
                addr+=1
                hi = self.bus.cpuRead(addr, True)
                addr+=1
                sInst += "$" + hex((hi << 8) | lo) + " {ABS}"
            elif (lookup[opcode].addrmode == self.ABX):
                lo = self.bus.cpuRead(addr, True)
                addr+=1
                hi = self.bus.cpuRead(addr, True)
                addr+=1
                sInst += "$" + hex((hi << 8) | lo) + ", X {ABX}"
            elif (lookup[opcode].addrmode == self.ABY):
                lo = self.bus.cpuRead(addr, True)
                addr+=1
                hi = self.bus.cpuRead(addr, True)
                addr+=1
                sInst += "$" + hex((hi << 8) | lo) + ", Y {ABY}"
            elif (lookup[opcode].addrmode == self.IND):
                lo = self.bus.cpuRead(addr, True)
                addr+=1
                hi = self.bus.cpuRead(addr, True)
                addr+=1
                sInst += "($" + hex((hi << 8) | lo) + ") {IND}"
            elif (lookup[opcode].addrmode == self.REL):
                value = self.bus.cpuRead(addr, True)
                addr+=1
                sInst += "$" + hex(value) + " [$" + hex(addr + value) + "] {REL}"

            # Add the formed string to a std::map, using the instruction's
            # address as the key. This makes it convenient to look for later
            # as the instructions are variable in length, so a straight up
            # incremental index is not sufficient.
            mapLines[line_addr] = sInst

        return mapLines