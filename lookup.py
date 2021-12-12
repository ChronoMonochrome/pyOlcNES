from typing import *

uint32_t = uint16_t = uint8_t = int

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


I = INSTRUCTION

# Assembles the translation table. It's big, it's ugly, but it yields a convenient way
# to emulate the 6502. I'm certain there are some "code-golf" strategies to reduce this
# but I've deliberately kept it verbose for study and alteration

# It is 16x16 entries. This gives 256 instructions. It is arranged to that the bottom
# 4 bits of the instruction choose the column, and the top 4 bits choose the row.

# For convenience to get function pointers to members of this class, I'm using this
# or else it will be much much larger :D

def get_lookup_table(s):
    return [
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