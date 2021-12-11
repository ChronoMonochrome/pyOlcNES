import os
from myNES import Py6502

def ni(py6502):
    while True:
        py6502.clock()
        if py6502.complete():
            return
            
def step_and_debug_print(py6502, mapAsm):
    ni(py6502)
    for k in list(mapAsm.keys()):
        prefix = ">>> " if k == py6502.pc else ""
        if k >= 0x8000 and k <=0x8020:
            print(prefix + mapAsm[k])
    print("STATUS: %x, %s" % (py6502.status, type(py6502.status)))
    print("STKP: %x, %s" % (py6502.stkp, type(py6502.stkp)))
    print("PC: %x, %s" % (py6502.pc, type(py6502.pc)))
    print("A: %x, %s" % (py6502.a, type(py6502.a)))
    print("X: %x, %s" % (py6502.x, type(py6502.x)))
    print("Y: %x, %s" % (py6502.y, type(py6502.y)))
    
def main():
    os.chdir(r"D:\dev\python\nes")
    py6502 = Py6502()
    ss="A2 0A 8E 00 00 A2 03 8E 01 00 AC 00 00 A9 00 18 6D 01 00 88 D0 FA 8D 02 00 EA EA EA"

    offset=0x8000
    for c in ss.split(" "):
        py6502.bus.ram[offset] = eval("0x"+c)
        offset += 1

        
    py6502.bus.ram[0xFFFC] = 0x00
    py6502.bus.ram[0xFFFD] = 0x80;
    mapAsm = py6502.disassemble(0x0000, 0xFFFF)
    for k in list(mapAsm.keys()):
        if k >= 0x8000 and k <=0x8020:
            print(mapAsm[k])

    py6502.reset()
    return py6502, mapAsm

if __name__ == "__main__":
    py6502, mapAsm = main()