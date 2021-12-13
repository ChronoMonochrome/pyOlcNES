from pyOlcNES import Bus, Cartridge, FLAGS6502, emulate_frame

import time

nes = Bus()
cart = Cartridge(r"C:\Users\chrono\Downloads\nestest.nes")
if (not cart.bImageValid):
    exit()

nes.insertCartridge(cart)
mapAsm = nes.cpu.disassemble(0x0000, 0x1FFF)

nes.reset()

frameCount = 0
frameMaxCount = 30
while True:
    try:
        t1 = time.time()
        emulate_frame(nes)
        nes.ppu.frame_complete = False
        print("frameCount=%d, dt=%f" % (frameCount, time.time() - t1))
        frameCount += 1
    except KeyboardInterrupt:
        exit()
        
    if frameCount > frameMaxCount:
        exit()