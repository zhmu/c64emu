@echo off
tasm /zi c64emu
tasm /zi p6510
tasm /zi input
tasm /zi v6567
tasm /zi i6526
tasm /zi c64debug
tlink /v c64emu.obj+p6510.obj+v6567.obj+input.obj+i6526.obj+c64debug.obj,c64emu.exe
