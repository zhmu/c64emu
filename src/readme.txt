C64EMU
------

This is an unfinished C64 emulator which I coded way back around 1999 or so;
the main difference with other emulators is that it's coded completely in 80386
assembly, which was quite necessary back then.

The 6502 core seems fairly complete; it works well enough to make it to the
BASIC interpreter. The VIC is horribly incomplete (barely enough to display
text charachters) and everyone else is just a stub. Keyboard input will not
work either.

There's also an integrated debugger in there, which was necessary to make it
working (well that and the trusty Turbo Debugger... ah, the memories :-). The
integrated debugger can be entered by hitting ` while running the emulator; use
F8 to single-step. Escape quits.

I hope someone will find this stuff useful (even though VICE seems a much
better emulator to use these days, and it's portable!) - as the license points
out, you can do with this whatever you want, so enjoy!

Rink Springer
mail@rink.nu

Disasm
------

There's also a disassembler in there which was useful to me while studying the
kernel ROM. It's provided as a bonus.

License
-------

I'm licensing this stuff as public domain.

ROMs
----

The original C64 ROMs are required, which I may not be legally allowed to
distribute. However, they are included in the VICE 1.2 emulator ROM package,
available at
http://zimmers.net/anonftp/pub/cbm/crossplatform/emulators/VICE/old/index.html -
the vice-roms.tar.gz package contains everyone necessary to run the emulator.

You'll need to copy the following files:

C64/basic -> roms/basic.rom
C64/kernel -> roms/kernel.rom
VIC20/chargen -> roms/char.rom (note: not the original C64 charset)

The original C64 charset seems to be in at least the vice-0.14.2-roms.tar.gz
along with the other files; these should work just as well.
