sim1802
=======

Simulator for the [RCA CDP1802](https://en.wikipedia.org/wiki/RCA_1802) processor

Build
-----

    $ make
    $ make PREFIX=${PREFIX} install

This installs the simulator as `cdp1802-unknown-elf-sim` in `${PREFIX}/bin/`,
a DejaGnu board definition `cdp1802-sim.exp` in `${PREFIX}/dejagnu/`, and
updates `${HOME}/.dejagnurc` to make that directory searchable by DejaGnu.

Usage
-----

This simulator runs ELF executables produced by the CDP1802 toolchain
(ports of GNU binutils and GCC, custom libc):

    $ echo 'extern int puts(const char *); int main(void) { puts("hello"); return 0; }' > hello.c
    $ cdp1802-unknown-elf-gcc -O hello.c
    $ cdp1802-unknown-elf-sim a.out
    hello

To use this simulator to run the test suite for `cdp1802-unknown-elf-gcc`,
append `RUNTESTFLAGS=--target_board=cdp1802-sim` to the `make check` command.

Use with Intel HEX files
------------------------

    $ a18 priv/hello.asm -l hello.lst -o hello.hex
    ...
    No Errors
    $ bin/sim1802 hello.hex
    HELO

For this use case you need a CDP1802 cross-assembler capable of emitting Intel
HEX files. I use [a18](https://github.com/mikpe/A18).

Dependencies
------------

The simulator is written in Erlang so you need that to build and run it.
