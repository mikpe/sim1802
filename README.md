sim1802
=======

Simulator for the RCA CDP1802 processor

Build
-----

    $ make
    $ make install

Run
---

    $ a18 priv/hello.asm -l hello.lst -o hello.hex
    ...
    No Errors
    $ bin/sim1802 hello.hex
    HELO

Dependencies
------------

The simulator is written in Erlang so you need that to build and run it.

To generate code for the simulator you need a CDP1802 cross-assembler capable
of emitting Intel HEX files. I use [a18](https://github.com/mikpe/A18).
