# FLUB, the FLuke Utility Box

Flub is a collection of utilities for working with the Fluke 9000 /
9010a / 9100 micro system troubleshooters. It can compile Fluke
programs into the hex format the Fluke expects, decompile hex back
into the source code, and calculate the signature of ROMs.

## Commands

You do not need to type the full command name. You may use any unique
subset of the commands.

### Compile

Produce a .H (ASCII-hex) file from Fluke .S source.

```
$ flub compile input.s
$ flub compile one.s two.s
```

### Decompile

Produce a Fluke .S source dump of a .H file.

```
$ flub decompile input.h
```

### Generate Signature

Produces Fluke-compatible signatures for ROMs.

```
$ flub sig input input...
```

### Copy

FIXME - should dump out a serial port

### Changes from the official tools

 - There is no restriction on line length.
 - BINARY statements are not currently supported.
 - Every character in a symbolic name is significant, instead of only
   the first eight.

## Status

 - Binary <-> Fluke hex codec. Working. -> `flub.io.hex`
 - Parse POD files. Working. -> `flub.parser.pod`
 - Process include files. Working.
 - Parse source code into AST. 90% working. `BINARY` statement not
   supported. -> `flub.parser.source`
 - Emit bytes from AST. Incomplete. -> `flub.assembler.core`
 - Parse hex into AST. Half working. Parses into an intermediate AST,
   but needs to go from that to source AST. -> `flub.parser.recordng`
 - Produce source code from source AST. Not implemented.
 - Uploading via serial port. Not implemented.

## License

Copyright © 2013, 2014 Ian Eure

Distributed under the Eclipse Public License, the same as Clojure.
