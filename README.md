```
  ___ __       __
.'  _|  .--.--|  |--.
|   _|  |  |  |  _  |
|__| |__|_____|_____|
```

Flub is a collection of utilities for working with the Fluke 9000 /
9010a / 9100 micro system troubleshooters. It can compile Fluke
programs into the hex format the Fluke expects, decompile hex back
into the source code, and calculate the signature of ROMs.

## Features

 - Generate ROM signatures (`flub sig`)
 - Decompile `.H` files (`flub dc`)
 - Compile `.S` files (`flub cc`)

### Planned

 - Upload/download via serial port.

## Status

 - Binary <-> Fluke hex codec. Working. -> `flub.io.hex`
 - Parse POD files. Working. -> `flub.parser.pod`
 - Parse source code into AST. 90% working. `BINARY` statement not
   supported. -> `flub.parser.source`
 - Process include files. Working.
 - Emit bytes from AST. 85% working, does not emit pod definitions or
   setup parameters ye. -> `flub.assembler`
 - Parse hex into AST. Half working. Parses into an intermediate AST,
   but needs to go from that to source AST. -> `flub.parser.record`
 - Produce source code from source AST. Not yet implemented.
 - Uploading via serial port. Not implemented.

## Commands

You do not need to type the full command name. You may use any unique
subset of the commands.

### Compile

Produce a hex from Fluke .S source.

```
$ flub compile input.s
$ flub compile one.s two.s
```

### Decompile

Produce a dump from a .H file.

```
$ flub decompile input.h
```

### Generate Signature

Produces Fluke-compatible signatures for ROMs.

```
$ flub sig input input...
```

### Changes from the official tools

 - There is no restriction on line length.
 - BINARY statements are not currently supported.
 - Every character in a symbolic name is significant, instead of only
   the first eight.

## License

Copyright © 2013, 2014 Ian Eure

Distributed under the Eclipse Public License, the same as Clojure.
