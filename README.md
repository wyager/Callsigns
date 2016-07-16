# Callsigns
A program that generates available 5-digit callsigns

To use this, you need to have the `HS.dat` database of callsign events from the FCC.

To compile, run `ghc -O2 Callsigns.hs`.

To run, run `cat /usr/share/dict/web2 | ./Callsigns`. You must have `HS.dat` in the same directory. You can use any dictionary file. The program will also try to use plurals and words with a single letter missing.

When it is finished processing the database, it will start to spit out available callsigns. The whole thing takes about 3 seconds on my computer.