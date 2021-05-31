# Callsigns
A program that generates available 5-digit callsigns that look like English words

To use this, you need to have the `HS.dat` database of callsign events from the FCC, available at [ftp://wirelessftp.fcc.gov/pub/uls/complete/l_amat.zip](ftp://wirelessftp.fcc.gov/pub/uls/complete/l_amat.zip).

You will also need [The Haskell Stack](https://haskellstack.org) (or plain old GHC if you prefer).

To build, run `stack build`. You can `stack install` if you want it on your system path. 

To run, run `stack exec callsigns`. You must have `HS.dat` from `l_amat.zip` in the same directory. The program will look for a list of English words at `/usr/share/dict/web2` - please change this path if you want to use an alternate wordlist. The program will also try to use plurals and words with a single letter missing.

With HS.dat at 154MB, it takes my laptop a bit under 1 second to find all available English-ish callsigns.