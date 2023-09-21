# Teatrino

This repository contains Teatrino, a code generation toolchain supporting
Multiparty Session Type (MPST) protocols with crash-stop failures. The tool
generates Scala code that uses the Effpi concurrency library. Teatrino was
presented in the ECOOP23 publication [*Designing Asynchronous Multiparty Protocols with Crash-Stop Failures*](https://doi.org/10.4230/LIPIcs.ECOOP.2023.1) and its accompanying [artefact](https://doi.org/10.4230/DARTS.9.2.9).

## Instructions

This tool is built using the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/). It has been tested on an M2 Mac running macOS Ventura 13.5 and 
GHCup v0.1.19.4 with Stack v2.9.3, cabal v3.6.2, and GHC v9.2.8. Key commands 
are:

- `stack build` to build the tool
- `stack exec -- Teatrino` to run the tool from within the directory
- `stack install` to enable usage of the tool elsewhere in your system
- `stack clean` to clean the working directory

Example Scribble protocols can be found in the `scribble/` subdirectory.
