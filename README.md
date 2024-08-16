# hatris

A modern SRS Tetris clone written in Haskell for the terminal.

## Compiling
To set up your Haskell environment to build Hatris, first download [GHCup](https://www.haskell.org/ghcup/) and install GHC 9.6.6 and the recommended `cabal` and `Stack` versions.    
When your Haskell environment is up, run `stack build` and `stack install` to compile and install the `hatris` executable.

## Controls
Currently, the controls are hard-coded for my preferences

| Control          | Keybind     |
| ---------------- | ----------- |
| Move piece left  | Left Arrow  |
| Move piece right | Right Arrow |
| Soft drop        | Down Arrow  |
| Instant drop     | Up Arrow    |
| Hard drop        | Space       |
| CW rotation      | D           |
| CCW rotation     | S           |
| 180 rotation     | A           |
| Hold             | <           |
