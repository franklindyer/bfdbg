## Static analysis

At some point, I'd like to add some utilities for static analysis of BF code. It could recognize some common patterns and comment on them for the user. For instance:

- Both `X+-Y` and `X-+Y` are identical to `XY`
- Both `X<>Y` and `X><Y` are identical to `XY`, except that one of them might possibly go out of bounds
- In `[X][Y]`, the `[Y]` block is dead code because when `[X]` ends the current cell will be zero
- In `[X+]Y`, the `Y` is dead code because the loop `[X+]` will never terminate
- Flag certain loops as being "balanced", i.e. guaranteed to end in the same cell they started in (if ever)
- A "loop linter" flagging loops following certain patterns that are guaranteed to terminate, for instance `[X[-]]` when `X` is also deemed safe

## Test suite

Add tools to run tests on a BF program that can be easily listed and parsed from a text file. These should include the following abilities:

- specifying inputs for test cases
- specifying outputs for test cases
- specifying certain properties of the memory state following a run
- specifying the head position following a run

## Transpilers

Add some tools to transpile BF code written for one architecture to behaviorally identical code for a different architecture. It's unclear, however, how the different I/O formats for the different BF architectures (e.g. ASCII chars for `bf256ou` versus natural numbers for `bfNat`) should be put in correspondence with each other.

## Virtual machines

Brainfuck programs that take BF code characters as input and execute them as a BF program. Bonus points if it can *also* take additional input.
