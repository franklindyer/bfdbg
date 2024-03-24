## Get started

![](https://github.com/franklindyer/bfdbg/blob/main/img/thuemorse-demo.png)

This is a WIP debugger/development environment for writing Brainfuck code. It doesn't run BF code particularly *quickly*, but it includes some tools that are useful to troubleshoot your code step by step.

You can try it out for yourself as follows:

- Clone this repo.
- Enter the root directory and run `stack build`. It may take a while.
- Run `stack run code/thuemorse.bf`. This will run some sample code.

## Write your own code

Brainfuck code run with the debugger can use any of the basic 8 commands allowed in Brainfuck. There is an additional allowed symbol `@` which is used to set breakpoints in your code. When running the debugger, it will naturally pause whenever it reaches a breakpoint. You can also press `j` to have it skip ahead to the next breakpoint (or the end of the program).

The first line of any `.bf` file that you run with the debugger should contain a list of three space-separated config options. These are:

- The Brainfuck architecture to use
- The number of cells to use
- The text file to draw input from (`/dev/null` if no input desired)

For instance, the sample script `code/thuemorse.bf` begins with the following line:

```
bf2ou 800 /dev/null
```

More on the different architectures in a second. For now, suffice it to say that if you're used to the usual version of Brainfuck in which each cell holds a byte, you should write `bf256ou` as the architecture.

## Architectures

A Brainfuck architecture determines what kinds of values are stored in the cells, how they are incremented/decremented, and how IO is processed.

Right now, three architectures are supported:

- `bf256ou`: cells contain bytes that can overflow or underflow
- `bf2ou`: cells contain individual bits that can overflow or underflow
- `bfNat`: cells contain nonnegative integers that are unbounded in size

In the future, I'd like to offer versions of the first two architectures that do *not* permit over/underflow, and possibly also a ternary architecture just for giggles.
