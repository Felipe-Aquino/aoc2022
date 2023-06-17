# aoc2022
Solutions for Advent of Code 2022

Written in Ocaml.

# Steps
```bash
# Compile the day X solution, where X is a number:
$ make dayX 

# Run the day X solution:
$ ./dayX part:1 file:filename

# Clean up the files produced by the Ocaml compiler:
$ make clean
`

# For development
Run the following to create a new dayX.ml file and update the makefile.
Then use the previous steps to compile and run.

```bash
$ make dayX.ml

...

$ make dayX 
$ ./dayX part:1 file:filename
```
