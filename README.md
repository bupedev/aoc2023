# Advent of Code 2023

This is my first year participating in Advent of Code after hearing about it from some colleagues at work. I am using it as an opportunity to become more comfortable writing code in F#, although I *might* switch to C# if I am stuggling to keep up the pace.

## Running Solutions

I looked into some previous years of Advent of Code and I noticed that most puzzles require you to write an algorithm that processes some randomized text. To accomodate this, I have established an F# console application project with command-line arguments:  

```text
Description:
  Bupé's solutions to Advent of Code 2023 (https://adventofcode.com/2023)

Usage:
  AOC2023 [options]

Options:
  -d, --day <day>    The day of the puzzle.
  -t, --text <text>  The input text for the puzzle. Mutually exclusive with --file (-f).
  -f, --file <file>  The file containing the input text for the puzzle. Mutually exclusive with --text (-t).
  --version          Show version information
  -?, -h, --help     Show help and usage information
```

The entry point for the application uses `System.Reflection` (I know, I am definitely on Santa's naughty list this year), to select the solution code for each day. Each name of each day's module must be `Day%20d`, and must contain a `solve` function that accepts a single `string` argument:

```fsharp
let solve (input: string) = ...
```

As a test, I have included `Day00.fs` in the project, which will simply print the input back to the console:

```text
> .\AOC2022.exe -d 0 -t "Hello World!"
This is a test solution to ensure the entry point is working well!
Here is the input you provided:
Hello World!
```