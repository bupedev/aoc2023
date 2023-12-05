module AOC2023

open System.CommandLine
open System.CommandLine.Parsing
open System.Diagnostics
open System.IO
open System.Reflection
open System.Text.Json

let hasOneOf (result: CommandResult) (symbols: seq<Symbol>) =
    let count =
        result.Children
        |> Seq.filter (fun child -> symbols |> Seq.contains child.Symbol)
        |> Seq.length
    count = 1

let readInputFromFile (filePath: FileInfo) = File.ReadAllText(filePath.FullName)

let rec stringify (obj: obj) =
    let options =  JsonSerializerOptions()
    options.WriteIndented <- true
    JsonSerializer.Serialize(obj, obj.GetType(), options)
    
let processDay day input =
    let dayModuleName = $"Day%02d{day}"
    let dayType = Assembly.GetExecutingAssembly().GetType(dayModuleName)

    if isNull dayType then
        printfn $"Day %d{day} module not implemented"
    else
        let solveMethod = dayType.GetMethod("solve")
        
        if isNull solveMethod then
            printfn $"Solve method not found in %s{dayModuleName}"
        else
            let stopwatch = Stopwatch.StartNew()
        
            solveMethod.Invoke(null, [| input |]) |> stringify |> printf "%s\n"
            
            stopwatch.Stop()
            let elapsed = stopwatch.Elapsed
            printfn $"Time taken: %A{elapsed}"

let processArguments (day: int) (text: string) (file: FileInfo) =
    let input = if isNull file then text else readInputFromFile file 
    processDay day input

[<EntryPoint>]
let main args =
    let rootCommand =
        RootCommand "Bupé's solutions to Advent of Code 2023 (https://adventofcode.com/2023)"

    let dayOption = Option<int>([| "--day"; "-d" |], "The day of the puzzle.")

    let textOption =
        Option<string>([| "--text"; "-t" |], "The input text for the puzzle. Mutually exclusive with --file (-f).")

    let fileOption =
        Option<FileInfo>(
            [| "--file"; "-f" |],
            "The file containing the input text for the puzzle. Mutually exclusive with --text (-t)."
        )

    rootCommand.AddOption(dayOption)
    rootCommand.AddOption(textOption)
    rootCommand.AddOption(fileOption)

    rootCommand.AddValidator(fun result ->
        if not (hasOneOf result [ textOption; fileOption ]) then
            result.ErrorMessage <- "You must use either --text or --file")

    rootCommand.AddValidator(fun result ->
        if not (hasOneOf result [ dayOption ]) then
            result.ErrorMessage <- "You must define a puzzle --day")

    rootCommand.SetHandler(processArguments, dayOption, textOption, fileOption)

    rootCommand.Invoke(args)