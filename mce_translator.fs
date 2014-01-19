//
// Main program
//
open FSharp.Data

type TranslationLines = CsvProvider<"example_input.csv">

[<EntryPoint>]
let entry args =
    if args.Length < 2 then
        printfn "%s" "Usage: mce_translator input.csv outputdir"
    else
        let inputCsv = args.[0]
        let outputDir = args.[1]
        let lines = TranslationLines.Load(inputCsv)
        lines.Data
        |> Seq.iter (fun l -> printfn "%s" l.FilePath)
    0
