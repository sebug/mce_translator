//
// Main program
//
open FSharp.Data

type TranslationLines = CsvProvider<"example_input.csv">

let rec path_element (pstr: string) (acc: string) : string * string =
    if pstr.Length = 0 then (acc, "")
    else
        match pstr.[0] with
        | '.' -> (acc, pstr.Substring(1))
        | c -> path_element (pstr.Substring(1)) (acc + c.ToString())

let rec path_elements (pstr: string) : string list =
    if pstr = "" then []
    else
        let p = path_element pstr ""
        match p with
        | (cur, rest) -> cur :: (path_elements rest)

[<EntryPoint>]
let entry args =
    if args.Length < 2 then
        printfn "%s" "Usage: mce_translator input.csv outputdir"
    else
        let inputCsv = args.[0]
        let outputDir = args.[1]
        let lines = TranslationLines.Load(inputCsv)
        lines.Data
        |> Seq.iter (fun l ->
                     printfn "%s" (path_elements l.TranslationPath |> String.concat ","))
    0
