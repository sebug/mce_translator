//
// Main program
//
open FSharp.Data

type TranslationLines = CsvProvider<"example_input.csv">


// Extracting path elements
exception EndGroupNotFoundError of string
exception PathSeparatorNotFoundError of string

let rec escaped_element (pstr: string) (acc: string) : string * string =
    if pstr.Length = 0 then raise (EndGroupNotFoundError("Did not find ]"))
    else
        match pstr.[0] with
        | ']' -> (acc, pstr.Substring(1))
        | c -> escaped_element (pstr.Substring(1)) (acc + c.ToString())

let rec path_element (pstr: string) (acc: string) : string * string =
    if pstr.Length = 0 then (acc, "")
    else
        match pstr.[0] with
        | '.' -> (acc, pstr.Substring(1))
        | '[' ->
            let p = escaped_element (pstr.Substring(1)) ""
            match p with
            | (eel,"") -> (eel,"")
            | (eel,more) -> if more.[0] = '.' then (eel, more.Substring(1))
                            else raise (PathSeparatorNotFoundError("Did not find ."))
        | c -> path_element (pstr.Substring(1)) (acc + c.ToString())

type PathElement = string
type TranslationPath = PathElement list

let rec path_elements (pstr: string) : TranslationPath =
    if pstr = "" then []
    else
        let p = path_element pstr ""
        match p with
        | (cur, rest) -> cur :: (path_elements rest)

// The objects to generate
type BaseValue = string
type Translation = string
        
type trans_obj = SingleTranslation of PathElement * Translation | TranslationGroup of PathElement * (trans_obj list)

// Intermediary representation
type TranslationLine = TranslationLine of TranslationPath * BaseValue * Translation

let translation_tree (lines: TranslationLine seq): trans_obj seq =
    Seq.empty

let tl_from_input (l: TranslationLines.Row): TranslationLine =
    let path = path_elements l.TranslationPath
    TranslationLine(path,l.English,l.Translated)

[<EntryPoint>]
let entry args =
    if args.Length < 2 then
        printfn "%s" "Usage: mce_translator input.csv outputdir"
    else
        let inputCsv = args.[0]
        let outputDir = args.[1]
        let lines = TranslationLines.Load(inputCsv)
        lines.Data
        |> Seq.groupBy (fun l -> l.FilePath)
        |> Seq.iter (fun fg ->
                     printfn "File name: %s" (fst fg)
                     let tls = (snd fg) |> Seq.map tl_from_input
                     let res = translation_tree tls
                     res
                     |> Seq.iter (fun tobj -> printfn "%s" (tobj.ToString())))
    0
