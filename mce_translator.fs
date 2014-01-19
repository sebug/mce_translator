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

let rec path_elements (pstr: string) : string list =
    if pstr = "" then []
    else
        let p = path_element pstr ""
        match p with
        | (cur, rest) -> cur :: (path_elements rest)

// The objects to generate
type trans_obj = SingleTranslation of string * string | TranslationGroup of string * (trans_obj list)

// the zipper (see filesystem zipper example in http://learnyouahaskell.com/zippers#a-very-simple-file-system )

type tocrumb = TOCrumb of string * (trans_obj list) * (trans_obj list)

type tozipper = trans_obj * (tocrumb list)

let to_up (z: tozipper) : tozipper =
    match z with
    | (item,TOCrumb(name,ls,rs) :: bs) -> (TranslationGroup(name,ls @ [item] @ rs), bs)

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
                     (snd fg)
                     |> Seq.iter (fun l ->
                                  printfn "%s" (path_elements l.TranslationPath |> String.concat ",")))
    0
