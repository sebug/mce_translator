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
        
type trans_obj = SingleTranslation of PathElement * Translation | TranslationGroup of PathElement * (trans_obj seq)

// Intermediary representation
type TranslationLine = TranslationLine of TranslationPath * BaseValue * Translation

exception CantDropHeadException of string

let drop_head (lines: TranslationLine seq): TranslationLine seq =
    lines
    |> Seq.map (fun l ->
                match l with
                | TranslationLine(_ :: rest,e,bt) -> TranslationLine(rest,e,bt)
                | TranslationLine([],e,bt) -> raise (CantDropHeadException("Can't drop head of empty path")))

let rec translation_tree (lines: TranslationLine seq): trans_obj seq =
    let base_elements =
        lines
        |> Seq.map (fun l ->
                    match l with
                    | TranslationLine([pel],_,trans) -> Some(SingleTranslation(pel,trans))
                    | _ -> None )
    let step =
        lines
        |> Seq.groupBy (fun l ->
                        match l with
                        | TranslationLine(f :: s :: _,_,_) -> Some(f)
                        | TranslationLine([one],_,_) -> None
                        | TranslationLine([],_,_) -> None)
        |> Seq.map (fun lg ->
                    match lg with
                    | (None,_) -> None
                    | (Some(pe),itms) -> Some(TranslationGroup(pe,drop_head itms |> translation_tree)))
    Seq.append base_elements step
    |> Seq.fold (fun acc elem ->
                 match elem with
                 | Some(e) -> Seq.append acc (Seq.singleton e)
                 | None -> acc) Seq.empty

let ind (indent: int) = String.replicate indent " "

let rec print (offset: int) (indent: int) (nl: string) (items: trans_obj seq) : string =
    (ind indent) + "{" + nl +
    (items
     |> Seq.map (fun obj ->
                match obj with
                | SingleTranslation(pel,trans) -> "\"" + pel + "\": \"" + trans + "\""
                | TranslationGroup(pel,itms) ->
                   let subobj = print offset (indent + offset) nl itms
                   "\"" + pel + "\":" + nl + subobj)
     |> Seq.map (fun l -> (ind (indent + offset)) + l)
     |> String.concat ("," + nl)) + nl + (ind indent) + "}"
                 

let tl_from_input (l: TranslationLines.Row): TranslationLine =
    let path = path_elements l.TranslationPath
    TranslationLine(path,l.English,l.Translated)

// the translation call
let functionCall = "tinyMCE.addI18n"

// the main call is different in format from the rest
let is_main (p: string) =
    p = "tiny_mce\langs\en.js"

exception UnexpectedFormatException of string

let write_file (filename: string) (lines: TranslationLines.Row seq) =
    printfn "File name: %s" filename
    let tobjs =
        lines
        |> Seq.map tl_from_input
        |> translation_tree
    let file_content =
        if is_main filename then
            sprintf "%s(%s);" functionCall  (print 0 0 "" tobjs)
        else
            match (Seq.head tobjs) with
            | TranslationGroup(pel,itms) ->
                sprintf "%s('%s',%s);" functionCall pel (print 0 0 "" itms)
            | SingleTranslation(pel,trans) ->
                raise (UnexpectedFormatException("Expected translation group"))
    printfn "%s" file_content
    

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
        |> Seq.iter (fun fg -> write_file (fst fg) (snd fg))
    0
