module Grep

open System
open System.IO
open System.Text

type FileOptions = {pattern : string
                    files : string list
                    lineNums : bool
                    fileNames : bool
                    noCase : bool
                    invert : bool
                    wholeLine : bool}

let defaultOptions = { pattern = ""
                       files = [""]
                       lineNums = false
                       fileNames = false
                       noCase = false
                       invert = false
                       wholeLine = false
                     }

let rec parseFlags (flags : string list) acc = 
    match flags with
    |[] -> acc
    |"-n"::flags' -> parseFlags flags' {acc with lineNums = true}
    |"-l"::flags' -> parseFlags flags' {acc with fileNames = true}
    |"-i"::flags' -> parseFlags flags' {acc with noCase = true}
    |"-v"::flags' -> parseFlags flags' {acc with invert = true}
    |"-x"::flags' -> parseFlags flags' {acc with wholeLine = true}
    //just ignore bad flags
    | _::flags' -> parseFlags flags' acc

// record to generate for each processed line 
type MatchInfo = {fileName : string
                  lineNum : int
                  lineText : string
                  wholeLine : bool 
                  isMatch : bool }

/// does the actual matching of the supplied pattern with each line in the input file(s);
/// problem doesn't call for globbing/pattern pattern matching, so the only condition to check
/// is if we're matching on case
let lineMatch (line : string) opts =
    match opts.noCase with
    | true -> line.IndexOf(opts.pattern, StringComparison.OrdinalIgnoreCase) >= 0
    | false -> line.Contains(opts.pattern) 

/// reads input file into array of lines, and converts each line into a MatchInfo record. This includes info
///necessary to satisfy other flags that might be present
let readFile options file = 
    File.ReadAllLines(file)
    |> Array.mapi (fun i line -> 
        //if a line matches the supplied pattern and the line and pattern are the same length, 
        //then it's a whole line match
        let isFullLine = options.pattern.Length = line.Length
        {fileName = file; lineNum = i+1; lineText = line; wholeLine = isFullLine; isMatch = (lineMatch line options)}
        )
    |> List.ofArray

let grep pattern (flags : string) fileList = 
    let options = 
        parseFlags 
            (List.ofArray (flags.Split([|' '|]))) 
            {defaultOptions with pattern = pattern; files = fileList}
    

    fileList
    |> List.map (readFile options)    
    |> List.reduce List.append
    |> List.filter (fun x -> 
        //here is where the -v flag gets applied; the list of records all have isMatch set true or false
        //so we can just filter on that, along with the -x flag since a match doesn't count in that case
        //unless wholeLine is true as well
        x.isMatch = not(options.invert) && (options.wholeLine = x.wholeLine))
    |> List.map (fun x -> 
        match options.fileNames,options.lineNums with
        | true, _ -> sprintf "%s\n" x.fileName
        | false, true -> 
            if fileList.Length > 1 then
                sprintf "%s:%d:%s\n" x.fileName x.lineNum x.lineText
            else sprintf "%d:%s\n" x.lineNum x.lineText
        | _ -> 
            if fileList.Length > 1 then
                sprintf "%s:%s\n" x.fileName x.lineText
            else sprintf "%s\n" x.lineText
        )
    |> List.distinct // when -n is set there can be duplicates which this filters
    |> fun x -> String.Concat(x)
