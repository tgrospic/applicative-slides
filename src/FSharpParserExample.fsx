#load "FSharpApplicativeOptionParser.fs"

open erecruit.OptionParser

open System

/// Parser for "read" command-line option.
let readOpt : unit OptionParser =
  i_ *> first "read" <* br <*
  i_ *> text "Read settings from database to a file (--path)."

/// Parser for "write" command-line option.
let writeOpt : unit OptionParser =
  i_ *> first "write" <* br <*
  i_ *> text "Write settings to database from a file (--path)."

/// Parser for "--user-id" command-line option.
let userOpt : Guid OptionParser =
  i_ *> guid (strShort "u" <|> strLong "user-id") <* br <*
  i__ *> text "ID of the user making changes."

/// Parser for "--path" command-line option.
let filePathOpt : string OptionParser =
  i_ *> (strShort "p" <|> strLong "path") <* br <*
  i__ *> text "Path to XML settings file."

/// This function creates parser for read command (chain of option parsers).
/// On successful parse `func` is called which represent our command to be executed.
let commandRead (func: string -> 'a) =
  func
  <!> br *> text "Usage: csman read [options]"
   *> readOpt *> br
   *> filePathOpt <* br

/// This function creates parser for write command (chain of option parsers).
/// On successful parse `func` is called which represents our command to be executed.
let commandWrite (func: Guid -> string -> 'a) =
  func
  <!> br *> text "Usage: csman write [options]"
   *> writeOpt *> br
   *> userOpt <* br
  <*> filePathOpt <* br

/// This holds the final result of the program.
type CmdResult = Success of string
               | Failure of string

let examples = sprintf "%s\n" """Examples:
  csman read -p c:\user\settings.xml

  csman write -u 6E5AB85A-B786-42DE-B7A8-3DACD28957C2
              -p c:\user\settings.xml
"""

/// Help command returns example text when executed.
let commandHelp =
  (fun () -> Success examples)
  <!> br *> text "Usage: csman help" *> br
   *> first "help"
   *> i_ *> text "Display usage with examples." <* br

/// This function adds help parser (command) to a given parser.
let wrapHelp (a: CmdResult OptionParser) : CmdResult OptionParser =
  let axs,fa = a
  let bxs,fb = commandHelp
  axs @ [" "] @ bxs, fun (env: Env) ->
    match fa env with
    | Empty Error,env1 ->
      match fb env with
      // If help command fails, returns errors from previous parser.
      | Empty Error,_ -> Empty Error,env1
      | Empty (Ok (Success x)),env2 ->
        // If help command succeeds, renders both parsers + help result message (with examples).
        let txt = sprintf "%s\n%s" (render (a <|> commandHelp)) x
        Empty (Ok (Success txt)),env2
      | other -> other
    | other -> other

// Main operations/effects
let readFromDb path = Success <| sprintf "READ DB -> %s" path
let writeToDb  user path = Success <| sprintf "USER %A WRITE DB <- %s" user path

/// Creates parser to parse read or write command.
let commands = wrapHelp (commandRead readFromDb <|> commandWrite writeToDb)

let args = ["read"; "-p"; "c:\user\settings.xml"; "-u"; "6E5AB85A-B786-42DE-B7A8-3DACD28957C2"]

let readResult = run commands ["read"; "-p"; "c:\user\settings.xml";]

let writeResult = run commands ["write"; "-p"; "c:\user\settings.xml"; "-u"; "6E5AB85A-B786-42DE-B7A8-3DACD28957C2";]

let failure = run commands ["write"; "-p"; "c:\user\settings.xml"; "-u"]


