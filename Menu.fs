module Game.Menu
type ProgramState =
 | Running
 | Terminated
type menu = 
 |NewGame
 |LoadGame
 |Exit 
open System
open Game.Utils

//
// Esta linea es para traer los simbolos
// del module App.Utils
//
open Game.Utils
open Game.Types
type MenuState =
| Active
| Terminated


type State = {
    MenuState: MenuState
    X: int
    Y: int
    CurSorSelection: int
    CursorX: int
    Commands: (Command * string) array
    RedrawScreen: bool
}


let initialState = {
    MenuState = Active
    X = 20
    Y = 10
    CurSorSelection = 0
    CursorX = 18
    Commands = [|
        StartGame,"Start Game"
        LoadGame, "Load Game"
        Exit,"Exit"
    |]
    RedrawScreen = true
}
let pauseState = {
    MenuState = Active
    X = 20
    Y = 10
    CurSorSelection = 0
    CursorX = 18
    Commands = [|
        Continue,"Continue"
        SaveGame, "Save Game"
        Exit,"Exit"
    |]
    RedrawScreen = true
}
let drawMenu state =
    state.Commands
    |> Array.iteri (fun i (_,legend) ->
        displayMessage state.X (state.Y+i)  legend ConsoleColor.Cyan
    )

    displayMessage state.CursorX (state.Y+state.CurSorSelection)  "*" ConsoleColor.Yellow


let updateMenuKeyboard (keyInfo: ConsoleKeyInfo) state =
    let key = keyInfo.Key
    let newState =
        match key with 
        | ConsoleKey.UpArrow -> {state with CurSorSelection = max 0 (state.CurSorSelection-1)}
        | ConsoleKey.DownArrow -> {state with CurSorSelection = min (state.Commands.Length-1) (state.CurSorSelection+1)}
        | ConsoleKey.Enter -> {state with MenuState = Terminated}
        | _ -> state

    if newState <> state then 
        {newState with RedrawScreen = true}
    else
        state

// Loop 

let myLoop = 
    createMainLoop 
        [||]
        (fun s -> s.MenuState = Active) 
        [||]
        [| drawMenu|]
        (fun s -> s.RedrawScreen)
        (fun s -> {s with RedrawScreen=false})
        [|updateMenuKeyboard|]


let mostrar() =
    let oldForeground = Console.ForegroundColor
    Console.CursorVisible <- false

    let state =
        initialState
        |> myLoop
        
    Console.CursorVisible <- true
    Console.ForegroundColor <- oldForeground
    Console.Clear()
    fst state.Commands[state.CurSorSelection]
let pauseM() =
    let oldForeground = Console.ForegroundColor
    Console.CursorVisible <- false

    let state =
        pauseState
        |> myLoop
        
    Console.CursorVisible <- true
    Console.ForegroundColor <- oldForeground
    Console.Clear()
    fst state.Commands[state.CurSorSelection]    
