module Game.Router

open Types
//
// La funcion de este modulo es decidir
// que se muestra en la pantalla
//

type RouterState =
| ShowingStartMenu
| ShowingGame
| LoadingGame
| Terminated

let initialState = ShowingStartMenu

let rec mainLoop state =
    match state with 
    | ShowingStartMenu ->
        match Menu.mostrar() with 
        | StartGame -> ShowingGame
        | LoadGame -> LoadingGame
        | Exit -> Terminated
    | ShowingGame -> 
        Loop.mostrar()
        ShowingStartMenu
    | LoadingGame ->
        Loop.cargar()
        ShowingStartMenu
    | Terminated ->
        Terminated
    |> fun s ->
        if s <> Terminated then
            mainLoop s

let mostrar() =
    initialState
    |> mainLoop