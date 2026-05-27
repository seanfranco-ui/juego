module Game.Utils
open System
open System.Threading
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
let R = new Random()
let oldBlack = Console.BackgroundColor
let height = Console.BufferHeight
let widht = Console.BufferWidth
let oldcolor = Console.ForegroundColor

let displayMessage x y (mensaje:String) color =
    Console.SetCursorPosition(x,y)
    Console.ForegroundColor <- color
    mensaje |> Console.Write
let displayMessageRight y (mensaje:String) color =
     let l = mensaje.Length
     let start = widht-l-1
     Console.SetCursorPosition(start,y)
     Console.ForegroundColor <- color
     Console.Write mensaje 

let createMainLoop pipeline isProgrammingRunning keyboardPipeline  drawPipeline needToRedraw clearRedraw AlternativeKeyboard =
  
    let processKeyboard (state:'State) =
        if Console.KeyAvailable then 
            let k = Console.ReadKey true
            let a =keyboardPipeline
                            |> Array.fold (fun acc f -> acc |> f k.Key) state
            AlternativeKeyboard
            |> Array.fold (fun acc f -> acc |> f k) a
        else
            state

    let redrawScreen (state:'State) =
        if needToRedraw state then 
            Console.Clear()
            drawPipeline
            |> Array.iter (fun f -> f state)
            clearRedraw state
        else
            state

    let rec mainLoop (state:'State) =
        pipeline
        |> Array.fold ( fun acc f -> f acc) state
        |> processKeyboard
        |> redrawScreen
        |> fun newState ->
            if isProgrammingRunning newState then 
                Thread.Sleep 25
                newState |> mainLoop
            else
                newState
    
    mainLoop
let options =
   JsonFSharpOptions.Default()

    .ToJsonSerializerOptions()
JsonFSharpOptions.Default()
 .AddToJsonSerializerOptions(options)    
