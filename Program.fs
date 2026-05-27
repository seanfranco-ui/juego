module Game.Loop
open System
open System.IO
open System.Threading
open System.Text.Json 
open System.Text.Json.Serialization
open FSharp.SystemTextJson
open Game.Utils 
open Game.Types
open Game.Enemy
open Game.Char
type ProgramState =
 | Running
 | Terminated

let guardar datos =  
    let json = JsonSerializer.Serialize(datos, options)
    File.WriteAllText("save.json", json)
  
let oldForeground = Console.ForegroundColor


let enemyRespawnScale = 3
let enemySpeedScale = 4
type state = {
    ProgramState: ProgramState
    Tick : int
    Clock : int
    redrawScreen : bool
    menuActive : bool
    menuSelected : int
    enter : bool
    Score : int
    EnemyInfo : EnemyInfo List
    CharInfo : CharInfo
    Termination : bool
}
let r = new Random()
let initialState = {
 ProgramState = Running
 Tick = -1
 Clock = 0
 redrawScreen = true
 menuActive = true
 menuSelected = 0
 enter = false
 Score = 0
 EnemyInfo = IEnemyCompendium
 CharInfo = InitialChar
 Termination = false
 } 


let cargarDatos() = 
    let a = File.ReadAllText("save.json")
    JsonSerializer.Deserialize<state>(a, options)   
let updateTick state =
   {state with Tick = state.Tick+1}
let updateClock state =
    if state.Tick <> 0 && state.Tick % 40 = 0 then
     {state with Clock = state.Clock+1;redrawScreen=true}
    else 
     state 
let displayScore state = 
    displayMessage 0 0 $"{state.Score}" ConsoleColor.Red   
         

let displayClock state =
  displayMessageRight 0  $"{state.Clock}" ConsoleColor.Cyan


let lookForESC key state =
    match key with
    |ConsoleKey.Escape -> {state with ProgramState = Terminated}
    |_ -> state
let lookForEnter key state=
     match key with
     |ConsoleKey.Enter -> {state with enter = true}
     |_ -> {state with enter = false}
   


let RDisplayEnemy state = displayEnemy state.EnemyInfo
let RUpdateEnemy state = 
 let x = updateEnemyPost state.EnemyInfo state.Tick state.Score state.CharInfo.X state.CharInfo.Y 
 {state with EnemyInfo = x}
let RdisplayEAttack state = displayEAttack state.EnemyInfo
let RupdateEnemyState state = 
 let a = updateEnemyState state.EnemyInfo state.CharInfo.AttackInfo.AttackX  state.CharInfo.AttackInfo.AttackY state.Tick state.Score
 {state with EnemyInfo = fst a; state.Score = snd a}                               

let RupdateEAttackPost state = 
 let a = updateEAttackPost state.EnemyInfo state.Tick
 {state with EnemyInfo = a}
 
let RenemyShoot state = 
 let a =EnemyShoot state.EnemyInfo state.Tick
 {state with EnemyInfo = a}

let RupdateEAttackState state = 
 let a = updateEAttackState state.EnemyInfo state.Tick
 {state with EnemyInfo = a}
let RprocessChar key state = 
 let a = processChar key state.CharInfo
 a 
 |> Option.map (fun s -> {state with CharInfo = s; redrawScreen = true})
 |> Option.defaultValue state

let RupdateCharState state =   
 let a = updatePlayerState state.CharInfo  state.EnemyInfo state.Tick
 {state with CharInfo = a}                                                                                                 
let RdisplayChar state = displayChar state.CharInfo
let RdisplayLives state = displayLives state.CharInfo 
let RReviveChar state = 
 let a = reviveChar state.CharInfo state.EnemyInfo state.Tick
 {state with CharInfo = a}        

let RupdateAttackState state = 
 let a =updateAttackState state.CharInfo state.Tick
 {state with CharInfo = a}
let RlookForZ key state = 
 let a = lookForZ key state.CharInfo state.Tick
 a
 |> Option.map (fun s -> {state with CharInfo = s; redrawScreen = true;})
 |> Option.defaultValue state
let RupdateAttackPost state = 
 let a = updateAttackPost state.CharInfo state.Tick 
 a
 |> Option.map (fun s -> {state with CharInfo = s; redrawScreen = true})
 |> Option.defaultValue state
let RprocessAttack key state = 
 let a = processAttack key state.CharInfo
 {state with CharInfo = a}
let RdisplayAttack state = displayAttack state.CharInfo
Console.CursorVisible <- false
let RgameOver state = 
 let a = GameOver state.CharInfo
 a
 |> Option.map (fun s -> {state with CharInfo = s; ProgramState = Terminated; Termination = true })
 |> Option.defaultValue state
let mainLoop  =
 Console.CursorVisible <- false
 createMainLoop
   [|updateTick
     updateClock
     RReviveChar
     RupdateEnemyState
     RupdateAttackState
     RUpdateEnemy
     RupdateAttackPost
     RupdateEAttackPost
     RupdateCharState
     RenemyShoot
     RupdateEAttackState
     RgameOver
     |]
     (fun s -> s.ProgramState = Running)
     [|
       lookForESC
       RprocessChar
       lookForEnter
       RprocessAttack 
       RlookForZ 
     |]
     [| displayClock
        RdisplayEAttack
        RDisplayEnemy
        RdisplayChar
        RdisplayLives
        RdisplayAttack
        displayScore|]
    (fun s -> s.redrawScreen)
    (fun s -> {s with redrawScreen=false})
    [| |]


let pause state =
 if state.CharInfo.Lifes > 0 then 
    let a = Menu.pauseM()
    match a with 
    |Continue -> {state with ProgramState = Running}
                
    |SaveGame -> guardar state
                 Console.Clear()
                 displayMessage (widht/2) (height/2) "GameSaved" ConsoleColor.Cyan
                 Thread.Sleep(1000)
                 {state with ProgramState = Running} 
                
    |Exit -> {state with Termination = true}
 else
  state  

let rec loopsito state =
  if state.Termination = false then  
    let statex() =
        state
        |> mainLoop
    let b = statex()
    if state.CharInfo.Lifes <= 0 then
     Console.Clear()
     displayMessage (widht/2) (height/2) "GAME OVER" ConsoleColor.Blue
     Thread.Sleep(2000)
     else
      loopsito (pause b)
  else
    if state.CharInfo.Lifes <= 0 then
     Console.Clear()
     displayMessage (widht/2) (height/2) "GAME OVER" ConsoleColor.Blue
     Thread.Sleep(2000)
let mostrar()=
 Console.CursorVisible <- false
 
 
 loopsito initialState 


        
 Console.CursorVisible <- true
 Console.ForegroundColor <- oldForeground
 Console.Clear()
            

let cargar()=
 Console.CursorVisible <- false
 let f = File.Exists("save.json")
 if f = true then
  loopsito ({cargarDatos() with ProgramState = Running})
 else 
  Console.Clear()
  displayMessage (widht/2) (height/2) "Partida no encontrada, cargando datos por defecto" ConsoleColor.Red
  Thread.Sleep(2000)
  loopsito initialState 
   
    

        
 Console.CursorVisible <- true
 Console.ForegroundColor <- oldForeground
 Console.Clear()


Console.CursorVisible <- true
Console.ForegroundColor <- oldcolor
Console.BackgroundColor <- oldBlack
Console.Clear()   