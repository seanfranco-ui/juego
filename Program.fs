open System
open System.Threading
open FSound.Play
open FSound.IO
open FSound.Signal
open FSound.Utilities

let oldBlack = Console.BackgroundColor
let height = Console.BufferHeight
let widht = Console.BufferWidth
let oldcolor = Console.ForegroundColor

type ProgramState =
 | Running
 | Terminated
type menu = 
 |NewGame
 |LoadGame
 |Exit 
type enemyLife =
 |Alive
 |Dead 
type Direcction =
 |Left
 |Right 
 |Up
 |Down
let RockSpeedScale = 2
let enemyRespawnScale = 3
let enemySpeedScale = 4
type state = {
    ProgramState: ProgramState
    Tick : int
    Clock : int
    redrawScreen : bool
    monsterX : int
    monsterY : int
    rockX : int
    rockY : int
    rockSpeed : int
    menuActive : bool
    menuSelected : int
    enter : bool
    enemyX : int
    enemyY : int 
    attackX : int
    attackY : int
    shoot : bool
    canShoot : bool
    enemyState : enemyLife
    lastEnemyTick : int
    Score : int
    attackD : Direcction
    parry : bool
    canParry : bool
}
let r = new Random()
let initialState = {
 ProgramState = Running
 Tick = -1
 Clock = 0
 redrawScreen = true
 monsterX= widht/2 - 1
 monsterY= height/2 - 1
 rockX = 0
 rockY = 0
 rockSpeed = 10
 menuActive = true
 menuSelected = 0
 enter = false
 enemyX = r.Next(1, widht-1)
 enemyY = r.Next(1, height/2-2)
 attackX = 0
 attackY = 0
 shoot = false
 canShoot = true
 enemyState = Alive
 lastEnemyTick = 0
 Score = 0
 attackD = Left
 parry = false
 canParry = false
 } 

let imprimirMensaje x y (mensaje:String) color =
    Console.SetCursorPosition(x,y)
    Console.ForegroundColor <- color
    mensaje |> Console.Write
let displayMessageRight y (mensaje:String) color =
     let l = mensaje.Length
     let start = widht-l-1
     Console.SetCursorPosition(start,y)
     Console.ForegroundColor <- color
     Console.Write mensaje 


let updateTick state =
   {state with Tick = state.Tick+1}
let updateClock state =
    if state.Tick <> 0 && state.Tick % 40 = 0 then
     {state with Clock = state.Clock+1;redrawScreen=true}
    else 
     state 
let displayScore state = 
    imprimirMensaje 0 0 $"{state.Score}" ConsoleColor.Red   
    state       

let displayClock state =
  displayMessageRight 0  $"{state.Clock}" ConsoleColor.Cyan
  state

     
let processMonster key state =
    match key with
    | ConsoleKey.UpArrow -> Some {state with monsterY =  max 0 (state.monsterY - 1) }
    | ConsoleKey.DownArrow -> Some {state with monsterY =  min (height-1) (state.monsterY + 1)}
    | ConsoleKey.LeftArrow -> Some {state with monsterX =  max 0 (state.monsterX - 1) }
    | ConsoleKey.RightArrow -> Some {state with monsterX =  min (widht-1) (state.monsterX + 1) }
    |_ -> None
    |> Option.map (fun s -> {s with redrawScreen = true})
    |> Option.defaultValue state
let processMenu key state =
    match key with
    |ConsoleKey.DownArrow -> Some {state with menuSelected = min 2 (state.menuSelected + 1) }  
    |ConsoleKey.UpArrow -> Some {state with menuSelected = max 0 (state.menuSelected - 1) }
    |_ -> None
    |> Option.map (fun s -> {s with redrawScreen = true})
    |> Option.defaultValue state
let processAttack key state =
    if state.shoot = true then
     match key with
     | ConsoleKey.UpArrow -> Some {state with attackY =  max 0 (state.monsterY - 1); attackX = state.monsterX; attackD = Up; }
     | ConsoleKey.DownArrow -> Some {state with attackY =  min (height-1) (state.monsterY + 1); attackX = state.monsterX; attackD = Down  }
     | ConsoleKey.LeftArrow -> Some {state with attackY =  state.monsterY; attackX = max 0 (state.monsterX - 1); attackD = Left  }
     | ConsoleKey.RightArrow -> Some {state with attackY =  state.monsterY; attackX = min (widht-1) (state.monsterX + 1); attackD = Right  }
     |_ -> None
     |> Option.map (fun s ->{s with redrawScreen = true; canShoot = false} )
     |> Option.defaultValue state
    else
     state       
let updateAttackPost state =
    let delay x = if state.Tick % 4 = 0 then
                           x
                          else
                           None 
    if state.canShoot = false then
     match state.attackD with
     |Up ->  Some {state with attackY = max 0 (state.attackY-1)}
             |> delay
             
     |Down -> 
             Some {state with attackY = min (height-1)(state.attackY+1)}
             |> delay
            
     |Left -> 
             Some {state with attackX = max 0 (state.attackX-1)}
             |> delay
             
     |Right -> 
             Some {state with attackX = min (widht-2) state.attackX+1}
             |> delay
     |> Option.map (fun s -> {s with redrawScreen = true})
     |> Option.defaultValue state
    else
     state
let ProcessParry state = 
    if state.canParry = true then
     if state.enemyX = state.monsterX + 1 || state.enemyX = state.monsterX - 1 || state.enemyX= state.monsterX && (state.enemyY = state.monsterY + 1 || state.enemyY = state.monsterY - 1) then
      Console.Clear()
      Console.BackgroundColor <- ConsoleColor.White
      Thread.Sleep 200
      {state with enemyState = Dead; lastEnemyTick = state.Tick; Score = state.Score+1; enemyX = 0; enemyY = 0}
     else
      state
    else
     state   
let updateAttackState state =
     if state.attackX = 0 || state.attackX = widht-1 || state.attackY = 0 || state.attackY = height-1 then
      {state with canShoot = true}
     else
      state     
let processRockKey key state =
    match key with
    | ConsoleKey.Enter -> {state with rockY = 0; redrawScreen = true; rockSpeed = 5}
    |_-> state   
let lookForESC key state =
    match key with
    |ConsoleKey.Escape -> {state with ProgramState = Terminated}
    |_ -> state
let lookForEnter key state=
     match key with
     |ConsoleKey.Enter -> {state with enter = true}
     |_ -> {state with enter = false}
let lookForZ key state =
    match key with
     |ConsoleKey.Z -> {state with shoot = true}
     |_ -> {state with shoot = false}     
let lookForX key state =
    match key with
     |ConsoleKey.X -> {state with canParry = true}
     |_ -> {state with canParry = false}     
let processKeyboard state =
 
    if Console.KeyAvailable then
      let k = Console.ReadKey true
      if state.menuActive = false then 
        state
        |> lookForESC k.Key
        |> processMonster k.Key
        |> processRockKey k.Key
        |> processAttack k.Key
        |> lookForZ k.Key
        |> lookForX k.Key
       else
        state
        |> processMenu k.Key
        |> lookForEnter k.Key
    else
     state     
let displayAttack state =
   if state.canShoot = false then
    imprimirMensaje state.attackX state.attackY "*" ConsoleColor.DarkRed
    state
   else
    state 
    
let displayEnemy state = 
   if state.enemyState = Alive then 
    imprimirMensaje state.enemyX state.enemyY "!" ConsoleColor.Red 
    state
   else
    state
let updatePlayerState state =
    if state.enemyX = state.monsterX && state.monsterY = state.enemyY then
     {state with ProgramState = Terminated}
    else
     state 
         
let updateEnemyState state =
  if state.enemyState = Alive then 
    if state.attackX = state.enemyX && state.attackY = state.enemyY then
       {state with enemyState = Dead; lastEnemyTick = state.Tick; Score = state.Score+1; enemyX = 0; enemyY = 0 }
    else
      state
  else
   if state.Tick <> 0  &&  (state.Tick+1 - state.lastEnemyTick) % max 10 (70/(state.Score+1)/5) = 0 then
       {state with enemyState = Alive; enemyX = r.Next(1, widht-1); enemyY = r.Next(1, height/2-2)}
    else
      state  

let updateEnemyPost state = 
   if state.enemyState = Alive then
    if state.Tick <> 0  && state.Tick % max 10 (120/(state.Score+1)/3) = 0 then    
     match state.enemyX with
     |d when d < state.monsterX -> {state with enemyX = min (widht-1) (state.enemyX+1)}
     |y when y > state.monsterX -> {state with enemyX = max 0 (state.enemyX-1)}
     |a when a = state.monsterX -> match state.enemyY with
                                        |d when d > state.monsterY -> {state with enemyY = max 0 (state.enemyY-1)}
                                        |y when y < state.monsterY -> {state with enemyY = min (height-1) (state.enemyY+1)}
                                        |_ -> state
                                           
    else
     state 
   else
    state
     
              
let displayMonster state =
    imprimirMensaje state.monsterX state.monsterY "%" ConsoleColor.Yellow
    state
let displayRock state =
    imprimirMensaje state.rockX state.rockY "*" ConsoleColor.Green
    state
let updateRockPos state=
     if state.Tick <> 0  && state.Tick % max 4 (60-state.rockSpeed) = 0 then
      {state with rockY = min (height-1) (state.rockY + 1 ); redrawScreen = true; rockSpeed = state.rockSpeed + RockSpeedScale }
     else
      state
let displayMenu state = 
    imprimirMensaje (widht/4) (height/3) "New Game" ConsoleColor.Blue
    imprimirMensaje (widht/4) (height/3+1) "Load Game" ConsoleColor.Blue
    imprimirMensaje (widht/4) (height/3+2) "Exit" ConsoleColor.Blue
    match state.menuSelected with
    |0 -> imprimirMensaje (widht/4-1) (height/3) "*" ConsoleColor.Yellow
          state
    |1 -> imprimirMensaje (widht/4-1) (height/3+1) "*" ConsoleColor.Yellow
          state 
    |2 -> imprimirMensaje (widht/4-1) (height/3+2) "*" ConsoleColor.Yellow
          state
    |_-> state     
let redrawScreen state =
    if state.redrawScreen then 
     if state.menuActive = false then 
        Console.Clear()
        Console.BackgroundColor <- oldBlack
        state
        |> displayClock
        |> displayMonster
        |> displayRock
        |> displayEnemy
        |> displayAttack
        |> displayScore
        |> fun s -> {s with redrawScreen=false}
     else
      Console.Clear()
      state
      |> displayMenu
      |> fun s -> {s with redrawScreen=false}   
    else
     state

 
Console.CursorVisible <- false


let rec menuNes state=
    let stateM =
     processKeyboard state 
     |> redrawScreen
    Thread.Sleep 25
    if stateM.enter = true then 
         match stateM.menuSelected with
         |0 -> {stateM with menuActive = false} 
         |1 -> Console.Clear()
               Console.WriteLine"Opcion no implementada cargando nuevo juego"
               {stateM with menuActive = false}
         |2 -> {stateM with ProgramState = Terminated; menuActive = false}
         |_-> {stateM with menuActive = false}
        else
         menuNes stateM   

let rec mainLoop state =
    let newState =
     state
     |> updateTick
     |> processKeyboard
     |> updateClock
     |> ProcessParry
     |> updateEnemyState
     |> updateAttackState
     |> updateRockPos
     |> updateEnemyPost
     |> updateAttackPost
     |> updatePlayerState
     |> redrawScreen 
    match newState.ProgramState with
     |Running ->
      Thread.Sleep 25
      mainLoop newState
     |Terminated -> Console.Clear()
                    imprimirMensaje (widht/2) (height/2) "Juego Terminado" ConsoleColor.Blue
                    Thread.Sleep 1000

if initialState.menuActive = true then
        initialState
        |> menuNes
        |> mainLoop
            

Console.CursorVisible <- true
Console.ForegroundColor <- oldcolor
Console.BackgroundColor <- oldBlack
Console.Clear()   

//tarea obligatoria
// usando la estructura del programa
// hacer un programa de menu estilo nintendo