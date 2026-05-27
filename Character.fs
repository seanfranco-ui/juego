module Game.Char
open Game.Types
open Game.Utils
open Game.Enemy
open System
type CharInfo ={
    Lifes : int
    Shooting : bool
    CanShoot : bool
    X : int
    Y : int
    AttackInfo : attackInfo
    State : LState
    LastTick : int
    AttackTick : int
}
let defaultAttack ={
    AttackX = 0
    AttackY = 0
    AttackDi = Up
}
let InitialChar ={
    Lifes = 3
    Shooting = false
    CanShoot = true
    X = widht/2
    Y = height/2
    AttackInfo = defaultAttack
    State = Alive
    LastTick = 0
    AttackTick = 0
    
}
let reviveChar state  (EnemyInfo: EnemyInfo list) Tick =
    
 if state.State = Dead && (Tick - state.LastTick) >= 40 then
     let rec findX()  = 
      let a = R.Next(0,widht-1)
      if (List.exists (fun (x : EnemyInfo) -> x.X = a) EnemyInfo) = true then
         findX()
      else
            a  
     let rec findY()  = 
      let b = R.Next(0,height-1)
      if (List.exists (fun (x : EnemyInfo) -> x.Y = b) EnemyInfo) = true then
         findY()
        else
         b         
     {state with X = findX(); Y = findY(); State = Alive}      
 else
  state     
let processChar key state =
 if state.State = Alive then
    match key with
    | ConsoleKey.UpArrow -> Some {(state : CharInfo) with Y =  max 0 (state.Y - 1) }
    | ConsoleKey.DownArrow -> Some {state with Y =  min (height-1) (state.Y + 1)}
    | ConsoleKey.LeftArrow -> Some {state with X =  max 0 (state.X - 1) }
    | ConsoleKey.RightArrow -> Some {state with X =  min (widht-1) (state.X + 1) }
    |_ -> None
 else
  Some state   
let updatePlayerState state (EnemyInfo: EnemyInfo list) Tick   =
    if state.State = Alive then
     let a = EnemyInfo
                    |> List.map (fun s -> 
                        if  (state.X = s.X && state.Y = s.Y) || (state.X = s.EAttackX && state.Y = s.EAttackY && s.Shooting = true) then
                            2  
                        else
                            1) 
     let b = a |> List.exists (fun x -> x = 2) 
     if b = true then 
      {state with Lifes = state.Lifes - 1; State = Dead; LastTick = Tick}    
     else
      state 
    else
     state  
let displayAttack state =
   if state.Shooting = true then
    displayMessage state.AttackInfo.AttackX state.AttackInfo.AttackY "*" ConsoleColor.DarkRed 
     
let displayChar state =
   if state.State = Alive then
    displayMessage state.X state.Y "%" ConsoleColor.Yellow  
let displayLives state =
    displayMessage (widht/2) 0 $"{state.Lifes}" ConsoleColor.Yellow  
let processAttack key state =
    if state.CanShoot = true && state.Shooting = false then
     match key with
     | ConsoleKey.UpArrow ->  {state with  AttackInfo.AttackDi = Up }
     | ConsoleKey.DownArrow ->  {state with  AttackInfo.AttackDi = Down  }
     | ConsoleKey.LeftArrow ->  {state with  AttackInfo.AttackDi = Left  }
     | ConsoleKey.RightArrow ->  {state with AttackInfo.AttackDi = Right }
     |_-> state
    else
      state       

let lookForZ key state Tick  =
  if state.CanShoot = true && state.Shooting = false then
    match key with
     |ConsoleKey.Z -> match state.AttackInfo.AttackDi with
                      |Up -> Some {state with AttackInfo.AttackY =  max 0 (state.Y - 1); AttackInfo.AttackX = state.X;  Shooting =true; CanShoot = false; AttackTick = Tick  }
                      |Down -> Some {state with AttackInfo.AttackY =  min (height-1) (state.Y + 1); AttackInfo.AttackX = state.X;  Shooting =true; CanShoot = false; AttackTick = Tick  }
                      |Left -> Some {state with AttackInfo.AttackY =  state.Y; AttackInfo.AttackX = max 0 (state.X - 1);  Shooting =true; CanShoot = false; AttackTick = Tick }
                      |Right -> Some {state with  AttackInfo.AttackY =  state.Y; AttackInfo.AttackX = min (widht-1) (state.X + 1);  Shooting =true; CanShoot =false; AttackTick = Tick }
                      |_-> Some state  
     |_ -> Some state                 
  else
   Some state         
let updateAttackPost state Tick =
    let delay x = if Tick % 4 = 0 then
                           x
                          else
                           None 
    if state.Shooting = true then
     match state.AttackInfo.AttackDi with
     |Up ->  Some {state with AttackInfo.AttackY = max 0 (state.AttackInfo.AttackY-1)}
             |> delay
             
     |Down -> 
             Some {state with AttackInfo.AttackY = min (height-1)(state.AttackInfo.AttackY+1)}
             |> delay
            
     |Left -> 
             Some {state with AttackInfo.AttackX = max 0 (state.AttackInfo.AttackX-1)}
             |> delay
             
     |Right -> 
             Some {state with AttackInfo.AttackX = min (widht-2) state.AttackInfo.AttackX+1}
             |> delay
    else
     Some state

let updateAttackState state Tick = 
   if state.Shooting = true then
     if (Tick - state.AttackTick) = 44 then
      {state with CanShoot = true; Shooting = false}
     else
      state   
    else
     state
let GameOver state = 
 if state.Lifes <= 0 then
  Some state
 else
  None          

