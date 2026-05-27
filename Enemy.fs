module Game.Enemy
open Game.Utils
open Game.Types
open System

type EnemyInfo ={
    EnemyState : LState
    X : int
    Y : int
    ReadyToAttack : bool
    LastEnemyTick : int
    EAttackX : int
    EAttackY : int
    EAttackD : Direcction
    EAttackTick : int
    Shooting : bool
    CanShoot : bool
}

let initialState ={
    EnemyState = Alive
    X = R.Next(1, widht-1)
    Y = R.Next(1, height-1)
    ReadyToAttack = true
    LastEnemyTick = 0
    EAttackX = -12
    EAttackY = -14
    EAttackD = Up
    EAttackTick = 0
    Shooting = false
    CanShoot = true
}


let IEnemyCompendium = [initialState; initialState]
let displayEnemy state = 
    state |>List.iter (fun state -> if state.EnemyState = Alive then displayMessage state.X state.Y "!" ConsoleColor.Red
    )
let displayEAttack state = 
     state |>List.iter (fun state -> if state.Shooting = true then displayMessage state.EAttackX state.EAttackY "!" ConsoleColor.DarkGreen
    )    
let updateEnemyPost state Tick Score MX MY = 
 state |> List.map (fun state ->if state.EnemyState = Alive then
                                
                                                let a = state.Shooting
                                                if Tick <> 0  && Tick % max 10 (120/(Score+1)/3) = 0 then    
                                                    match state.X with
                                                    |x when x < MX -> {state with X = min (widht-1) (state.X+1); EAttackD = if a =false then Right else state.EAttackD}
                                                    |x when x > MX-> {state with X = max 0 (state.X-1); EAttackD = if a =false then Left else state.EAttackD}
                                                    |x when x = MX -> match state.Y with
                                                                                        |d when d > MY -> {state with Y = max 0 (state.Y-1); EAttackD = if a =false then Up else state.EAttackD}
                                                                                        |y when y < MY -> {state with Y = min (height-1) (state.Y + 1); EAttackD = if a =false then Down else state.EAttackD}
                                                                                        |_ -> state
                                                else    
                                                 state                                    
                                            else                                            
                                               state)
let updateEnemyState state AttackX AttackY Tick Score =
 let mutable Score' = Score
 let Ustate =
    state 
    |> List.map (fun state ->
     if state.EnemyState = Alive then 
        if AttackX-1 <= state.X && state.X <= AttackX + 1  && AttackY-1 <= state.Y && state.Y <= AttackY+1 then
         Score' <- Score+1
         {state with EnemyState = Dead; LastEnemyTick = Tick ; X = 0; Y = 0 }
        else
         state
     else
      if Tick <> 0  &&  (Tick+1 - state.LastEnemyTick) % max 10 (70/(Score+1)/5) = 0 then
         {state with EnemyState = Alive; X = R.Next(1, widht-1); Y = R.Next(1, height/2-2)}
        else
          state
    )
 Ustate,Score'    
let updateEAttackPost state Tick =
 state|> List.map (fun state ->
    let delay x = if Tick % 4 = 0 then
                           x
                          else
                           state
    if state.Shooting = true then
     match state.EAttackD with
     |Up ->   {state with EAttackY = max 0 (state.EAttackY-1)}
             |> delay
             
     |Down -> 
             {state with EAttackY = min (height-1)(state.EAttackY+1)}
             |> delay
            
     |Left -> 
              {state with EAttackX = max 0 (state.EAttackX-1)}
             |> delay
             
     |Right -> 
             {state with EAttackX = min (widht-2) state.EAttackX+1}
             |> delay
    else
     state 
)
let EnemyShoot state Tick =
 state |> List.map (fun state-> 
  if state.Shooting = false && state.CanShoot = true then
   if (Tick % 4 = 0) && (R.Next(0,9)) = 5 then 
     match state.EAttackD with
      |Up ->  {state with EAttackY =  max 0 (state.Y - 1); EAttackX = state.X;  Shooting =true; CanShoot = false; EAttackTick = Tick  }
      |Down ->  {state with EAttackY =  min (height-1) (state.Y + 1); EAttackX = state.X;  Shooting =true; CanShoot = false; EAttackTick = Tick  }
      |Left ->  {state with EAttackY =  state.Y; EAttackX = max 0 (state.X - 1);  Shooting =true; CanShoot = false; EAttackTick = Tick }
      |Right ->  {state with EAttackY =  state.Y; EAttackX = min (widht-1) (state.X + 1);  Shooting =true; CanShoot =false; EAttackTick = Tick }
      |_-> state 
    else
      state
  else
    state                      
)
                
let updateEAttackState state Tick = 
 state|> List.map (fun state -> 
   if state.Shooting = true then
     if (Tick - state.EAttackTick) = 44 then
      {state with CanShoot = true; Shooting = false}
     else
      state   
    else
     state         
     )                                  