module Game.Types

type Command =
| StartGame
| LoadGame
| Exit
| SaveGame
| Continue
type Direcction =
 |Left
 |Right 
 |Up
 |Down
type attackInfo ={
    AttackX : int
    AttackY : int
    AttackDi : Direcction
}
type LState =
| Alive
| Dead

