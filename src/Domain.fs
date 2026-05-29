namespace Game

open System.Numerics

type Dir = N | S | E | W
type Vel = Zero | Dir of Dir
type VelS = Map<Dir, Vel>
type VelSMap = Map<int, VelS>

module Dir =
    let opp = function N -> S | S -> N | E -> W | W -> E
    let CW = function N -> E | E -> S | S -> W | W -> N
    let CCW = function N -> W | W -> S | S -> E | E -> N
    let toVecI = function N -> 0, -1 | S -> 0, 1 | E -> 1, 0 | W -> -1, 0
    let toVector2 = function N -> Vector2(0f, -1f) | S -> Vector2(0f, 1f) | E -> Vector2(1f, 0f) | W -> Vector2(-1f, 0f)

module Vel =
    let toVecI = function Dir dir -> Dir.toVecI dir | Zero -> (0, 0)
    let toVector2 = function Dir dir -> Dir.toVector2 dir | Zero -> Vector2(0f, 0f)

module VelS =
    let create (pivot: Dir) (vel1: Vel) (vel2: Vel) (vel3: Vel) (vel4: Vel) =
        Map.ofList [(pivot, vel1); ((Dir.CW pivot), vel2); ((Dir.opp pivot), vel3); ((Dir.CCW pivot), vel4)]
    let createOne vel =
        create N vel vel vel vel
    let zero = createOne Zero
    let toVel (vel: VelS) =
        vel |> Map.fold (fun s dir vel -> if Dir dir = vel then vel else s) Zero
    let toVectorI = toVel >> Vel.toVecI
    let toVector2 = toVel >> Vel.toVector2

module VecI =
    let add (x1, y1) (x2, y2) = x1 + x2, y1 + y2

type Tile = Wall | Empty

type Entity =
    | Tile of Tile
    | Id of int

type NodeInfo = {
    colorVar: int
}
type NodeState = {
    X: int
    Y: int
    Dir: Dir
    IsBite: bool
    IsGlow: bool
}
type NodeInfoMap = Map<int, NodeInfo>
type NodeStateMap = Map<int, NodeState>

type Level = {
    Num: int
    Tiles: Tile[,]
    MapWidth: int
    MapHeight: int
    NodeInfoMap: NodeInfoMap
    PastNodeStateMaps: list<NodeStateMap>
    NodeStateMap: NodeStateMap
    AttemptVelSMap: VelSMap
    ResolveVelSMap: VelSMap
    TempVelSMap: VelSMap
    NextNodeStateMap: NodeStateMap
    Workspace: Entity[,]
    IsWin: bool
}

type Phase =
    | PreAction
    | Rigid
    | Soft
    | Move
    | GlowTransfer

type MenuLevel = {
    Num: int
    Title: string
}

type Menu = {
    ColNum: int
    SelectedIndex: int
    SelectedCol: int
    MenuLevels: MenuLevel array
}

type CurrentState = Menu | Play of Level