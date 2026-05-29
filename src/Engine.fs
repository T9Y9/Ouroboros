namespace Game

open System
open System.IO

module LevelSt =
    let create num =
        let linesOfMap = File.ReadAllLines $"Levels/{num}-map.txt"
        let linesOfState = File.ReadAllLines $"Levels/{num}-state.txt"
        let height = linesOfMap.Length
        let width = linesOfMap[0].Length
        let tiles = Array2D.create width height Empty
        let mutable nodeInfos = Map.empty
        let mutable nodeStates = Map.empty
        let rnd = Random()
        for i in 0..height - 1 do
            for j in 0..width - 1 do
                match linesOfMap[i][j], linesOfState[i][j] with
                | '#', _ -> tiles[j, i] <- Wall
                | '.', _ -> tiles[j, i] <- Empty
                | d, s ->
                    let newId = nodeInfos.Count
                    let direction = match d with | '^' -> N | 'v' -> S | '>' -> E | '<' -> W | _ -> failwith "not"
                    nodeInfos <- nodeInfos |> Map.add newId { colorVar = rnd.Next(0, 5) }
                    nodeStates <- nodeStates |> Map.add newId { X=j; Y=i; Dir=direction; IsBite=(s = 'B' || s = 'O'); IsGlow=(s = 'G' || s = 'O') }
        let arr = tiles |> Array2D.map (fun tile -> Tile tile)
        for id, ns in nodeStates |> Map.toList do
            arr[ns.X, ns.Y] <- Id id
        {
            Num = num
            Tiles = tiles
            MapWidth = width
            MapHeight = height
            NodeInfoMap = nodeInfos
            PastNodeStateMaps = []
            NodeStateMap = nodeStates
            AttemptVelSMap = Map.empty
            ResolveVelSMap = Map.empty
            TempVelSMap = Map.empty
            NextNodeStateMap = nodeStates
            Workspace = arr
            IsWin = false
        }
    
    let setWorkspace l =
        let arr = l.Tiles |> Array2D.map (fun tile -> Tile tile)
        for id, ns in l.NodeStateMap |> Map.toList do
            arr[ns.X, ns.Y] <- Id id
        { l with Workspace = arr }
    
    let updateNodeStates l =
        { l with NodeStateMap = l.NodeStateMap |> Map.map (fun x y -> l.NextNodeStateMap |> Map.tryFind x |> Option.defaultValue y) }
    
    let private getEntity x y l =
        if x < 0 || x >= l.MapWidth || y < 0 || y >= l.MapHeight then failwith "out"
        else
            match l.Workspace[x, y] with
            | Id id ->
                Id id
            | Tile tile ->
                Tile tile
    
    let private facing startId l =
        let ns = l.NodeStateMap[startId]
        let x, y = VecI.add (ns.X, ns.Y) (Dir.toVecI ns.Dir)
        l |> getEntity x y
    
    let biting startId l =
        let ns = l.NodeStateMap[startId]
        if ns.IsBite then
            let x, y = VecI.add (ns.X, ns.Y) (Dir.toVecI ns.Dir)
            match l |> getEntity x y with
            | Id id -> Some id
            | _ -> failwith "empty bite"
        else None
    
    let private biterOf startId l =
        let ns = l.NodeStateMap[startId]
        let mutable result = None
        for dir in [N; S; E; W] do
            let x, y = VecI.add (ns.X, ns.Y) (Dir.toVecI dir)
            match l |> getEntity x y with
            | Id id ->
                let BitNs = l.NodeStateMap[id]
                if BitNs.IsBite && BitNs.Dir = Dir.opp dir then result <- Some id
            | _ -> ()
        result
    
    let private findHead startId l =
        let rec traverse curId =
                match l |> biting curId with
                | Some id -> if id <> startId then traverse id else None
                | None -> Some curId
        traverse startId
    
    let private getChain startId l =
        let headId = 
            match l |> findHead startId with
            | Some id -> id
            | None -> startId
        let rec gather curId acc =
            match l |> biterOf curId with
            | Some id when id <> headId -> gather id (curId :: acc)
            | _ -> curId :: acc
        gather headId []
    
    let private addAttemptVel id vel l =
        { l with AttemptVelSMap = l.AttemptVelSMap |> Map.add id vel }
    
    let private addResolveVel id vel l =
        { l with ResolveVelSMap = l.ResolveVelSMap |> Map.add id vel }
    
    let private iterMap l = seq {
        for x in 0 .. l.MapWidth - 2 do
            for y in 0 .. l.MapHeight - 1 do
                E, W, (l |> getEntity x y, l |> getEntity (x + 1) y)                    
        for x in 0 .. l.MapWidth - 1 do
            for y in 0 .. l.MapHeight - 2 do
                S, N, (l |> getEntity x y, l |> getEntity x (y + 1))
    }
    
    // pre-action
    let setAttemptVelToZero l =
        { l with AttemptVelSMap = l.NodeStateMap |> Map.map (fun id _ -> VelS.createOne Zero) }
        
    let initAttemptVel inputDir l =
        let mutable l = l |> setAttemptVelToZero
        l <- { l with NextNodeStateMap = l.NodeStateMap }
        for id, ns in l.NodeStateMap |> Map.toList do
            if ns.IsGlow then
                if not (ns.Dir = inputDir || ns.Dir = Dir.opp inputDir) && (l |> findHead id <> None) then
                    l <- { l with NextNodeStateMap = l.NextNodeStateMap |> Map.add id { ns with Dir = inputDir } }
                elif not ns.IsBite && ns.Dir = inputDir then
                    // forward move
                    for id2 in l |> getChain id do
                        let nodeDir = l.NodeStateMap[id2].Dir
                        match l |> biterOf id2 with
                        | Some id3 ->
                            let biterDir = l.NodeStateMap[id3].Dir
                            if biterDir = nodeDir then
                                l <- l |> addAttemptVel id2 (VelS.createOne (Dir nodeDir))
                            elif biterDir = Dir.CW nodeDir then
                                l <- l |> addAttemptVel id2 (VelS.create nodeDir (Dir nodeDir) (Dir nodeDir) (Dir biterDir) (Dir biterDir))
                            elif biterDir = Dir.CCW nodeDir then
                                l <- l |> addAttemptVel id2 (VelS.create nodeDir (Dir nodeDir) (Dir biterDir) (Dir biterDir) (Dir nodeDir))
                        | _ -> l <- l |> addAttemptVel id2 (VelS.createOne (Dir nodeDir))
                else
                    // backward move & ouroboros
                    for id2 in l |> getChain id do
                        l <- l |> addAttemptVel id2 (VelS.createOne (Dir inputDir))
        l
    
    let resolveFacing l =
        let mutable l = l
        for id, node in l.NodeStateMap |> Map.toList do
            match l |> biting id with
            | Some id2 ->
                match l |> facing id2 with
                | Id id3 when id3 = id ->
                    l <- { l with NextNodeStateMap = l.NextNodeStateMap |> Map.add id { l.NodeStateMap[id] with IsBite = false } }
                | _ -> ()
            | _ -> ()
        l
    
    let propagateVel l =
        let mutable l = { l with ResolveVelSMap = l.AttemptVelSMap }
        let mutable newCount = 1
        let propagateAxis dir opp id1 id2 =
            if l.NodeStateMap[id1].Dir <> dir && l.NodeStateMap[id2].Dir <> opp && l.ResolveVelSMap[id1][dir] = Dir dir && l.ResolveVelSMap[id2][opp] = Zero then
                for id3 in l |> getChain id2 do
                    newCount <- 1
                    l <- l |> addResolveVel id3 (VelS.create opp (Dir dir) (Dir dir) (Dir dir) (Dir dir))
        while newCount > 0 do
            newCount <- 0
            for dir, opp, pair in l |> iterMap do
                match pair with
                | Id id1, Id id2 -> propagateAxis dir opp id1 id2; propagateAxis opp dir id2 id1
                | _ -> ()
        l
    
    let rigidResolveVel l =
        let mutable l = l
        let mutable newCount = 1
        let propagateZero id =
            for id3 in l |> getChain id do
                newCount <- 1
                l <- l |> addResolveVel id3 VelS.zero
        while newCount > 0 do
            newCount <- 0
            for dir, opp, pair in l |> iterMap do
                match pair with
                | Id id1, Id id2 ->
                    if l.NodeStateMap[id1].Dir <> dir && l.NodeStateMap[id2].Dir <> opp then
                        if l.ResolveVelSMap[id1][dir] = Dir dir && l.ResolveVelSMap[id2][opp] <> Dir dir then propagateZero id1
                        if l.ResolveVelSMap[id2][opp] = Dir opp && l.ResolveVelSMap[id1][dir] <> Dir opp then propagateZero id2
                | Id id1, Tile Wall ->
                    if l.NodeStateMap[id1].Dir <> dir && l.ResolveVelSMap[id1][dir] = Dir dir then propagateZero id1
                | Tile Wall, Id id2 ->
                    if l.NodeStateMap[id2].Dir <> opp && l.ResolveVelSMap[id2][opp] = Dir opp then propagateZero id2
                | _ -> ()
        l
    
    let reInitAttemptVel l =
        let mutable l = l
        for id, vel in l.AttemptVelSMap |> Map.toList do
            if l.ResolveVelSMap[id] = VelS.zero then l <- l |> addAttemptVel id l.ResolveVelSMap[id]
        l
    
    // action
    let saveTempVel l =
        let mutable l = l
        for id, vel in l.AttemptVelSMap |> Map.toList do
            if l.ResolveVelSMap[id] = VelS.zero then l <- l |> addAttemptVel id l.ResolveVelSMap[id]
        { l with TempVelSMap = l.ResolveVelSMap }
    
    let softResolveVel l =
        let mutable l = l
        let mutable newCount = 0
        let propagateZero id =
            for id3 in l |> getChain id do
                newCount <- 1
                l <- l |> addResolveVel id3 VelS.zero
        for dir, opp, pair in l |> iterMap do
            match pair with
            | Id id1, Id id2 ->
                if l.NodeStateMap[id1].Dir = dir && l.NodeStateMap[id2].Dir = opp then
                    if VelS.toVel l.TempVelSMap[id1] = Dir dir && VelS.toVel l.TempVelSMap[id2] <> Dir dir then propagateZero id1
                    elif VelS.toVel l.TempVelSMap[id2] = Dir opp && VelS.toVel l.TempVelSMap[id1] <> Dir opp then propagateZero id2
                elif l.NodeStateMap[id1].Dir = dir && ((l.TempVelSMap[id1][dir] = Dir dir && l.TempVelSMap[id2][opp] <> Dir dir) || (l.TempVelSMap[id1][dir] <> Dir opp && l.TempVelSMap[id2][opp] = Dir opp)) then
                    l <- { l with NextNodeStateMap = l.NextNodeStateMap |> Map.add id1 { l.NodeStateMap[id1] with IsBite = true } }
                    propagateZero id1; propagateZero id2
                    match l |> biterOf id2 with
                    | Some id3 when id1 <> id3 -> l <- { l with NextNodeStateMap = l.NextNodeStateMap |> Map.add id3 { l.NodeStateMap[id3] with IsBite = false } }
                    | _ -> ()
                elif l.NodeStateMap[id2].Dir = opp && ((l.TempVelSMap[id2][opp] = Dir opp && l.TempVelSMap[id1][dir] <> Dir opp) || (l.TempVelSMap[id2][opp] <> Dir dir && l.TempVelSMap[id1][dir] = Dir dir)) then
                    l <- { l with NextNodeStateMap = l.NextNodeStateMap |> Map.add id2 { l.NodeStateMap[id2] with IsBite = true } }
                    propagateZero id1; propagateZero id2
                    match l |> biterOf id1 with
                    | Some id3 when id2 <> id3 -> l <- {l with NextNodeStateMap = l.NextNodeStateMap |> Map.add id3 { l.NodeStateMap[id3] with IsBite = false } }
                    | _ -> ()
                else
                    if l.TempVelSMap[id1][dir] = Dir dir && l.TempVelSMap[id2][opp] <> Dir dir then propagateZero id1
                    elif l.TempVelSMap[id1][dir] <> Dir opp && l.TempVelSMap[id2][opp] = Dir opp then propagateZero id2
            | Id id1, Tile Wall ->
                if l.NodeStateMap[id1].Dir = dir && l.ResolveVelSMap[id1][dir] = Dir dir then propagateZero id1
            | Tile Wall, Id id2 ->
                if l.NodeStateMap[id2].Dir = opp && l.ResolveVelSMap[id2][opp] = Dir opp then propagateZero id2
            | _ -> ()
        l, newCount
    
    let restoreTempVel l =
        { l with AttemptVelSMap = l.TempVelSMap }
    
    let move l =
        let mutable newCount = 0
        let updateNodeState id (ns: NodeState) =
            let vel = VelS.toVel l.ResolveVelSMap[id]
            let x, y =
                match vel with
                | Dir dir -> newCount <- 1; Dir.toVecI dir
                | _ -> 0, 0
            let movedNS = { ns with X = ns.X + x; Y = ns.Y + y }
            match l |> biting id with
            | Some id2 ->
                match VelS.toVel l.ResolveVelSMap[id2] with
                | Dir dir2 ->
                    match vel with
                    | Dir dir when dir = Dir.CW dir2 || dir = Dir.CCW dir2 -> { movedNS with Dir = dir2}
                    | _ -> movedNS
                | _ -> movedNS
            | None -> movedNS
        { l with NextNodeStateMap = l.NodeStateMap |> Map.map updateNodeState }, newCount
    
    // glow transfer
    let glowTransfer l =
        let mutable l = l
        let mutable newCount = 0
        let mutable temp = Map.empty
        for id, ns in l.NodeStateMap |> Map.toList do
            match l |> findHead id with
            | Some headId ->
                if headId <> id && ns.IsGlow then
                    match l |> biting id with
                    | Some bitingId ->
                        l <- { l with NextNodeStateMap = l.NextNodeStateMap |> Map.add id { l.NodeStateMap[id] with IsGlow = false } }
                        temp <- temp |> Map.add bitingId { l.NodeStateMap[bitingId] with IsGlow = true }
                        newCount <- 1
                    | None -> ()
            | None ->
                if not l.NodeStateMap[id].IsGlow && l |> getChain id |> List.exists (fun id2 -> l.NodeStateMap[id2].IsGlow) then
                    temp <- temp |> Map.add id { l.NodeStateMap[id] with IsGlow = true }
                    newCount <- 1
        l <- { l with NextNodeStateMap = l.NextNodeStateMap |> Map.map ( fun id ns ->
                    match temp |> Map.tryFind id with
                    | Some ns2 -> if ns2.IsGlow then { ns with IsGlow = true } else ns
                    | None -> ns
            ) }
        l, newCount
    
    let saveNodeStateMap l =
        { l with PastNodeStateMaps = l.NodeStateMap :: l.PastNodeStateMaps }
    
    let prevNodeStateMap l =
        match l.PastNodeStateMaps with
        | old :: past -> { l with NodeStateMap = old; PastNodeStateMaps = past }
        | _ -> l
    
    let firstNodeStateMap l =
        match l.PastNodeStateMaps with
        | old :: past ->
            let lastNodeStateMap = l.PastNodeStateMaps |> List.last
            { l with NodeStateMap = lastNodeStateMap; PastNodeStateMaps = [lastNodeStateMap] }
        | _ -> l
    
    let checkWin l =
        if l |> getChain 0 |> List.length = l.NodeInfoMap.Count && not (l.NodeStateMap |> Map.exists (fun _ ns -> ns.IsBite = false)) then
            { l with IsWin = true }
        else l