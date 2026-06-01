namespace Game

open Raylib_cs

module State =
    let isPressed k = Raylib.IsKeyPressed(k) = CBool(true)
    let inputVel () =
        if isPressed KeyboardKey.W || isPressed KeyboardKey.Up then Dir N
        elif isPressed KeyboardKey.S || isPressed KeyboardKey.Down then Dir S
        elif isPressed KeyboardKey.D || isPressed KeyboardKey.Right then Dir E
        elif isPressed KeyboardKey.A || isPressed KeyboardKey.Left then Dir W
        else Zero
    
    let handleSelect (c: Current) (menu: Menu) =
        let mutable menu = menu
        let mutable state = c.State
        let cols     = menu.ColNum
        // let rows     = (menu.MenuLevels.Length + cols - 1) / cols
        let maxIdx   = menu.MenuLevels.Length - 1
        
        let mutable moved = false
        
        match inputVel () with
        | Dir N ->
            let next = menu.SelectedIndex - cols
            if next >= 0 then
                menu <- { menu with SelectedIndex = next }
                moved <- true
        | Dir S ->
            let next = menu.SelectedIndex + cols
            if next <= maxIdx then
                menu <- { menu with SelectedIndex = next }
                moved <- true
        | Dir W ->
            if menu.SelectedIndex % cols > 0 then
                menu <- { menu with SelectedIndex = menu.SelectedIndex - 1 }
                moved <- true
        | Dir E ->
            if menu.SelectedIndex % cols < cols - 1 && menu.SelectedIndex < maxIdx then
                menu <- { menu with SelectedIndex = menu.SelectedIndex + 1 }
                moved <- true
        | _ -> ()
        
        if isPressed KeyboardKey.Enter ||
            isPressed KeyboardKey.Space then
            state <- Play (LevelSt.create menu.MenuLevels[menu.SelectedIndex].Num)
        
        Render.drawMenu menu c.Scale c.X c.Y
        menu, state
    
    let handlePlay (c: Current) (lvl: Level) =
        let mutable lvl = lvl
        let mutable state = c.State
        if isPressed KeyboardKey.R then
            lvl <- lvl |> LevelSt.firstNodeStateMap |> LevelSt.setWorkspace
        if isPressed KeyboardKey.Q then
            state <- Menu
        if isPressed KeyboardKey.Z then
            lvl <- lvl |> LevelSt.prevNodeStateMap |> LevelSt.setWorkspace
        let interpolate (f, step, t, n) phase lvl =
            for t in f .. step .. t do
                lvl |> Render.drawLevel (float32 t / float32 n) phase c.Scale c.X c.Y
            lvl
        match inputVel () with
        | Dir inputDir ->
            lvl <- lvl
            |> LevelSt.saveNodeStateMap
            |> LevelSt.initAttemptVel inputDir
            |> interpolate (0, 1, 8, 8) PreAction
            |> LevelSt.updateNodeStates |> LevelSt.setWorkspace
            |> LevelSt.resolveFacing
            |> interpolate (8, -1, 0, 8) PreAction
            |> LevelSt.updateNodeStates |> LevelSt.setWorkspace
            
            lvl <- lvl |> LevelSt.propagateVel
            
            lvl <- lvl
            |> LevelSt.rigidResolveVel
            |> LevelSt.reInitAttemptVel
            |> LevelSt.propagateVel
            if lvl.ResolveVelSMap |> Map.exists (fun _ vel -> vel <> VelS.createOne Zero) then
                lvl <- lvl |> interpolate (1, 1, 4, 4) Rigid |> LevelSt.updateNodeStates |> LevelSt.setWorkspace
            
            while lvl |> LevelSt.saveTempVel |> LevelSt.softResolveVel |> snd > 0 do
                lvl <- lvl
                |> LevelSt.saveTempVel |> LevelSt.softResolveVel |> fst
                |> LevelSt.rigidResolveVel
                |> LevelSt.reInitAttemptVel
                |> LevelSt.propagateVel
                |> interpolate (1, 1, 4, 4) Soft
                |> LevelSt.updateNodeStates |> LevelSt.setWorkspace
            
            if lvl |> LevelSt.move |> snd > 0 then
                lvl <- lvl
                |> LevelSt.move |> fst
                |> interpolate (1, 1, 8, 8) Move
                |> LevelSt.updateNodeStates |> LevelSt.setWorkspace
            
            while lvl |> LevelSt.glowTransfer |> snd > 0 do
                lvl <- lvl
                |> LevelSt.glowTransfer |> fst
                |> interpolate (1, 1, 4, 4) GlowTransfer
                |> LevelSt.updateNodeStates |> LevelSt.setWorkspace
            
            lvl <- lvl |> LevelSt.checkWin
        | Zero ->
            lvl <- lvl |> LevelSt.setAttemptVelToZero
            lvl |> Render.drawLevel 0f GlowTransfer c.Scale c.X c.Y
        
        lvl, state