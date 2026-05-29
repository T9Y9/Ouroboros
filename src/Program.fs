namespace Game

open Raylib_cs

module Program =
    let menuLevels =
        [|
            { Num = 1; Title = "Clock" }
            { Num = 2; Title = "Walnut" }
            { Num = 3; Title = "Warmup" }
            { Num = 4; Title = "Factory" }
            { Num = 5; Title = "WIP" }
        |]
    
    let mutable menu = {
        ColNum = 5
        SelectedIndex = 0
        SelectedCol = 0
        MenuLevels = menuLevels
    }
    
    let mutable currentState = Menu
    
    [<EntryPoint>]
    let main _ =
        Render.newWindow ()
        // Raylib.ToggleFullscreen()
        while not (Raylib.WindowShouldClose() = CBool(true)) do
            // Render.setWindow ()
            match currentState with
            | Menu ->
                let n = menu |> State.handleSelect currentState
                menu <- fst n
                currentState <- snd n
            | Play lvl ->
                let n = lvl |> State.handlePlay currentState
                match snd n with
                | Menu -> currentState <- Menu
                | Play _ -> currentState <- Play (fst n)
        Raylib.CloseWindow()
        0