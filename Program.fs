namespace Game

open Raylib_cs

module Program =
    let menuLevels =
        [|
            { Num = 1; Title = "Clock" }
            { Num = 2; Title = "Walnut" }
            { Num = 3; Title = "Warmup" }
            { Num = 4; Title = "Factory" }
            { Num = 5; Title = "Untitled" }
        |]
    
    let mutable menu = {
        ColNum = 5
        SelectedIndex = 0
        SelectedCol = 0
        MenuLevels = menuLevels
    }
    
    let mutable current = { State = Menu; Scale = 1f; X = 0f; Y = 0f; W = 0; H = 0 }
    
    [<EntryPoint>]
    let main _ =
        let scale, width, height = Render.newWindow ()
        current <- { current with Scale = scale; W = width; H = height }
        while not (Raylib.WindowShouldClose() = CBool(true)) do
            if Raylib.IsWindowResized() = CBool(true) then
                let width = Raylib.GetScreenWidth()
                let height = Raylib.GetScreenHeight()
                let size = min (width * 3 / 4) height
                current <- { current with Scale = float32 size / 1200f; X = float32 (width / 2) - float32 size * 4f / 3f / 2f; Y = float32 (height / 2 - size / 2); W = width; H = height }
            match current.State with
            | Menu ->
                let n = menu |> State.handleSelect current
                menu <- fst n
                current <- { current with State = snd n }
            | Play lvl ->
                let n = lvl |> State.handlePlay current
                match snd n with
                | Menu -> current <- { current with State = snd n }
                | Play _ -> current <- { current with State = Play (fst n) }
        Raylib.CloseWindow()
        0