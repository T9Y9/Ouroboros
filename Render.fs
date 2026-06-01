namespace Game

open Raylib_cs
open System.Numerics

module Render =
    type AnimatedState = {
        Pos: Vector2
        Vel: Vector2
        Bite: float32
        Glow: float32
        Size: float32
    }
    
    let ScreenHeight = 1200
    let ScreenWidth = 1600
    let TileSize = 120f
    let colorMuted = Color(50, 50, 80, 255)
    let colorWall = Color(180, 180, 185, 255)
    let colorAccent = Color(90, 158, 255, 255)
    let colorSelected = Color(225, 193, 30, 255)
    
    let newWindow () =
        Raylib.SetConfigFlags(ConfigFlags.ResizableWindow)
        Raylib.InitWindow(1600, 1200, "Ouroboros")
        Raylib.SetWindowMinSize(480, 360)
        
        let monitor = Raylib.GetCurrentMonitor()
        let width = Raylib.GetMonitorWidth(monitor)
        let height = Raylib.GetMonitorHeight(monitor)
        let ratio = 3f / 4f
        
        let scale = float32 height / 1200f * ratio

        let w = int (float32 height * ratio * 4f / 3f)
        let h = int (float32 height * ratio)
        
        Raylib.SetWindowSize(w, h)
        Raylib.SetWindowPosition(width / 2 - w / 2, height / 2 - h / 2)
        Raylib.SetTargetFPS(60)
        
        scale
    
    let drawTextCentered (text: string) (cx: int) (y: int) (size: int) (color: Color) s xOff yOff =
        let w = Raylib.MeasureText(text, int (float32 size * s))
        Raylib.DrawText(text, int (xOff + float32 cx * s - float32 w / 2f), int (yOff + s * float32 y), int (float32 size * s), color)
    
    let drawRoundedRect (x: int) (y: int) (w: int) (h: int) (radius: float32) (thick: float32) (color: Color) s xOff yOff =
        let rect = Rectangle(xOff + s * float32 x, yOff + s * float32 y, s * float32 w, s * float32 h)
        Raylib.DrawRectangleRoundedLines(rect, radius, 6, color)
    
    let fillRoundedRect (x: int) (y: int) (w: int) (h: int) (radius: float32) (color: Color) s xOff yOff =
        let rect = Rectangle(xOff + s * float32 x, yOff + s * float32 y, s * float32 w, s * float32 h)
        Raylib.DrawRectangleRounded(rect, radius, 6, color)
    
    let ipColor t (r1, g1, b1, a1) (r2, g2, b2, a2) =
        Color(
            int ((1f - t) * float32 r1 + t * float32 r2),
            int ((1f - t) * float32 g1 + t * float32 g2),
            int ((1f - t) * float32 b1 + t * float32 b2),
            int ((1f - t) * float32 a1 + t * float32 a2)
        )
    let interpolate id t (phase: Phase) (lvl: Level) =
        let ns = lvl.NodeStateMap[id]
        let nns = lvl.NextNodeStateMap[id]
        let d =
            let mov = 0.1f
            if phase = Rigid && VelS.toVel lvl.ResolveVelSMap[id] <> Zero then
                mov * t * VelS.toVector2 lvl.ResolveVelSMap[id]
            elif phase = Soft then
                if VelS.toVel lvl.TempVelSMap[id] <> Zero then
                    if VelS.toVel lvl.ResolveVelSMap[id] = Zero then
                        mov * (1f - t) * Vel.toVector2 (VelS.toVel lvl.TempVelSMap[id])
                    else mov * VelS.toVector2 lvl.TempVelSMap[id]
                else Vector2(0f, 0f)
            elif phase = Move then
                // let tVel = VelS.toVel lvl.TempVelSMap[id]
                let rVel = VelS.toVel lvl.ResolveVelSMap[id]
                // if tVel <> Zero && rVel = Zero then
                //     mov * (1f - t) * Vel.toVector2 tVel
                // elif rVel <> Zero then
                if rVel <> Zero then
                    let attempt = (1f - t) * mov + t
                    Vector2(float32 (nns.X - ns.X), float32 (nns.Y - ns.Y)) * attempt
                else Vector2(0f, 0f)
            else Vector2(0f, 0f)
        {
            Pos = Vector2(float32 ns.X, float32 ns.Y) + d
            Vel = (1f - t) * Dir.toVector2 ns.Dir + t * Dir.toVector2 nns.Dir
            Bite =
                match ns.IsBite, nns.IsBite with
                | true, true -> 1f
                | false, false -> 0f
                | true, false -> if phase = PreAction then t else 1f - t
                | false, true -> if phase = PreAction then 1f - t else t
            Glow =
                match ns.IsGlow, nns.IsGlow with
                | true, true -> 1f
                | false, false -> 0f
                | true, false -> 1f - t
                | false, true -> t
            Size = 0.05f * float32 TileSize + if phase = PreAction then float32 TileSize * 0.05f * (if lvl.NodeStateMap[id].IsGlow then t else 0f) else 0f
        }
    
    let drawLevel t (phase: Phase) s xOff yOff (lvl: Level) =
        Raylib.BeginDrawing()
        Raylib.ClearBackground(Color.RayWhite)
        drawTextCentered "WASD/Arrows to move   Z to undo   R to restart   Q to quit" (ScreenWidth / 2) 50 22 colorMuted s xOff yOff
        let AnimatedStateMap = lvl.NodeStateMap |> Map.map (fun id _ -> lvl |> interpolate id t phase)
        let xOffset = float32 ScreenWidth / 2f - float32 lvl.MapWidth * float32 TileSize / 2f
        let yOffset = float32 ScreenHeight / 2f - float32 lvl.MapHeight * float32 TileSize / 2f
        let offset = Vector2(xOff, yOff) + Vector2(xOffset, yOffset) * s
        // map
        for x in 0 .. lvl.MapWidth-1 do
            for y in 0 .. lvl.MapHeight-1 do
                let rectPos = offset + TileSize * Vector2(float32 x, float32 y) * s
                let rect = Rectangle(rectPos, TileSize * s, TileSize * s)
                let rect2 = Rectangle(rectPos + Vector2(0.025f, 0.025f) * TileSize * s, TileSize * 0.95f * s, TileSize * 0.95f * s)
                match lvl.Tiles[x, y] with
                | Wall ->
                    Raylib.DrawRectangleRounded(rect, 0f, 10, Color(237, 237, 237, 255))
                    Raylib.DrawRectangleRounded(rect2, 0.1f, 10, colorWall)
                | Empty ->
                    Raylib.DrawRectangleRounded(rect, 0f, 10, Color(237, 237, 237, 255))
                    Raylib.DrawRectangleRounded(rect2, 0.1f, 10, Color.RayWhite)
        // nodes
        for id, a in AnimatedStateMap |> Map.toList do
            let EyeColor = Color(30, 30, 30, 255)
            let rectPos = offset + a.Pos * TileSize * s + 0.5f * Vector2(a.Size, a.Size) * s
            let rect = Rectangle(rectPos, (TileSize - a.Size) * s, (TileSize - a.Size) * s)
            Raylib.DrawRectangleRounded(rect, 0.1f, 10, ipColor a.Glow (180 + lvl.NodeInfoMap[id].colorVar, 198 + lvl.NodeInfoMap[id].colorVar, 217 + lvl.NodeInfoMap[id].colorVar, 255) (235, 203, 40, 255))
            let c = offset + (a.Pos * TileSize + Vector2(TileSize / 2f, TileSize / 2f) + a.Vel * 0.3f * TileSize) * s
            let rs = Vector2(c.X - TileSize * a.Vel.Y * 0.25f * s, c.Y + TileSize * a.Vel.X * 0.25f * s)
            Raylib.DrawCircle(int rs.X, int rs.Y, 4f * s, EyeColor)
            let ls = Vector2(c.X + TileSize * a.Vel.Y * 0.25f * s, c.Y - TileSize * a.Vel.X * 0.25f * s)
            Raylib.DrawCircle(int ls.X, int ls.Y, 4f * s, EyeColor)
        // bites
        for id, a in AnimatedStateMap |> Map.toList do
            if a.Bite > 0f then
                let BiteWidth = 0.05f
                let Between = 0.25f
                let BiteSize = 0.12f
                let BiteColor = Color(170, 170, 170, 255)
                let center = offset + TileSize * (a.Pos + Vector2(0.5f, 0.5f) + a.Vel * 0.45f) * s
                let startPos1 =  Vector2(center.X - TileSize * a.Vel.Y * (Between - BiteWidth) * s, center.Y + TileSize * a.Vel.X * (Between - BiteWidth) * s)
                let startPos2 =  Vector2(center.X - TileSize * a.Vel.Y * (Between + BiteWidth) * s, center.Y + TileSize * a.Vel.X * (Between + BiteWidth) * s)
                let endPos = Vector2(center.X + TileSize * (a.Vel.X * BiteSize * a.Bite - a.Vel.Y * Between) * s, center.Y + TileSize * (a.Vel.Y * BiteSize * a.Bite + a.Vel.X * Between) * s)
                Raylib.DrawTriangle(startPos1, startPos2, endPos, BiteColor)
                let startPos1 =  Vector2(center.X + TileSize * a.Vel.Y * (Between - BiteWidth) * s, center.Y - TileSize * a.Vel.X * (Between - BiteWidth) * s)
                let startPos2 =  Vector2(center.X + TileSize * a.Vel.Y * (Between + BiteWidth) * s, center.Y - TileSize * a.Vel.X * (Between + BiteWidth) * s)
                let endPos = Vector2(center.X + TileSize * (a.Vel.X * BiteSize * a.Bite + a.Vel.Y * Between) * s, center.Y + TileSize * (a.Vel.Y * BiteSize * a.Bite - a.Vel.X * Between) * s)
                Raylib.DrawTriangle(startPos2, startPos1, endPos, BiteColor)
        
        if lvl.IsWin then
            drawTextCentered "YOU WIN!" (ScreenWidth / 2) (ScreenHeight / 3) 80 Color.Blue s xOff yOff
        
        Raylib.EndDrawing()
    
    let drawMenu (menu: Menu) s xOff yOff =
        let drawLevelSelect (menu: Menu) =
            let levelCellW = 120
            let levelCellH = 120
            let levelPadX = 24
            let levelPadY = 24
            let gridOrigin (cols: int) (rows: int) =
                let totalW = cols * levelCellW + (cols - 1) * levelPadX
                let totalH = rows * levelCellH + (rows - 1) * levelPadY
                (ScreenWidth - totalW) / 2, (ScreenHeight - totalH) / 2 + 40
            drawTextCentered "OUROBOROS" (ScreenWidth / 2) 120 72 colorAccent s xOff yOff
            let cols  = menu.ColNum
            let rows  = (menu.MenuLevels.Length + cols - 1) / cols
            let ox, oy = gridOrigin cols rows
            drawTextCentered "WASD/Arrows to navigate   Space/Enter to play   Esc to quit" (ScreenWidth / 2) 338 22 colorMuted s xOff yOff
            for i in 0 .. menu.MenuLevels.Length - 1 do
                let menuLvl  = menu.MenuLevels[i]
                let col    = i % cols
                let row    = i / cols
                let cx     = ox + col * (levelCellW + levelPadX)
                let cy     = oy + row * (levelCellH + levelPadY)
                let isHov  = i = menu.SelectedIndex            
                if isHov then
                    drawTextCentered menuLvl.Title (ScreenWidth / 2) 448 24 colorMuted s xOff yOff
                    fillRoundedRect cx cy levelCellW levelCellH 0.25f (Color(225, 193, 30, 255)) s xOff yOff
                else
                    fillRoundedRect cx cy levelCellW levelCellH 0.25f colorWall s xOff yOff
                let numStr = sprintf "%02d" menuLvl.Num
                drawTextCentered numStr (cx + levelCellW / 2) (cy + 42) 36 Color.RayWhite s xOff yOff
        Raylib.BeginDrawing()
        Raylib.ClearBackground(Color.RayWhite)

        drawLevelSelect menu

        Raylib.EndDrawing()