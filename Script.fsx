#load @"C:\Users\Takayuki\Projects\ffxiv-crafting-mcts\references.fsx"

open Microsoft.ML

let mlContext = MLContext()

let (model, shcema) = mlContext.Model.Load("model-large-c.zip")

open FfxivCrafting.AlphaZero

let predictionFunction = mlContext.Model.CreatePredictionEngine<DataPoint, Prediction>(model)

open FfxivCrafting
open FfxivCrafting.Game

let gameProps = {
    Rlvl = 430
    Difficulty = 2000
    Craftsmanship = 2566
    Control = 2455
    InitialCp = 300
    InitialDurability = 30
    MaxQuality = 36254
}
let gameEnv = Game.Env(gameProps)

let state = gameEnv.InitialState |> (fun s -> gameEnv.TakeAction(s, InnerQuiet)) |> (fun s -> gameEnv.TakeAction(s, WasteNot)) 
gameEnv.GetAvailableActions(state)
|> Set.toSeq
|> Seq.map (fun action ->
    let nextState = gameEnv.TakeAction(state, action)
    (action, predictionFunction.Predict(DataPoint.FromSelfPlayHindsight gameEnv { State = nextState; Value = 0. }).Score * single gameProps.MaxQuality))
|> Seq.toList
