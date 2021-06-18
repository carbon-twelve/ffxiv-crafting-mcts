namespace FfxivCrafting

open System
open System.Diagnostics
open FfxivCrafting
open FfxivCrafting.Game

module Main =
    let runPlainUct () =
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
        let search = PlainUct.Search({
            GameEnv = gameEnv
            NumEpisodes = 1600
            UcbCp = 1. / Math.Sqrt(2.)
        })
        let stopwatch = Stopwatch()
        stopwatch.Start()
        search.Search()
        printfn "Search time: %f" stopwatch.Elapsed.TotalSeconds
        let rootNode = search.TreeNodes.[gameEnv.InitialState]
        printfn "Average quality: %f" (rootNode.TotalRewards / float rootNode.VisitCount * float gameProps.MaxQuality)
        let episode =
            Seq.unfold
                (fun (state: Game.State) ->
                    match gameEnv.GetFinalReward state with
                    | Some _ -> None
                    | None ->
                        let action = search.BestAction(state)
                        let nextState = gameEnv.TakeAction(state, action)
                        Some ((action, nextState), nextState))
                gameEnv.InitialState
            |> Seq.toList
        episode |> List.iter (printfn "%A")
        0

    let runMaxUct () =
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
        let search = MaxUct.Search({
            GameEnv = gameEnv
            NumEpisodes = 1600
            UcbCp = 1. / Math.Sqrt(2.)
        })
        let stopwatch = Stopwatch()
        stopwatch.Start()
        search.Search()
        printfn "Search time: %f" stopwatch.Elapsed.TotalSeconds
        let rootNode = search.TreeNodes.[gameEnv.InitialState]
        printfn "Max quality: %f" (rootNode.MaxReward * float gameProps.MaxQuality)
        let episode =
            Seq.unfold
                (fun (state: Game.State) ->
                    match gameEnv.GetFinalReward state with
                    | Some _ -> None
                    | None ->
                        let action = search.BestAction(state)
                        let nextState = gameEnv.TakeAction(state, action)
                        Some ((action, nextState), nextState))
                gameEnv.InitialState
            |> Seq.toList
        episode |> List.iter (printfn "%A")
        0

    let runAlphaZero () =
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
        let mctsProps = {
            AlphaZero.NumEpisodes = 1600
            AlphaZero.CPuct = 5.
        }
        let learnerProps = {
            AlphaZero.NumIterations = 30000
            AlphaZero.HistorySize = 1000
        }
        let guide = AlphaZero.Guide.Create(gameEnv)
        let learner = AlphaZero.Leaner((fun guide -> AlphaZero.Mcts(mctsProps, gameEnv, guide)), gameEnv, learnerProps)
        let stopwatch = Stopwatch()
        stopwatch.Start()
        let finalGuide =  learner.Learn(guide)
        printfn "Search time: %f" stopwatch.Elapsed.TotalSeconds
        finalGuide.Save("model-large-c.zip")

    let playByAlphaZero () =
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
        let mctsProps = {
            AlphaZero.NumEpisodes = 100000
            AlphaZero.CPuct = 5.
        }
        let guide = AlphaZero.Guide.Load("model-large-c.zip", gameEnv)
        let mcts = AlphaZero.Mcts(mctsProps, gameEnv, guide)
        let player = AlphaZero.Player(mcts)
        Seq.unfold
            (fun (state: Game.State) ->
                match gameEnv.GetFinalReward state with
                | Some _ -> None
                | None ->
                    let action = player.Think(state)
                    let nextState = gameEnv.TakeAction(state, action)
                    printfn "%A" {|  Action = action.UnionCaseName ; Next = nextState |}
                    Some ((action, nextState), nextState))
            gameEnv.InitialState
        |> Seq.toList

    let playByAlphaZeroBasedSearch () =
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
        let searchProps = {
            AlphaZeroBasedSearch.MaxDepth = 4
        }
        let guide = AlphaZero.Guide.Load("model-longrunning.zip", gameEnv)
        let player = AlphaZeroBasedSearch.Player(searchProps, gameEnv, guide)
        Seq.unfold
            (fun (state: Game.State) ->
                match gameEnv.GetFinalReward state with
                | Some _ -> None
                | None ->
                    let action = player.Think(state)
                    let nextState = gameEnv.TakeAction(state, action)
                    printfn "%A" {|  Action = action.UnionCaseName ; Next = nextState |}
                    Some ((action, nextState), nextState))
            gameEnv.InitialState
        |> Seq.toList

    [<EntryPoint>]
    let main argv =
        playByAlphaZero ()
        0