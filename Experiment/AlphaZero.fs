namespace FfxivCrafting

open System
open System.Collections.Generic
open FSharpPlus
open Utils
open Game
open ShellProgressBar
open Microsoft.ML
open Microsoft.ML.Data
open Microsoft.ML.Trainers

module AlphaZero =
    let logger = NLog.LogManager.GetCurrentClassLogger()

    let undefined () = failwith "Not implemented"

    type SelfPlayHindsight = {
        State: Game.State
        Value: float
    }

    [<CLIMutable>]
    type DataPoint = {
        [<VectorType(18)>]
        Features: single array
        Label: single
    } with
        static member FromSelfPlayHindsight (gameEnv: Game.Env) (example: SelfPlayHindsight): DataPoint =
            let buffToSingle (buff: int option) (maxValue: int): single =
                match buff with
                | Some x -> single x / single maxValue
                | None -> 0.f
            let features =
                Array.append
                    [|
                        single example.State.Cp / single gameEnv.Props.InitialCp
                        single example.State.Progress / single gameEnv.Props.Difficulty
                        single example.State.Quality / single gameEnv.Props.MaxQuality
                        single example.State.Durability / single gameEnv.Props.InitialDurability
                        if example.State.FirstStep then 1.f else 0.f
                        buffToSingle example.State.InnerQuiet 11
                        buffToSingle example.State.WasteNot 4
                        buffToSingle example.State.Veneration 4
                        buffToSingle example.State.GreatStrides 3
                        buffToSingle example.State.Innovation 4
                        buffToSingle example.State.WasteNotII 8
                        buffToSingle example.State.Manipulation 8
                        buffToSingle example.State.MuscleMemory 3
                        buffToSingle example.State.BasicTouch 1
                    |]
                    (
                        let condition = example.State.Condition
                        [|
                            if condition = Normal then 1.f else 0.f
                            if condition = Poor then 1.f else 0.f
                            if condition = Good then 1.f else 0.f
                            if condition = Excellent then 1.f else 0.f
                        |]
                    )
            { Features = features; Label = single example.Value }
    
    [<CLIMutable>]
    type Prediction = {
        Score: single
    }
    let mlContext = MLContext()
    let pipeline = mlContext.Regression.Trainers.FastTree()

    type Guide
        (gameEnv: Game.Env,
        model: RegressionPredictionTransformer<FastTree.FastTreeRegressionModelParameters>,
        schema: DataViewSchema) =
        let predictionFunction = mlContext.Model.CreatePredictionEngine<DataPoint, Prediction>(model)

        member _.Predict(state: Game.State): float =
            predictionFunction.Predict(DataPoint.FromSelfPlayHindsight gameEnv { State = state; Value = 0. }).Score |> float

        member _.Train(examples: SelfPlayHindsight seq): Guide =
            let data = mlContext.Data.LoadFromEnumerable(examples |> Seq.map (DataPoint.FromSelfPlayHindsight gameEnv))
            let model = pipeline.Fit(data)
            Guide(gameEnv, model, data.Schema)

        member _.Save(filePath: string): unit =
            mlContext.Model.Save(model, schema, filePath)

        static member Create(gameEnv: Game.Env): Guide =
            let data = mlContext.Data.LoadFromEnumerable([DataPoint.FromSelfPlayHindsight gameEnv { State = gameEnv.InitialState; Value = 0. }])
            let model = pipeline.Fit(data)
            Guide(gameEnv, model, data.Schema)

        static member Load(filePath: string, gameEnv: Game.Env): Guide =
            let model, schema = mlContext.Model.Load(filePath)
            Guide(gameEnv, model :?> RegressionPredictionTransformer<FastTree.FastTreeRegressionModelParameters>, schema)

    let choose (random: Random) (items: Set<'T>): 'T =
        let array = items |> Set.toArray
        array.[random.Next(array.Length)]

    type MctsProps = {
        NumEpisodes: int
        CPuct: float
    }

    type Edge = {
        ActionValue: float
        TotalRewards: float
        VisitCount: int
    } with
        static member Zero = { ActionValue = 0.; TotalRewards = 0.; VisitCount = 0 }

    type Mcts(props: MctsProps, gameEnv: Game.Env, guide: Guide) =

        let treeEdges: Dictionary<(Game.State * Game.Action), Edge> = Dictionary()

        let bestAction (c: float) (state: Game.State): Game.Action =
            let availableActions = gameEnv.GetAvailableActions(state)
            let visitCountOfCurrentState =
                availableActions
                |> Set.toSeq
                |> Seq.sumBy (fun action -> treeEdges.GetOrDefault((state, action), Edge.Zero).VisitCount)
            let prior = 1. / float (Set.count availableActions)
            availableActions
            |> Set.toSeq
            |> Seq.map (fun action ->
                let edge = treeEdges.GetOrDefault((state, action), Edge.Zero)
                let selectionScore =
                    edge.ActionValue + c * prior * Math.Sqrt(float visitCountOfCurrentState) / (1. + float edge.VisitCount)
                (action, selectionScore))
            |> Seq.maxBy snd
            |> fst

        let backup (state: Game.State) (action: Game.Action) (value: float): unit =
            let edge = treeEdges.GetOrDefault((state, action), Edge.Zero)
            let visitCount = edge.VisitCount + 1
            let totalRewards = edge.TotalRewards + value
            let actionValue = totalRewards / float visitCount
            treeEdges.[(state, action)] <- { ActionValue = actionValue; TotalRewards = totalRewards; VisitCount = visitCount }

        let composeBackupCont (state: Game.State) (action:  Game.Action) (backupCont: float -> unit) (value: float) =
            backup state action value
            backupCont value

        let isLeaf (state: Game.State): bool =
            gameEnv.GetAvailableActions(state)
            |> Set.forall (fun action -> not (treeEdges.ContainsKey(state, action)))

        let rec sampleReward (state: Game.State) (backupCont: float -> unit): unit =
            match gameEnv.GetFinalReward state with
            | Some v -> backupCont v
            | None ->
                if isLeaf state then
                    gameEnv.GetAvailableActions(state)
                    |> Set.iter (fun action -> treeEdges.[(state, action)] <- Edge.Zero)
                    backupCont (guide.Predict(state))
                else
                    let action = bestAction props.CPuct state
                    let nextState = gameEnv.TakeAction(state, action)
                    sampleReward nextState (composeBackupCont state action backupCont)

        let runEpisode (rootState: Game.State) =
            sampleReward rootState (fun _ -> ())

        member _.Search(rootState: Game.State): unit =
            use progressBar = new ProgressBar(props.NumEpisodes, "Searching...", ProgressBarOptions())
            seq { 0 .. props.NumEpisodes }
            |> Seq.iter (fun _ -> runEpisode rootState |> ignore; progressBar.Tick())

        member _.BestAction(state: Game.State): Game.Action =
            bestAction 0. state

        member _.TreeEdges = treeEdges

    type LearnerProps = {
        NumIterations: int
        HistorySize: int
    }

    type Leaner(mctsFactory: Guide -> Mcts, gameEnv: Game.Env, props: LearnerProps) =

        let runIteration (guide: Guide) (exampleHistory: FixedSizedQueue<SelfPlayHindsight>): Guide * FixedSizedQueue<SelfPlayHindsight> =
            let mcts = mctsFactory guide
            let trajectory =
                Seq.unfold
                    (fun (state: Game.State) ->
                        match gameEnv.GetFinalReward state with
                        | Some x -> None
                        | None ->
                            mcts.Search(state)
                            let action = mcts.BestAction(state)
                            let nextState = gameEnv.TakeAction(state, action)
                            Some(nextState, nextState))
                    gameEnv.InitialState
                |> Seq.toList
            let finalReward =
                match gameEnv.GetFinalReward (List.last trajectory) with
                | Some x -> x
                | None -> failwith "The last element of the trajectory is not a final state"
            logger.Debug(sprintf "Final reward: %f" finalReward)
            trajectory
            |> List.map (fun state -> { State = state; Value = finalReward })
            |> exampleHistory.EnqueueAll
            (guide.Train(exampleHistory.InnerQueue), exampleHistory)

        member _.Learn(initialGuide: Guide): Guide =
            logger.Info("Started learning")
            progress 0 10 props.NumIterations
            |> Seq.fold (fun (guide, history) _ -> runIteration guide history) (initialGuide, FixedSizedQueue(props.HistorySize))
            |> fst
        
    type Player(mcts: Mcts) =
        member _.Think(state: Game.State): Game.Action =
            mcts.Search(state)
            mcts.BestAction(state)