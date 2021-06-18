namespace FfxivCrafting

open System
open System.Collections.Generic
open FSharpPlus
open Utils

module MaxUct =

    let choose (random: Random) (items: Set<'T>): 'T =
        let array = items |> Set.toArray
        array.[random.Next(array.Length)]

    type Props = {
        GameEnv: Game.Env
        NumEpisodes: int
        UcbCp: float
    }

    type Node = {
        MaxReward: float
        TotalRewards: float
        VisitCount: int
        TriedActions: Set<Game.Action>
    } with
        static member Zero = { MaxReward = 0.; TotalRewards = 0.; VisitCount = 0; TriedActions = Set.empty }

    type Search(props: Props) =

        let gameEnv = props.GameEnv

        let random = Random()

        let treeNodes: Dictionary<Game.State, Node> = Dictionary()

        let bestAction (c: float) (state: Game.State) (final: bool) (debug: bool): Game.Action =
            let currentNode = treeNodes.[state]
            gameEnv.GetAvailableActions(state)
            |> Set.toSeq
            |> Seq.map (fun action ->
                let nextState = gameEnv.TakeAction(state, action)
                let nextNode = treeNodes.GetOrDefault(nextState, Node.Zero)
                let upperConfidenceBound =
                    if final then
                        nextNode.MaxReward
                    else
                        nextNode.MaxReward + c * Math.Sqrt(2. * Math.Log((float) currentNode.VisitCount) / (float) nextNode.VisitCount)
                (action, upperConfidenceBound))
            |> (fun s ->
                if debug then s |> Seq.toList |> printfn "%A"
                s)
            |> Seq.maxBy snd
            |> fst

        let backup (state: Game.State) (value: float) (updateTriedActions: Set<Game.Action> -> Set<Game.Action>): unit =
            let node = treeNodes.GetOrDefault(state, Node.Zero)
            treeNodes.[state] <-
                { MaxReward = max node.MaxReward value; TotalRewards = node.TotalRewards + value; VisitCount = node.VisitCount + 1; TriedActions = updateTriedActions node.TriedActions }

        let composeBackupCont (state: Game.State) (updateTriedActions: Set<Game.Action> -> Set<Game.Action>) (backupCont: float -> unit) (value: float) =
            backup state value updateTriedActions
            backupCont value

        let rec sampleReward (state: Game.State) (backupCont: float -> unit): unit =
            match gameEnv.GetFinalReward state with
            | Some v -> float v |> composeBackupCont state id backupCont
            | None ->
                let triedActions = (treeNodes.GetOrDefault(state, Node.Zero)).TriedActions
                let availableActions = props.GameEnv.GetAvailableActions(state)
                let untriedActions = availableActions - triedActions
                if Set.isEmpty untriedActions then
                    sampleReward
                        (gameEnv.TakeAction(state, bestAction props.UcbCp state false false))
                        (composeBackupCont state id backupCont)
                else
                    let action = choose random untriedActions
                    sampleReward
                        (gameEnv.TakeAction(state, action))
                        (composeBackupCont state (Set.add action) backupCont)

        let runEpisode () =
            let initialGameState = props.GameEnv.InitialState
            sampleReward initialGameState (fun _ -> ())

        member _.Search(): unit =
            seq { 0 .. props.NumEpisodes }
            |> Seq.iter (fun _ -> runEpisode () |> ignore)

        member _.BestAction(state: Game.State): Game.Action =
            bestAction 0. state true true

        member _.TreeNodes = treeNodes