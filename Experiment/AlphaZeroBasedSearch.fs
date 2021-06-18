namespace FfxivCrafting

module AlphaZeroBasedSearch =
    type Props = {
        MaxDepth: int
    }

    type Player(props: Props, gameEnv: Game.Env, guide: AlphaZero.Guide) =

        let rec dfs (state: Game.State) (depth: int): float =
            if depth = props.MaxDepth then
                guide.Predict(state)
            else
                match gameEnv.GetFinalReward state with
                | Some x -> x
                | None ->
                    gameEnv.GetAvailableActions(state)
                    |> Set.toSeq
                    |> Seq.map (fun action ->
                        let nextState = gameEnv.TakeAction(state, action)
                        dfs nextState (depth + 1))
                    |> Seq.max

        member _.Think(rootState: Game.State): Game.Action =
            gameEnv.GetAvailableActions(rootState)
            |> Seq.maxBy (fun action ->
                let nextState = gameEnv.TakeAction(rootState, action)
                dfs nextState 1)