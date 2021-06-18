namespace FfxivCrafting

open System
open FSharpPlus
open Utils
open Microsoft.FSharp.Reflection

module Game =

    let undefined () = failwith "Not Implemented"

    type Condition =
        | Normal
        | Poor
        | Good
        | Excellent

    type Props = {
        Rlvl: int
        Difficulty: int
        Craftsmanship: int
        Control: int
        InitialCp: int
        InitialDurability: int
        MaxQuality: int
    }

    type State = {
        Cp: int
        Progress: int
        Quality: int
        Durability: int
        FirstStep: bool
        Condition: Condition
        InnerQuiet: int option // スタック数
        WasteNot: int option // 残りターン数
        Veneration: int option // 残りターン数
        GreatStrides: int option // 残りターン数
        Innovation: int option // 残りターン数
        WasteNotII: int option // 残りターン数
        Manipulation: int option // 残りターン数
        MuscleMemory: int option // 残りターン数
        BasicTouch: int option // 残りターン数
    }

    type Action =
        | BasicSynthesis
        | BasicTouch
        | MastersMend
        | HastyTouch
        | RapidSynthesis
        | InnerQuiet
        | WasteNot
        | Veneration
        | StandardTouch
        | GreatStrides
        | Innovation
        | WasteNotII
        | ByregotsBlessing
        | PreciseTouch
        | MuscleMemory
        | CarefulSynthesis
        | PatientTouch
        | Manipulation
        | PrudentTouch
        | Reflect
        | PreparatoryTouch
        | GroundWork
        | DelicateSynthesis
        | IntensiveSynthesis
        member this.UnionCaseName =
            (FSharpValue.GetUnionFields(this, typeof<Action>) |> fst).Name
        static member AllActions = [
            BasicSynthesis
            BasicTouch
            MastersMend
            InnerQuiet
            WasteNot
            Veneration
            StandardTouch
            GreatStrides
            (*
            HastyTouch            
            RapidSynthesis
            
            GreatStrides
            Innovation
            WasteNotII
            ByregotsBlessing
            PreciseTouch
            MuscleMemory
            CarefulSynthesis
            PatientTouch
            Manipulation
            PrudentTouch
            Reflect
            PreparatoryTouch
            GroundWork
            DelicateSynthesis
            IntensiveSynthesis
            *)
        ]

    type RlvlAdjuster = {
        Crafts: int
        Control: int
        Progress: int
        Quality: int
        Durability: int
        Conditions: int
    }

    type LevelMod = {
        Progress: int
        Quality: int
    }

    type Env(props: Props) =

        let random = Random()

        let actionSuccessful probability = random.NextDouble() < probability

        // TODO: 430以外のrlvlを実装する
        let rlvlMap: Map<int, RlvlAdjuster> = [(430, { Crafts = 1866; Control = 1733; Progress = 3943; Quality = 18262; Durability = 80; Conditions = 15 })] |> Map.ofList

        let initialState: State = {
            Cp = props.InitialCp
            Progress = 0
            Quality = 0
            Durability = props.InitialDurability
            FirstStep = true
            Condition = Normal
            InnerQuiet = None
            WasteNot = None
            Veneration = None
            GreatStrides = None
            Innovation = None
            WasteNotII = None
            Manipulation = None
            MuscleMemory = None
            BasicTouch = None
        }

        // TODO: 420以外のclvlを実装する
        let clvl = 420

        let rlvlAdjuster: RlvlAdjuster = Map.find props.Rlvl rlvlMap

        // TODO: clvlがrlvlより高いケースを実装する
        let levelModifier: LevelMod = { Progress = 100; Quality = 100 }

        // TODO: Name of the elements, Muscle Memoryを実装する
        let progressEfficiency (baseAmount: int) (state: State): float =
            let buffs = [
                100.
                if state.Veneration.IsSome then 50. else 0.
                if state.MuscleMemory.IsSome then 100. else 0.
            ]
            (float) baseAmount * List.sum buffs / 10000.

        let qualityEfficiency (baseAmount: int) (state: State): float =
            let buffs = [
                100.
                if state.GreatStrides.IsSome then 100. else 0.
                if state.Innovation.IsSome then 50. else 0.
            ]
            (float) baseAmount * List.sum buffs / 10000.

        // TODO: 高進捗を実装する
        let progressCondition (state: State): float = 1.

        let qualityCondition (state: State): float =
            match state.Condition with
            | Poor -> 0.5
            | Normal -> 1.
            | Good -> 1.5
            | Excellent -> 4.

        let increaseDurability (baseAmount: int) (state: State): State =
            { state with Durability = Math.Min(state.Durability + baseAmount, props.InitialDurability) }

        let durabilityCost (baseAmount: int) (state: State): int =
            if state.WasteNot.IsSome || state.WasteNotII.IsSome then baseAmount / 2 else baseAmount

        let decreaseDurability (baseAmount: int) (state: State): State =
            { state with Durability = max (state.Durability - durabilityCost baseAmount state) 0 }

        let increaseProgress (baseAmount: int) (state: State): State =
            let p1 = (float) props.Craftsmanship * 21. / 100. + 2.
            let p2 = p1 * ((float) props.Craftsmanship + 10000.) / ((float) rlvlAdjuster.Crafts + 10000.)
            let p3 = p2 * (float) levelModifier.Progress / 100.
            let progress = (int) (Math.Floor((Math.Floor(p3) * progressCondition state * progressEfficiency baseAmount state)))
            { state with Progress = Math.Min(state.Progress + progress, props.Difficulty)}

        // TODO: 高能率を実装する
        let actualCp (nominalCp: int): int = nominalCp

        let hasCp (nominalCp: int) (state: State): bool =
            state.Cp >= actualCp nominalCp
        
        let decreaseCp (baseAmount: int) (state: State): State =
            let actual = actualCp baseAmount
            assert (state.Cp - actual >= 0)
            { state with Cp = state.Cp - actual }

        let activateInnerQuiet (state: State): State =
            { state with InnerQuiet = Some 1 }

        let deactivateInnerQuiet (state: State): State =
            { state with InnerQuiet = None }

        let mapInnerQuiet (func: int -> int) (state: State): State =
            { state with InnerQuiet = state.InnerQuiet |> Option.map (fun stacks -> max (min (func stacks) 11) 0) }

        let stackInnerQuietIfActive (count: int) (state: State): State =
            mapInnerQuiet (fun stacks -> stacks + count) state

        let doubleInnerQuietStacksIfActive (state: State): State =
            mapInnerQuiet (fun stacks -> stacks * 2) state

        let halveInnerQuietStacksIfActive (state: State): State =
            mapInnerQuiet (fun stacks -> stacks / 2 + stacks % 2) state

        let activateWasteNot (state: State): State =
            { state with WasteNot = Some 4 }

        let activateVeneration (state: State): State =
            { state with Veneration = Some 4 }

        let activateGreatStrides (state: State): State =
            { state with GreatStrides = Some 3 }

        let deactivateGreatStrides (state: State): State =
            { state with GreatStrides = None }

        let activateInnovation (state: State): State =
            { state with Innovation = Some 4 }

        let activateWasteNotII (state: State): State =
            { state with WasteNotII = Some 8 }

        let activateMuscleMemory (state: State): State =
            { state with MuscleMemory = Some 5 }

        let deactivateMuscleMemory (state: State): State =
            { state with MuscleMemory = None }

        let activateManipulation (state: State): State =
            { state with Manipulation = Some 8 }

        let activateBasicTouchCombo (state: State): State =
            { state with BasicTouch = Some 1 }

        let proceedTransientBuffs = function
            | None -> None
            | Some 1 -> None
            | Some x when x > 1-> Some (x - 1)
            | _ -> invalidArg "steps" "should be None or Some x where x > 1"
            
        // TODO: ターン経過系バフを追加する
        let proceedStep (state: State): State =
            {
                state with
                    Durability = state.Durability + if state.Manipulation.IsSome && state.Durability > 0 then 5 else 0
                    FirstStep = false
                    WasteNot = proceedTransientBuffs state.WasteNot
                    Veneration = proceedTransientBuffs state.Veneration
                    GreatStrides = proceedTransientBuffs state.GreatStrides
                    Innovation = proceedTransientBuffs state.Innovation
                    WasteNotII = proceedTransientBuffs state.WasteNotII
                    Manipulation = proceedTransientBuffs state.Manipulation
                    MuscleMemory = proceedTransientBuffs state.MuscleMemory
                    BasicTouch = proceedTransientBuffs state.BasicTouch
            }

        // 高能率を実装する
        let cpCost (baseAmount: int) (state: State): int = baseAmount

        let quality (baseEfficiency: int) (state: State): int =
            let iqAdjuster =
                state.InnerQuiet
                |> Option.map (fun stacks -> (float) props.Control * (float) (stacks - 1) * 20. / 100.)
                |> Option.defaultValue 0.
                |> (+) ((float) props.Control)
            let q1 = iqAdjuster * 35. / 100. + 35.
            let q2 = q1 * (iqAdjuster + 10000.) / ((float) rlvlAdjuster.Control + 10000.)
            let q3 = q2 * (float) levelModifier.Quality / 100.
            (int) (Math.Floor(Math.Floor(q3 * qualityCondition state) * qualityEfficiency baseEfficiency state))

        let byregotsBlessingQuality (state: State) =
            match state.InnerQuiet with
            | Some stacks -> quality (100 + (stacks - 1) * 20) state
            | None -> invalidArg "state.InnerQuiet" "cannot be None"

        let increaseQuality (baseEfficiency: int) (state: State): State =
            let increase = quality baseEfficiency state
            { state with Quality = min (state.Quality + increase) props.MaxQuality }

        let conditionTransitionMatrix: Map<Condition, (Condition * float) list> = Map.ofList [
            (Excellent, [(Poor, 1.)])
            (Normal, [(Good, 0.11); (Excellent, 0.02); (Normal, 0.87)])
            (Good, [(Good, 0.11); (Excellent, 0.02); (Normal, 0.87)])    
            (Poor, [(Good, 0.11); (Excellent, 0.02); (Normal, 0.87)])    
        ]
        
        (*
        let transitionsGroupedByAction (state: State): (Action * Transition list) list =
            Action.AllActions
            |> List.map (fun action ->
                let transitions =
                    match action with
                    | BasicSynthesis ->
                        [{ State = state |> decreaseDurability 10 |> increaseProgress 120 |> deactivateMuscleMemory |> proceedStep; Probability = 1.; QualityIncrease = 0. }] |> conditionBranching
                    | BasicTouch ->
                        if state.Cp >= cpCost 18 state
                        then [{ State = state |> decreaseCp 18 |> decreaseDurability 10 |> stackInnerQuietIfActive 1 |> deactivateGreatStrides |> proceedStep |> activateBasicTouchCombo; Probability = 1.; QualityIncrease = quality 100 state }] |> conditionBranching
                        else []
                    | MastersMend ->
                        if state.Cp >= cpCost 88 state
                        then [{ State = state |> decreaseCp 88 |> increaseDurability 30 |> proceedStep; Probability = 1.; QualityIncrease = 0. }] |> conditionBranching
                        else []
                    | HastyTouch ->
                        [
                            { State = state |> decreaseDurability 10 |> stackInnerQuietIfActive 1 |> deactivateGreatStrides |> proceedStep; Probability = 0.6; QualityIncrease = quality 100 state }
                            { State = state |> decreaseDurability 10 |> proceedStep; Probability = 0.4; QualityIncrease = 0. }
                        ]
                        |> conditionBranching
                    | RapidSynthesis ->
                        [
                            { State = state |> decreaseDurability 10 |> increaseProgress 500 |> deactivateMuscleMemory |> proceedStep; Probability = 0.5; QualityIncrease = 0. }
                            { State = state |> decreaseDurability 10 |> proceedStep; Probability = 0.5; QualityIncrease = 0. }
                        ]
                        |> conditionBranching
                    | InnerQuiet ->
                        if state.Cp >= cpCost 18 state && state.InnerQuiet.IsNone
                        then [{ State = state |> decreaseCp 18 |> proceedStep |> activateInnerQuiet; Probability = 1.; QualityIncrease = 0. }] |> conditionBranching
                        else []
                    | WasteNot ->
                        if state.Cp >= cpCost 56 state
                        then [{ State = state |> decreaseCp 56 |> proceedStep |> activateWasteNot; Probability = 1.; QualityIncrease = 0. }] |> conditionBranching
                        else []
                    | Veneration ->
                        if state.Cp >= cpCost 18 state
                        then [{ State = state |> decreaseCp 18 |> proceedStep |> activateVeneration; Probability = 1.; QualityIncrease = 0. }] |> conditionBranching
                        else []
                    | StandardTouch ->
                        let baseCpCost = if state.BasicTouch.IsSome then 18 else 32
                        if state.Cp >= cpCost baseCpCost state
                        then [{ State = state |> decreaseCp baseCpCost |> decreaseDurability 10 |> stackInnerQuietIfActive 1 |> deactivateGreatStrides |> proceedStep; Probability = 1.; QualityIncrease = quality 125 state }] |> conditionBranching
                        else []
                    | GreatStrides ->
                        if state.Cp >= cpCost 32 state
                        then [{ State = state |> decreaseCp 32 |> proceedStep |> activateGreatStrides; Probability = 1.; QualityIncrease = 0. }] |> conditionBranching
                        else []
                    | Innovation ->
                        if state.Cp >= cpCost 18 state
                        then [{ State = state |> decreaseCp 18 |> proceedStep |> activateInnovation; Probability = 1.; QualityIncrease = 0. }] |> conditionBranching
                        else []
                    | WasteNotII ->
                        if state.Cp >= cpCost 98 state
                        then [{ State = state |> decreaseCp 98 |> proceedStep |> activateWasteNotII; Probability = 1.; QualityIncrease = 0. }] |> conditionBranching
                        else []
                    | ByregotsBlessing ->
                        if state.InnerQuiet >= Some 2 && state.Cp >= cpCost 24 state
                        then [{ State = state |> decreaseCp 24 |> decreaseDurability 10 |> deactivateInnerQuiet |> deactivateGreatStrides |> proceedStep; Probability = 1.; QualityIncrease = byregotsBlessingQuality state }] |> conditionBranching
                        else []
                    | PreciseTouch ->
                        if List.contains state.Condition [Good; Excellent] && state.Cp >= cpCost 18 state
                        then [{ State = state |> decreaseCp 18 |> decreaseDurability 10 |> stackInnerQuietIfActive 2 |> deactivateGreatStrides |> proceedStep; Probability = 1.; QualityIncrease = quality 150 state }] |> conditionBranching
                        else []
                    | MuscleMemory ->
                        if state.FirstStep && state.Cp >= cpCost 6 state
                        then [{ State = state |> decreaseCp 6 |> decreaseDurability 10 |> increaseProgress 300 |> proceedStep |> activateMuscleMemory; Probability = 1.; QualityIncrease = 0. }] |> conditionBranching
                        else []
                    | CarefulSynthesis ->
                        if state.Cp >= cpCost 7 state
                        then [{ State = state |> decreaseCp 7 |> decreaseDurability 10 |> increaseProgress 150 |> deactivateMuscleMemory |> proceedStep; Probability = 1.; QualityIncrease = 0. }] |> conditionBranching
                        else []
                    | PatientTouch ->
                        if state.Cp >= cpCost 6 state
                        then
                            [
                                { State = state |> decreaseCp 6 |> decreaseDurability 10 |> doubleInnerQuietStacksIfActive |> deactivateGreatStrides |> proceedStep; Probability = 0.5; QualityIncrease = quality 100 state }
                                { State = state |> decreaseCp 6 |> decreaseDurability 10 |> halveInnerQuietStacksIfActive |> proceedStep; Probability = 0.5; QualityIncrease = 0. }
                            ]
                            |> conditionBranching
                        else []
                    | Manipulation ->
                        if state.Cp >= cpCost 96 state
                        then [{ State = state |> decreaseCp 96 |> proceedStep |> activateManipulation; Probability = 1.; QualityIncrease = 0. }] |> conditionBranching
                        else []
                    | PrudentTouch ->
                        if state.WasteNot.IsNone && state.WasteNotII.IsNone && state.Cp >= cpCost 25 state
                        then [{ State = state |> decreaseCp 25 |> decreaseDurability 5 |> stackInnerQuietIfActive 1 |> deactivateGreatStrides |> proceedStep; Probability = 1.; QualityIncrease = quality 100 state }] |> conditionBranching
                        else []
                    | Reflect ->
                        if state.FirstStep && state.Cp >= cpCost 24 state
                        then [{ State = state |> decreaseCp 24 |> decreaseDurability 10 |> proceedStep |> activateInnerQuiet |> stackInnerQuietIfActive 2; Probability = 1.; QualityIncrease = quality 100 state }] |> conditionBranching
                        else []
                    | PreparatoryTouch ->
                        if state.Cp >= cpCost 40 state
                        then [{ State = state |> decreaseCp 40 |> decreaseDurability 20 |> stackInnerQuietIfActive 2 |> deactivateGreatStrides |> proceedStep; Probability = 1.; QualityIncrease = quality 200 state }] |> conditionBranching
                        else []
                    | GroundWork ->
                        if state.Cp >= cpCost 18 state
                        then [{ State = state |> decreaseCp 18 |> decreaseDurability 20 |> increaseProgress (if state.Durability >= durabilityCost 20 state then 300 else 150) |> deactivateMuscleMemory |> proceedStep; Probability = 1.; QualityIncrease = 0. }] |> conditionBranching
                        else []
                    | DelicateSynthesis ->
                        if state.Cp >= cpCost 32 state
                        then [{ State = state |> decreaseCp 32 |> decreaseDurability 10 |> increaseProgress 100 |> deactivateMuscleMemory |> stackInnerQuietIfActive 1 |> deactivateGreatStrides |> proceedStep; Probability = 1.; QualityIncrease = quality 100 state }] |> conditionBranching
                        else []
                    | IntensiveSynthesis ->
                        if List.contains state.Condition [Good; Excellent] && state.Cp >= cpCost 6 state
                        then [{ State = state |> decreaseCp 6 |> decreaseDurability 10 |> increaseProgress 120 |> deactivateMuscleMemory |> proceedStep; Probability = 1.; QualityIncrease = 0. }] |> conditionBranching
                        else []
                (action, transitions))

        let nextStates (state: State) (action: Action): State list =
            transitionsGroupedByAction state
            |> List.find (fun (action', _) -> action' = action)
            |> snd
            |> List.map (fun transition -> transition.State)
        *)

        member _.InitialState = initialState

        member _.GetAvailableActions(state: State): Set<Action> =
            Action.AllActions
            |> List.filter (function
                | BasicSynthesis -> true
                | BasicTouch -> state |> hasCp 18
                | MastersMend -> state |> hasCp 88
                | HastyTouch -> true
                | InnerQuiet -> state |> hasCp 18 && state.InnerQuiet.IsNone
                | WasteNot -> state |> hasCp 56
                | Veneration -> state |> hasCp 18
                | StandardTouch -> 
                    let baseCpCost = if state.BasicTouch.IsSome then 18 else 32
                    state |> hasCp baseCpCost
                | GreatStrides -> state |> hasCp 32
                | _ -> false)
            |> Set.ofList

        member _.TakeAction(state: State, action: Action): State =
            match action with
            | BasicSynthesis ->
                state |> decreaseDurability 10 |> increaseProgress 120 |> deactivateMuscleMemory |> proceedStep
            | BasicTouch ->
                state |> decreaseCp 18 |> decreaseDurability 10 |> increaseQuality 100 |> stackInnerQuietIfActive 1 |> deactivateGreatStrides |> proceedStep |> activateBasicTouchCombo
            | MastersMend ->
                state |> decreaseCp 88 |> increaseDurability 30 |> proceedStep
            | HastyTouch ->
                if actionSuccessful 0.6 then
                    state |> decreaseDurability 10 |> increaseQuality 100 |> stackInnerQuietIfActive 1 |> deactivateGreatStrides |> proceedStep
                else
                    state |> decreaseDurability 10 |> proceedStep
            | InnerQuiet ->
                state |> decreaseCp 18 |> proceedStep |> activateInnerQuiet
            | WasteNot ->
                state |> decreaseCp 56 |> proceedStep |> activateWasteNot
            | Veneration ->
                state |> decreaseCp 18 |> proceedStep |> activateVeneration
            | StandardTouch ->
                let baseCpCost = if state.BasicTouch.IsSome then 18 else 32
                state |> decreaseCp baseCpCost |> decreaseDurability 10 |> increaseQuality 125 |> stackInnerQuietIfActive 1 |> deactivateGreatStrides |> proceedStep
            | GreatStrides ->
                state |> decreaseCp 32 |> proceedStep |> activateGreatStrides
            | _ -> failwith (sprintf "Not available %A" action)

        member _.GetFinalReward(state: State): float option =
            if state.Progress = props.Difficulty then
                Some ((float) state.Quality / (float) props.MaxQuality)
            else if state.Durability = 0 then
                Some 0.
            else
                None

        member _.Props = props