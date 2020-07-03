// Learn more about F# at http://fsharp.org

open System

type Rate = {S:float ; I:float; R:float}

type Compartment = { N:int; initial:int;rate:Rate}

type state = {suspectable:int; infected:int; immune:int}

type change = {time:int;rate:Rate}

let (|Int|_|) str =
   match System.Int32.TryParse(str) with
   | (true,int) when int > 0 -> Some(int)
   | _ -> None

let (|Float|_|) str = 
    match Double.TryParse(str) with
    | (true,v)  when v > 0.0 ->  Some(v) 
    | _ -> None

let appendRate (str:string) lis =
    match str.Split(' ') with
    | [|Int t;  Float s; Float i; Float r|] -> {time = t; rate = {S = s; I = i; R = r}}::lis
    | _ -> printfn "invalid input (input eof character to end input)" 
           lis

let createRate s i r =
    {S = s; I = i; R = r}

let createCompartment (str:string) =
    match str.Split(' ') with
    | [|Int n; Int initial; Float s; Float i; Float r|] -> Some   { N = n; initial = initial; rate = createRate s i r }
    | _ -> None

let maybeCompartment()  =
    printfn "Enter initial number of computers (N) infected number (i) then transition rates from S -> I, R -> S, S-> R"
    printfn "Numbers should be in this format int int float float float"
    match Console.ReadLine() with
    |  null -> None
    |  v ->  (createCompartment v) 
    

let getchangeParameters() =
    let rec getListChangeTimes lis =
        printfn "Enter change rates in this format t , S -> I, R -> S, S-> R"
        printfn "Numbers should be in this format int float float float"
        match Console.ReadLine() with
        | null -> lis |> List.sortBy(fun x -> x.time)
        | v  ->   getListChangeTimes  (appendRate v lis) 
    getListChangeTimes []


let runSimulations changeLis (compart:Compartment) =
    let beginState = {suspectable = compart.N; infected = compart.initial; immune = 0; }

    //handles edge case of change happening at 0
    let beginRate = match changeLis |> List.head with
                    | v when v.time = 0 -> v.rate
                    | _ -> compart.rate
    let rnd = System.Random()

    //the core of the simulation 
    Seq.unfold (fun state ->
        
        let i,spread,nextChange,currentState = state

        let newlyInfected =    (float) currentState.suspectable * rnd.NextDouble() * spread.S  |> int
        let newlyImmunised =   (float) currentState.suspectable * rnd.NextDouble() * spread.I |> int
        let newlyRecovered =   (float) currentState.infected * rnd.NextDouble() * spread.R |> int
        let newState ={ 
          suspectable = currentState.suspectable - newlyInfected;
          infected = newlyInfected + currentState.infected - newlyRecovered;
          immune = newlyImmunised + newlyRecovered + currentState.immune
        }
        let nextChangeTime = match (nextChange |> List.tryHead) with  
                             | Some v -> v.time
                             | _ -> i + 1

        //move but not before checking next change time
        match i with 
        | v when nextChangeTime = i -> Some(newState,((v+1),nextChange.Head.rate,nextChange.Tail,newState))
        | v  -> Some(newState,((v+1),spread,nextChange,newState))

        ) (0,beginRate ,changeLis, beginState)
        


[<EntryPoint>]
let main argv =
    maybeCompartment() //input
    |> Option.map (fun v -> (v, getchangeParameters () )) //input
    |> Option.map (fun (v,changes) -> runSimulations  changes v) //calculations
    |> function //output
    | Some result -> 
       result
       |> Seq.take 1000
       |> Seq.iter(fun x -> printfn "%A" x)
    | None -> printfn "invalid input"
    0