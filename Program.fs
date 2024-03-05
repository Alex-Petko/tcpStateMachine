open System
open Entities
open StateMachine
open StringConvertor

let toEventOptionSeq seq = seq |> Seq.map fromString<Event>

let unwrapOptionSeq seq = seq |> Seq.map Option.get

let execute state eventSeq = 
    let (|EmptySeq|_|) seq = if Seq.isEmpty seq then Some () else None

    let rec next stateOption eventSeq = 
        match stateOption, eventSeq with
        | None, _ -> None
        | _, EmptySeq -> stateOption
        | _, _ -> 
            let nextStateOption = getNextState stateOption.Value (Seq.head eventSeq)
            if nextStateOption.IsSome then 
                next nextStateOption (Seq.tail eventSeq)
            else 
                None

    next (Some state) eventSeq 

let main () =
    // Примеры для проверки
    //let strEventSeq = [| "APP_PASSIVE_OPEN"; "APP_SEND"; "RCV_SYN_ACK" |] // ESTABLISHED
    //let strEventSeq = [| "APP_ACTIVE_OPEN" |] //  SYN_SENT
    //let strEventSeq = [| "RCV_SYN"; "RCV_SYN_ACK"; "APP_CLOSE"; "RCV_FIN_ACK"; "RCV_ACK" |] // ERROR
    //let strEventSeq = [| "APP_ACTIVE_OPEN"; "RCV_SYN_ACK"; "APP_CLOSE"; "RCV_FIN_ACK"; "RCV_ACK" |] // ERROR

    Console.WriteLine("Введите количество событий\nВведите каждое событие с новой строки");
    let strEventSeq =  Console.ReadLine() |> int |> fun n -> Array.init n (fun _ -> Console.ReadLine())

    let eventOptionSeq = strEventSeq |> toEventOptionSeq 

    if Seq.contains None eventOptionSeq then 
        None
    else
        eventOptionSeq |> unwrapOptionSeq |> execute CLOSED 

main () |> fun result -> printf "\n%s" (if result.IsSome then (toString result.Value) else "ERROR")