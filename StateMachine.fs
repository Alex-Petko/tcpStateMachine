module StateMachine 

open Entities

let getNextState state event =
    match state, event with
    | CLOSED, APP_PASSIVE_OPEN -> Some LISTEN
    | CLOSED, APP_ACTIVE_OPEN  -> Some SYN_SENT

    | LISTEN, RCV_SYN   -> Some SYN_RCVD
    | LISTEN, APP_SEND  -> Some SYN_SENT
    | LISTEN, APP_CLOSE -> Some CLOSED

    | SYN_RCVD, APP_CLOSE -> Some FIN_WAIT_1
    | SYN_RCVD, RCV_ACK   -> Some ESTABLISHED

    | SYN_SENT, RCV_SYN     -> Some SYN_RCVD
    | SYN_SENT, RCV_SYN_ACK -> Some ESTABLISHED
    | SYN_SENT, APP_CLOSE   -> Some CLOSED

    | ESTABLISHED, APP_CLOSE -> Some FIN_WAIT_1
    | ESTABLISHED, RCV_FIN   -> Some CLOSE_WAIT

    | FIN_WAIT_1, RCV_FIN     -> Some CLOSING
    | FIN_WAIT_1, RCV_FIN_ACK -> Some TIME_WAIT
    | FIN_WAIT_1, RCV_ACK     -> Some FIN_WAIT_2

    | CLOSING, RCV_ACK -> Some TIME_WAIT

    | FIN_WAIT_2, RCV_FIN -> Some TIME_WAIT

    | TIME_WAIT, APP_TIMEOUT -> Some CLOSED

    | CLOSE_WAIT, APP_CLOSE -> Some LAST_ACK

    | LAST_ACK, RCV_ACK -> Some CLOSED

    | _, _ -> None

