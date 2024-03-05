namespace Entities

type Event =
    | APP_PASSIVE_OPEN 
    | APP_ACTIVE_OPEN 
    | APP_SEND 
    | APP_CLOSE 
    | APP_TIMEOUT 
    | RCV_SYN 
    | RCV_ACK 
    | RCV_SYN_ACK
    | RCV_FIN 
    | RCV_FIN_ACK 