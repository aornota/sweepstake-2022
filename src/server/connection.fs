module Aornota.Sweepstake2022.Server.Connection

open System

type ConnectionId = | ConnectionId of guid : Guid with
    static member Create () = Guid.NewGuid () |> ConnectionId
