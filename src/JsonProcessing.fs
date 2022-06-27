module JsonProcessing

    open Fable.Core
    
    let deserializeJson(json: string) =
        JS.JSON.parse(json) |> unbox
    
    let serializeJson(o: obj) =
        JS.JSON.stringify(o)
