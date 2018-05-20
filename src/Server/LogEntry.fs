[<AutoOpen>]
module entry

    type LogEntry = { id: int; type_: string; date: string; message: string }

    let parseLine (line: int * string) =
        let parts = (snd line).Split('\t')
        if parts.Length = 3 then
            Some { id = fst line; type_ = parts.[0]; date = parts.[1]; message = parts.[2] }
        else
            None
