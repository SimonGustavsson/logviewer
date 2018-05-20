module LogReader
    open System.IO

    /// Creates a tuple from two elements.
    let tuple2 (a: 'a) (b: 'b) : 'a * 'b = (a, b)

    let random =
        new System.Random()

    let rand min max =
        random.Next(min, max)

    let getLogPathForProduct logDirectory productName =
        Directory.GetFiles(logDirectory, "*.log")
        |> Array.filter (fun path -> Path.GetFileName(path).StartsWith(productName))
        |> Array.sortBy (fun path -> (new FileInfo(path)).LastWriteTime)
        |> Array.tryHead

    let getProductNames logDirectory =
        Directory.GetFiles(logDirectory, "*.log")
        |> Array.map (fun path ->
            let file = Path.GetFileNameWithoutExtension path
            file.Substring(0, file.IndexOf('-'))
        )

    let rec getNewLines filePath lastReadLine =
       try
           let allLines = File.ReadAllLines filePath |> Array.mapi tuple2

           if allLines.Length = lastReadLine then
               ([||], lastReadLine)
           else if allLines.Length > lastReadLine then
               (allLines.[lastReadLine..], allLines.Length)
           else
                  // Must be a new file?
                  (allLines, allLines.Length)
       with
       | :? FileNotFoundException ->
           // File has been removed
             ([||], 0)
       | ex when ex.HResult = -2147024864 -> // File in use

          // Sleep, then try again
          System.Threading.Thread.Sleep (rand 5 50)

          getNewLines filePath lastReadLine
