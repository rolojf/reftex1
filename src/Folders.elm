module Folders exposing (Reporte, all)

import DataSource exposing (DataSource)
import DataSource.Glob as Glob


type alias Reporte =
    { slug : String
    , filePath : String
    }


all : DataSource (List Reporte)
all =
    Glob.succeed Reporte
        |> Glob.match (Glob.literal "public/")
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal "/notas.md")
        |> Glob.captureFilePath
        |> Glob.toDataSource
