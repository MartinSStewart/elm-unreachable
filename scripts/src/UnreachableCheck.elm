module UnreachableCheck exposing (Dependency, DependencyRaw, dependencyPath, elmHome, getDependencyData, handleApplicationElmJson, handleDependencies, handlePackageElmJson, main, parseModule, script)

import Elm.Package
import Elm.Parser
import Elm.Processing
import Elm.Project exposing (Project(..))
import Elm.RawFile
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module
import Elm.Version
import Json.Decode as JD
import List.Extra as List
import Script exposing (Script)
import Script.Directory
import Script.Environment
import Script.File
import Script.Platform
import Time


handleApplicationElmJson : String -> Result String Elm.Project.ApplicationInfo
handleApplicationElmJson elmJsonText =
    case JD.decodeString Elm.Project.decoder elmJsonText of
        Ok (Application elmJson) ->
            Ok elmJson

        Ok (Package _) ->
            Err "Can't handle package elm.json files"

        Err error ->
            Err (JD.errorToString error)


handlePackageElmJson : String -> Result String Elm.Project.PackageInfo
handlePackageElmJson elmJsonText =
    case JD.decodeString Elm.Project.decoder elmJsonText of
        Ok (Application _) ->
            Err "Can't handle application elm.json files"

        Ok (Package elmJson) ->
            Ok elmJson

        Err error ->
            Err (JD.errorToString error)


type alias DependencyRaw =
    { name : Elm.Package.Name
    , version : Elm.Version.Version
    , modules : List Elm.RawFile.RawFile
    }


type alias Dependency =
    { name : Elm.Package.Name
    , version : Elm.Version.Version
    , modules : List File
    }


parseModule : Script.Init -> String -> Script String Elm.RawFile.RawFile
parseModule init path =
    Script.File.readOnly init.userPrivileges path
        |> Script.File.read
        |> Script.thenWith
            (\text ->
                case Elm.Parser.parse text of
                    Ok rawFile ->
                        Script.succeed rawFile

                    Err errors ->
                        Script.fail ("Error parsing " ++ path ++ "\n" ++ String.join "\n" errors)
            )


getDependencyData : Script.Init -> Elm.Project.ApplicationInfo -> ( Elm.Package.Name, Elm.Version.Version ) -> Script String DependencyRaw
getDependencyData init applicationInfo dependency =
    dependencyPath init applicationInfo.elm dependency
        ++ "src/"
        |> Script.Directory.readOnly init.userPrivileges
        |> Script.Directory.listFiles
        |> Script.thenWith (List.map Script.File.read >> Script.sequence)
        |> Script.thenWith
            (List.map (parseModule init)
                >> Script.sequence
                >> Script.map
                    (\rawFiles ->
                        { name = Tuple.first dependency
                        , version = Tuple.second dependency
                        , modules = rawFiles
                        }
                    )
            )


handleDependencies : Script.Init -> Elm.Project.ApplicationInfo -> Script String (List DependencyRaw)
handleDependencies init applicationInfo =
    applicationInfo.depsDirect
        ++ applicationInfo.depsIndirect
        ++ applicationInfo.testDepsDirect
        ++ applicationInfo.testDepsIndirect
        |> Script.collect (getDependencyData init applicationInfo)



--dependencyPath applicationInfo.elm dependency
--    ++ "/elm.json"
--    |> Script.File.readOnly init.userPrivileges
--    |> Script.File.read
--    |> Script.thenWith (\elmJsonText ->
--        case handlePackageElmJson elmJsonText of
--            Ok elmJson ->
--                elmJson.
--
--            Err error ->
--                Script.fail error


dependencyPath : Script.Init -> Elm.Version.Version -> ( Elm.Package.Name, Elm.Version.Version ) -> String
dependencyPath init elmVersion ( name, version ) =
    elmHome init
        ++ Elm.Version.toString elmVersion
        ++ "/packages/"
        ++ Elm.Package.toString name
        ++ "/"
        ++ Elm.Version.toString version
        ++ "/"


script : Script.Init -> Script String ()
script init =
    case init.arguments of
        [ elmJsonPath ] ->
            Script.File.readOnly init.userPrivileges elmJsonPath
                |> Script.File.read
                |> Script.thenWith
                    (\elmJson ->
                        case handleApplicationElmJson elmJson of
                            Ok appInfo ->
                                Script.succeed appInfo

                            Err error ->
                                Script.fail error
                    )
                |> Script.thenWith (handleDependencies init)
                |> Script.map
                    (\dependencies ->
                        let
                            context : Elm.Processing.ProcessContext
                            context =
                                dependencies
                                    |> List.map .modules
                                    |> List.foldl
                                        (\rawFile context -> Elm.Processing.addFile rawFile context)
                                        Elm.Processing.init
                        in
                        List.map
                            (\{ name, version, modules } ->
                                { name = name, version = version, modules = List.map (Elm.Processing.process context) modules }
                            )
                            dependencies
                    )
                |> Script.andThen (Script.printLine "Success!")

        _ ->
            Script.printLine "Please provide exactly one argument that is the path to an elm.json file."


elmHome : Script.Init -> String
elmHome init =
    let
        default =
            case init.platform of
                Script.Platform.Posix _ ->
                    "~/.elm/"

                Script.Platform.Windows ->
                    "%appdata%/elm/"
    in
    Script.Environment.get "ELM_HOME" init.environment |> Maybe.withDefault default


main : Script.Program
main =
    Script.program script
