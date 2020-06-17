module UnreachableCheck exposing
    ( Dependency
    , DependencyRaw
    , dependencyPath
    , elmHome
    , getDependencyData
    , handleApplicationElmJson
    , handleDependencies
    , handlePackageElmJson
    , main
    , parseModule
    , script
    )

import Elm.Package
import Elm.Parser
import Elm.Processing
import Elm.Project exposing (Project(..))
import Elm.RawFile
import Elm.Syntax.File exposing (File)
import Elm.Version
import Json.Decode as JD
import Parser
import Script exposing (Script, UserPrivileges)
import Script.Directory
import Script.Environment
import Script.File
import Script.Platform


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


parseModule : String -> Script String Elm.RawFile.RawFile
parseModule text =
    case Elm.Parser.parse text of
        Ok rawFile ->
            Script.succeed rawFile

        Err errors ->
            Script.fail ("Error parsing " ++ text ++ "\n" ++ Parser.deadEndsToString errors)


getDependencyData :
    Script.Init
    -> Elm.Project.ApplicationInfo
    -> ( Elm.Package.Name, Elm.Version.Version )
    -> Script String DependencyRaw
getDependencyData init applicationInfo dependency =
    dependencyPath init applicationInfo.elm dependency
        ++ "src/"
        |> getFilesInDirectory init
        |> Script.thenWith
            (List.map parseModule
                >> Script.sequence
                >> Script.map
                    (\rawFiles ->
                        { name = Tuple.first dependency
                        , version = Tuple.second dependency
                        , modules = rawFiles
                        }
                    )
            )


listAllFiles : Script.Directory.Directory permissions -> Script String (List (Script.File.File permissions))
listAllFiles directory =
    Script.map2 (++)
        (Script.Directory.listFiles directory)
        (Script.Directory.listSubdirs directory
            |> Script.thenWith (List.map listAllFiles >> Script.sequence >> Script.map List.concat)
        )


getFilesInDirectory : Script.Init -> String -> Script String (List String)
getFilesInDirectory init directoryPath =
    directoryPath
        |> Script.Directory.readOnly init.userPrivileges
        |> listAllFiles
        |> Script.map
            (List.filterMap
                (\file ->
                    if String.endsWith ".elm" (Script.File.name file) then
                        Just file

                    else
                        Nothing
                )
            )
        |> Script.aside (\files -> List.map Script.File.name files |> String.join ", " |> Script.printLine)
        |> Script.onError (\_ -> Script.fail <| "Failed to read directory " ++ directoryPath)
        |> Script.thenWith
            (List.map
                (\file ->
                    Script.File.read file
                        |> Script.onError (\_ -> Script.fail <| "Failed to read module: " ++ Script.File.name file)
                )
                >> Script.sequence
            )


handleDependencies : Script.Init -> Elm.Project.ApplicationInfo -> Script String (List DependencyRaw)
handleDependencies init applicationInfo =
    applicationInfo.depsDirect
        ++ applicationInfo.depsIndirect
        ++ applicationInfo.testDepsDirect
        ++ applicationInfo.testDepsIndirect
        |> Script.collect (getDependencyData init applicationInfo)


handleUserCode : Script.Init -> Elm.Project.ApplicationInfo -> Script String (List Elm.RawFile.RawFile)
handleUserCode init applicationInfo =
    let
        sourceDirectories =
            applicationInfo.dirs
    in
    sourceDirectories
        |> List.map (getFilesInDirectory init)
        |> Script.sequence
        |> Script.thenWith (List.concat >> List.map parseModule >> Script.sequence)


dependencyPath : Script.Init -> Elm.Version.Version -> ( Elm.Package.Name, Elm.Version.Version ) -> String
dependencyPath init elmVersion ( name, version ) =
    elmHome init
        ++ Elm.Version.toString elmVersion
        ++ "/packages/"
        ++ Elm.Package.toString name
        ++ "/"
        ++ Elm.Version.toString version
        ++ "/"


parseAllModules : ( List Elm.RawFile.RawFile, List DependencyRaw ) -> ( List File, List Dependency )
parseAllModules ( rawModules, rawDependencies ) =
    let
        context : Elm.Processing.ProcessContext
        context =
            rawDependencies
                |> List.concatMap .modules
                |> (++) rawModules
                |> List.foldl Elm.Processing.addFile Elm.Processing.init

        newDependencies : List Dependency
        newDependencies =
            List.map
                (\{ name, version, modules } ->
                    { name = name, version = version, modules = List.map (Elm.Processing.process context) modules }
                )
                rawDependencies

        newModules : List File
        newModules =
            List.map (Elm.Processing.process context) rawModules
    in
    ( newModules, newDependencies )


script : Script.Init -> Script String ()
script init =
    case init.arguments of
        [ elmJsonPath ] ->
            Script.File.readOnly init.userPrivileges elmJsonPath
                |> Script.File.read
                |> Script.onError (\_ -> Script.fail <| "Failed to read " ++ elmJsonPath)
                |> Script.thenWith
                    (\elmJson ->
                        case handleApplicationElmJson elmJson of
                            Ok appInfo ->
                                Script.succeed appInfo

                            Err error ->
                                Script.fail error
                    )
                |> Script.thenWith
                    (\appInfo ->
                        Script.map2 Tuple.pair
                            (handleUserCode init appInfo)
                            (handleDependencies init appInfo)
                    )
                |> Script.map (parseAllModules >> unreachableCheck)
                |> Script.thenWith (\_ -> Script.printLine "Success!")

        _ ->
            Script.printLine "Please provide exactly one argument that is the path to an elm.json file."


unreachableCheck : ( List File, List Dependency ) -> List String
unreachableCheck ( modules, dependencies ) =
    []


elmHome : Script.Init -> String
elmHome init =
    let
        default =
            case init.platform of
                Script.Platform.Posix _ ->
                    "/Users/martinstewart/.elm/"

                Script.Platform.Windows ->
                    "%appdata%/elm/"
    in
    Script.Environment.get "ELM_HOME" init.environment |> Maybe.withDefault default


main : Script.Program
main =
    Script.program script
