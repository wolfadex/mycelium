module Main exposing (main)

import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Result.Extra
import String


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = Projects (Request (List Project))
    | CreateProject NewProject
    | WorkOnProject Project


type alias NewProject =
    { path : Maybe Path
    , possiblePaths : Request (List Path)
    , name : String
    }


type alias Project =
    { path : Path
    , favorited : Favorited
    }


decodeProject : Decoder Project
decodeProject =
    Json.Decode.map2 Project
        (Json.Decode.field "path" Json.Decode.string)
        (Json.Decode.field "favorited" decodeFavorite)


type Favorited
    = Favorite
    | NotFavorite


decodeFavorite : Decoder Favorited
decodeFavorite =
    Json.Decode.bool
        |> Json.Decode.map
            (\fav ->
                if fav then
                    Favorite

                else
                    NotFavorite
            )


type alias Path =
    String


type Request a
    = Loading
    | Success a
    | Failure Http.Error


mapRequest : Result Http.Error a -> Request a
mapRequest =
    Result.map Success >> Result.mapError Failure >> Result.Extra.merge


decodeProjects : Decoder (List Project)
decodeProjects =
    Json.Decode.list decodeProject


init : () -> ( Model, Cmd Msg )
init () =
    ( Projects Loading, getProjects )


getProjects : Cmd Msg
getProjects =
    Http.get
        { url = "/api/projects"
        , expect = Http.expectJson GotProjects decodeProjects
        }


getFolderList : Maybe Path -> Cmd Msg
getFolderList maybePath =
    Http.get
        { url =
            "/api/listfolders"
                ++ (case maybePath of
                        Just path ->
                            "?path=" ++ path

                        Nothing ->
                            ""
                   )
        , expect = Http.expectJson GotFolderPaths decodeFolderPaths
        }


createProject : NewProject -> Cmd Msg
createProject project =
    Http.post
        { url = "/api/projects"
        , body =
            [ ( "newProjectName", Json.Encode.string project.name )
            , ( "newProjectPath"
              , project.path
                    |> Maybe.withDefault ""
                    |> Json.Encode.string
              )
            ]
                |> Json.Encode.object
                |> Http.jsonBody
        , expect = Http.expectJson ProjectCreated decodeProject
        }


decodeFolderPaths : Decoder (List Path)
decodeFolderPaths =
    Json.Decode.list Json.Decode.string


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = GotProjects (Result Http.Error (List Project))
    | ProjectSelected Project
    | NewProjectRequested
    | GoToMainPage
    | SetNewProjectName String
    | GotFolderPaths (Result Http.Error (List Path))
    | SetPathForNewProject Path
    | GetChildPaths Path
    | CreateProjectDir
    | ProjectCreated (Result Http.Error Project)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotProjects response, Projects _ ) ->
            ( Projects (mapRequest response), Cmd.none )

        ( ProjectSelected project, _ ) ->
            ( WorkOnProject project, Cmd.none )

        ( NewProjectRequested, _ ) ->
            ( CreateProject
                { path = Nothing
                , possiblePaths = Loading
                , name = ""
                }
            , getFolderList Nothing
            )

        ( GoToMainPage, _ ) ->
            ( Projects Loading, getProjects )

        ( SetNewProjectName name, CreateProject project ) ->
            ( CreateProject { project | name = name }
            , Cmd.none
            )

        ( GotFolderPaths response, CreateProject project ) ->
            ( CreateProject { project | possiblePaths = mapRequest response }
            , Cmd.none
            )

        ( SetPathForNewProject path, CreateProject project ) ->
            ( CreateProject { project | path = Just path }
            , Cmd.none
            )

        ( GetChildPaths path, CreateProject project ) ->
            ( CreateProject { project | possiblePaths = Loading }
            , getFolderList (Just path)
            )

        ( CreateProjectDir, CreateProject project ) ->
            ( model, createProject project )

        _ ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Mycelium"
    , body = [ layout [ width fill, height fill ] <| viewBody model ]
    }


viewBody : Model -> Element Msg
viewBody model =
    column
        [ width fill
        , height fill
        ]
        [ el
            [ width fill
            , Background.color elmBlue
            , Font.color elmWhite
            ]
          <|
            Input.button [ Font.size 40, centerX, padding 16 ]
                { label = text "Mycelium"
                , onPress = Just GoToMainPage
                }
        , case model of
            Projects projects ->
                viewProjects projects

            CreateProject project ->
                viewCreateProject project

            WorkOnProject project ->
                text "working!"
        ]


viewCreateProject : NewProject -> Element Msg
viewCreateProject project =
    column
        [ centerX
        , spacing 16
        ]
        [ el [ centerX, Font.size 24 ] (text "Create a New Elm Project")
        , Input.text
            []
            { label = Input.labelLeft [] (text "Name")
            , text = project.name
            , onChange = SetNewProjectName
            , placeholder = Nothing
            }
        , row
            [ spacing 8 ]
            [ text "Path"
            , case project.possiblePaths of
                Loading ->
                    text "Loading paths..."

                Failure _ ->
                    text "Failed to load paths"

                Success [] ->
                    text "Not folders to choose from"

                Success paths ->
                    List.map (viewPath project.path) paths
                        |> column
                            [ spacing 8
                            , width fill
                            , scrollbarY
                            ]
                        |> el
                            [ height (fill |> maximum 400)
                            , width fill
                            , clipY
                            ]
            ]
        , let
            isDisabled =
                case project.path of
                    Nothing ->
                        True

                    Just _ ->
                        String.isEmpty project.name
          in
          Input.button
            [ Background.color
                (if isDisabled then
                    elmGray

                 else
                    elmBlue
                )
            , paddingXY 8 32
            ]
            { label = text "Create Project"
            , onPress =
                if isDisabled then
                    Nothing

                else
                    Just CreateProjectDir
            }
        ]


viewPath : Maybe Path -> Path -> Element Msg
viewPath selectPath path =
    row [ spacing 8 ]
        [ Input.button
            [ padding 16
            , Background.color <|
                case selectPath of
                    Nothing ->
                        elmBlue

                    Just p ->
                        if p == path then
                            elmPurple

                        else
                            elmBlue
            , Font.color elmWhite
            ]
            { label = text path
            , onPress = Just (SetPathForNewProject path)
            }
        , Input.button
            [ padding 16
            , Background.color elmBlue
            , Font.color elmWhite
            ]
            { label = text "->"
            , onPress = Just (GetChildPaths path)
            }
        ]


viewProjects : Request (List Project) -> Element Msg
viewProjects projects =
    column
        [ width fill
        , height fill
        ]
        [ el
            [ width fill
            , Background.color elmBlue
            , Font.color elmWhite
            ]
            (el [ centerX, padding 16 ] (text "Projects"))
        , case projects of
            Loading ->
                el [ centerX ] (text "Loading...")

            Failure err ->
                el [ centerX ] (text "Failed to load projects")

            Success projs ->
                case projs of
                    [] ->
                        Input.button [ centerX, padding 64 ]
                            { label = text "Create Your First Project"
                            , onPress = Just NewProjectRequested
                            }

                    _ ->
                        column [ width fill, height fill, paddingXY 16 32 ] <|
                            List.map viewProject projs
        ]


viewProject : Project -> Element Msg
viewProject project =
    Input.button
        [ width fill ]
        { label =
            row
                [ width fill ]
                [ text "Open"
                , text project.path
                , (case project.favorited of
                    Favorite ->
                        "F"

                    NotFavorite ->
                        "UF"
                  )
                    |> text
                    |> el [ alignRight ]
                ]
        , onPress = Just (ProjectSelected project)
        }


elmBlue : Color
elmBlue =
    rgb 0.071 0.576 0.847


elmWhite : Color
elmWhite =
    rgb 1 1 1


elmPurple : Color
elmPurple =
    rgb 0.537 0.349 0.659


elmGray : Color
elmGray =
    rgb 0.4 0.4 0.4
