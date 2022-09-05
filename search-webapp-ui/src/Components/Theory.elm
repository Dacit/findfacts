module Components.Theory exposing
    ( Config, State
    , config, empty, update, view
    )

{-| Component for browsing a theory.


# Types

@docs Config, State


# Component

@docs config, empty, update, view

-}

import DataTypes exposing (..)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (style)
import Material.Elevation as Elevation
import Material.Extra.Code as Code
import Material.LinearProgress as Progress
import Material.List as MList exposing (listConfig, listItemConfig)
import Material.Theme as Theme
import Material.Typography as Typography
import Util exposing (stripTrailingBlanklines, trailingBlanklines)


{-| Opaque config type for theory component.
-}
type Config msg
    = Config (ConfigInternal msg)


type alias ConfigInternal msg =
    { toDetailMsg : String -> msg }


{-| Creates a new config.
-}
config : (String -> msg) -> Config msg
config toDetailMsg =
    Config <| ConfigInternal toDetailMsg


{-| Opaque state type for the theory component.
-}
type State
    = State StateInternal


type alias StateInternal =
    { status : Status
    , session: String
    , theory : String
    }


type Status
    = Searching
    | Error String
    | Value (List ShortBlock)


{-| Creates an initial state.
-}
empty : String -> String -> State
empty session theory =
    State <| StateInternal Searching session theory


{-| Updates the state with results.
-}
update : Result String (ResultList ShortBlock) -> State -> State
update result (State state) =
    case result of
        Ok res ->
            State { state | status = Value <| List.sortBy .startLine res.values }

        Err e ->
            State { state | status = Error e }


{-| Renders the component.
-}
view : State -> Config msg -> Html msg
view (State state) (Config conf) =
    div [ style "display" "inline-block" ]
        (h1 [ Typography.headline4 ] [ text <| state.session ++ "." ++ state.theory ]
            :: (case state.status of
                    Searching ->
                        [ Progress.indeterminateLinearProgress Progress.linearProgressConfig ]

                    Error e ->
                        [ text e ]

                    Value blocks ->
                        [ div [ Elevation.z2, Theme.background, style "padding" "16px 0" ]
                            [ MList.list { listConfig | additionalAttributes = [] } <| List.concatMap (renderBlock conf) blocks ]
                        ]
               )
        )


renderBlock : ConfigInternal msg -> ShortBlock -> List (MList.ListItem msg)
renderBlock conf block =
    let
        src =
            stripTrailingBlanklines block.src

        trailing =
            trailingBlanklines block.src

        trailingBlanks =
            -- last blank line is from block splitting
            List.take (List.length trailing - 1) <| trailing
    in
    MList.listItem
        { listItemConfig
            | additionalAttributes = [ style "height" "100%" ]
            , onClick = Just <| conf.toDetailMsg block.id
        }
        [ Code.block src |> Code.withLineNumbersFrom block.startLine |> Code.view ]
        :: (if List.isEmpty trailingBlanks then
                []

            else
                [ MList.listItem { listItemConfig | disabled = True, additionalAttributes = [ style "height" "100%" ] }
                    [ Code.block
                        (List.take (List.length trailingBlanks - 1) trailingBlanks
                            |> List.intersperse "\n"
                            |> String.concat
                        )
                        |> Code.withLineNumbersFrom (block.startLine + (List.length <| String.lines src))
                        |> Code.view
                    ]
                ]
           )
