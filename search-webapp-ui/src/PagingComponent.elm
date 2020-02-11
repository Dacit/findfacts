module PagingComponent exposing
    ( Config, State
    , config, empty, encode, decoder, update, view
    , buildFilterQuery
    , isEmpty, samePage
    )

{-| This module controls results pagination.


# Types

@docs Config, State


# Component

@docs config, empty, encode, decoder, update, view


# Special paging functionality

@docs buildFilterQuery


# Helpers

@docs isEmpty, samePage

-}

import Array exposing (Array)
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup exposing (ButtonItem)
import Bootstrap.Grid as Grid
import Bootstrap.Spinner as Spinner
import Entities exposing (ResultShortlist)
import Html exposing (Html, text)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra
import Maybe.Extra
import Query exposing (AbstractFQ, FilterQuery)



-- CONFIG


{-| Opaque config for the paging component.
-}
type Config msg
    = Config (State -> msg)


{-| Opaque state for the paging component.
-}
type State
    = State StateInternal


type alias StateInternal =
    { -- Next cursor is not set when query was executed but result is not there yet
      next : Maybe String
    , totalResults : Int
    , previous : Array String
    }


{-| Creates an initial config for a paging component.
-}
config : (State -> msg) -> Config msg
config toMsg =
    Config toMsg


{-| Creates an initial state for a paging component.
-}
empty : State
empty =
    State (StateInternal Nothing 0 Array.empty)


{-| Encodes the persistent component state as json.
-}
encode : State -> Value
encode (State state) =
    Encode.array Encode.string state.previous


{-| Decodes a json of the persistent component state.
-}
decoder : Decoder State
decoder =
    Decode.array Decode.string
        |> Decode.map (StateInternal Nothing 0)
        |> Decode.map State


{-| Update state with new results.
-}
update : ResultShortlist -> State -> State
update res (State state) =
    State { state | next = Just res.nextCursor, totalResults = res.count }


{-| Render the component.
-}
view : State -> Config msg -> List (Html msg)
view (State state) (Config toMsg) =
    if isEmpty (State state) then
        []

    else
        let
            numPagesRound =
                state.totalResults // pageSize

            numPages =
                if numPagesRound * pageSize < state.totalResults then
                    numPagesRound + 1

                else
                    numPagesRound

            currentPage =
                Array.length state.previous
        in
        [ Grid.row []
            [ Grid.col []
                [ ButtonGroup.buttonGroup []
                    (renderButtons numPages currentPage toMsg state)
                ]
            ]
        ]


{-| Builds a correctly paged filter query from a filter and the paging component.
-}
buildFilterQuery : AbstractFQ -> State -> FilterQuery
buildFilterQuery fq (State state) =
    FilterQuery fq pageSize (state.previous |> Array.toList |> List.Extra.last)


{-| Checks if the paging is empty, i.e. does not have any results to control the page for.

    (PagingComponent.isEmpty PagingComponent.empty) == True
    -- for non-empty res
    (PagingComponent.isEmpty (PagingComponent.update res PagingComponent.empty) == False

-}
isEmpty : State -> Bool
isEmpty (State state) =
    state.totalResults == 0


{-| Checks if two components are on the same page.

    PagingComponent.samePage PagingComponent.empty (PagingComponent.update res PagingComponent.empty) == True

-}
samePage : State -> State -> Bool
samePage (State s1) (State s2) =
    s1.previous == s2.previous


pageSize =
    10


type ButtonState
    = Disabled
    | Enabled StateInternal
    | Waiting
    | Active


renderButton label conf buttonState =
    case buttonState of
        Disabled ->
            ButtonGroup.button [ Button.disabled True, Button.secondary ] [ text label ]

        Waiting ->
            ButtonGroup.button
                [ Button.secondary, Button.disabled True ]
                [ Spinner.spinner [ Spinner.small ] [], text label ]

        Enabled state ->
            ButtonGroup.button [ Button.secondary, Button.onClick (conf (State state)) ] [ text label ]

        Active ->
            ButtonGroup.button [ Button.primary ] [ text label ]


renderButtons numPages currentPage conf state =
    Maybe.Extra.values
        [ -- first
          if currentPage > 0 then
            Just (renderButton "1" conf (Enabled { state | previous = Array.empty, next = Array.get 0 state.previous }))

          else
            Nothing
        , -- ...
          Array.get 2 state.previous
            |> Maybe.map (\_ -> renderButton "..." conf Disabled)
        , -- previous
          Array.get 1 state.previous
            |> Maybe.andThen (\_ -> Array.get (currentPage - 1) state.previous)
            |> Maybe.map
                (\c ->
                    renderButton
                        (if currentPage == 2 then
                            "2"

                         else
                            "<"
                        )
                        conf
                        (Enabled { state | previous = Array.slice 0 (currentPage - 1) state.previous, next = Just c })
                )
        , -- current
          Just (renderButton (String.fromInt (currentPage + 1)) conf Active)

        -- loading/next
        , if currentPage + 1 < numPages then
            let
                symbol =
                    if currentPage + 2 == numPages then
                        String.fromInt numPages

                    else
                        ">"
            in
            case state.next of
                Nothing ->
                    Just (renderButton symbol conf Waiting)

                Just cursor ->
                    Just
                        (renderButton
                            symbol
                            conf
                            (Enabled { state | previous = Array.push cursor state.previous, next = Nothing })
                        )

          else
            Nothing
        , -- ...
          if currentPage + 3 < numPages then
            Just (renderButton "..." conf Disabled)

          else
            Nothing
        , -- last
          if currentPage + 2 < numPages then
            Just (renderButton (String.fromInt numPages) conf Disabled)

          else
            Nothing
        ]
