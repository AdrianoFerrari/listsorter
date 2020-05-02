module Main exposing (..)

import Browser
import DnDList
import Html exposing (..)
import Html.Attributes as A exposing (value)
import Html.Events exposing (onClick)
import List.Extra as ListExtra
import List.Split exposing (..)


testData =
    List.range 1 23 |> List.map String.fromInt |> String.join "\n"


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( DataInput { field = testData, error = Nothing }, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Drag and Drop Config


config : DnDList.Config String
config =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Free
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }


system : DnDList.System String Msg
system =
    DnDList.create config DnDMsg


ghostView : DnDList.Model -> List String -> Html.Html Msg
ghostView dnd items =
    let
        maybeDragItem : Maybe String
        maybeDragItem =
            system.info dnd
                |> Maybe.andThen (\{ dragIndex } -> items |> List.drop dragIndex |> List.head)
    in
    case maybeDragItem of
        Just item ->
            Html.div
                (system.ghostStyles dnd)
                [ Html.text item ]

        Nothing ->
            Html.text ""



-- MODEL


type Model
    = DataInput { field : String, error : Maybe String }
    | PresortStep { sorted : List (List String), unsorted : List (List String), dnd : DnDList.Model }
    | MergeStep { merged : List (List String), unmerged : List ( List String, List String, List String ) }
    | Complete (List String)



-- UPDATE


type Msg
    = ChangeData String
    | SubmitData
    | DnDMsg DnDList.Msg
    | SubmitPresort


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( DataInput dataModel, ChangeData newField ) ->
            ( DataInput { dataModel | field = newField }, Cmd.none )

        ( DataInput { field }, SubmitData ) ->
            let
                items =
                    field
                        |> String.trim
                        |> String.split "\n"
            in
            if List.length items < 5 then
                ( DataInput { field = field, error = Just "5 items minimum" }, Cmd.none )

            else
                let
                    chunked =
                        chunksOfLeft 5 items

                    oddChunked =
                        case modBy 5 <| List.length items of
                            1 ->
                                items
                                    |> List.reverse
                                    |> ListExtra.splitAt 6
                                    |> (\( end, begin ) -> chunksOfLeft 3 end ++ chunksOfLeft 5 begin)
                                    |> List.reverse

                            2 ->
                                items
                                    |> List.reverse
                                    |> ListExtra.splitAt 7
                                    |> (\( end, begin ) -> (chunksOfRight 4 end |> List.reverse) ++ chunksOfLeft 5 begin)
                                    |> List.map List.reverse
                                    |> List.reverse

                            3 ->
                                items
                                    |> List.reverse
                                    |> ListExtra.splitAt 8
                                    |> (\( end, begin ) -> chunksOfLeft 4 end ++ chunksOfLeft 5 begin)
                                    |> List.map List.reverse
                                    |> List.reverse

                            _ ->
                                chunked
                in
                ( PresortStep { sorted = [], unsorted = oddChunked, dnd = system.model }, Cmd.none )

        ( PresortStep ({ unsorted, dnd } as presortModel), DnDMsg dndMsg ) ->
            case unsorted of
                firstUnsorted :: restUnsorted ->
                    let
                        ( newDnd, newUnsorted ) =
                            system.update dndMsg dnd firstUnsorted
                    in
                    ( PresortStep { presortModel | dnd = newDnd, unsorted = newUnsorted :: restUnsorted }
                    , system.commands dnd
                    )

                [] ->
                    Debug.todo "Shouldn't happen"

        ( PresortStep ({ sorted, unsorted } as presortModel), SubmitPresort ) ->
            case ( sorted, unsorted ) of
                ( _, onlyUnsorted :: [] ) ->
                    ( MergeStep { merged = [], unmerged = pairForMerging unsorted }, Cmd.none )

                ( _, firstUnsorted :: restUnsorted ) ->
                    ( PresortStep { presortModel | sorted = sorted ++ [ firstUnsorted ], unsorted = restUnsorted }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


pairForMerging : List (List String) -> List ( List String, List String, List String )
pairForMerging sortedLists =
    ListExtra.zip sortedLists (List.drop 1 sortedLists)
        |> List.map (\( l, r ) -> ( l, r, [] ))



-- VIEW


itemView : DnDList.Model -> Int -> String -> Html Msg
itemView dnd index item =
    let
        itemId =
            "id-" ++ item
    in
    case system.info dnd of
        Just { dragIndex } ->
            if dragIndex == index then
                li
                    [ A.id itemId ]
                    [ text "____" ]

            else
                li
                    (A.id itemId :: system.dropEvents index itemId)
                    [ text item ]

        Nothing ->
            li
                (A.id itemId :: system.dragEvents index itemId)
                [ text item ]


view : Model -> Html Msg
view model =
    case model of
        DataInput dataModel ->
            div []
                [ dataModel.error |> Maybe.withDefault "" |> text
                , textarea [ value dataModel.field, Html.Events.onInput ChangeData ] []
                , button [ onClick SubmitData ] [ text "Submit" ]
                ]

        PresortStep { unsorted, dnd } ->
            case unsorted of
                firstUnsorted :: _ ->
                    div []
                        [ ul [] (firstUnsorted |> List.indexedMap (itemView dnd))
                        , ghostView dnd firstUnsorted
                        , button [ onClick SubmitPresort ] [ text "Submit" ]
                        ]

                [] ->
                    div [] [ text "Something's wrong!!" ]

        MergeStep ({ unmerged } as mergeModel) ->
            div [] [ text "MergeStep" ]

        _ ->
            div [] [ text "Other step" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        PresortStep { dnd } ->
            system.subscriptions dnd

        _ ->
            Sub.none
