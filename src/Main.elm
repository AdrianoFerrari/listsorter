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
        { init = \_ -> ( ( DataInput { field = testData, error = Nothing }, [] ), Cmd.none )
        , update = updateWithHistory
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


type alias PresortStepData =
    { sorted : List (List String)
    , unsorted : List (List String)
    , dnd : DnDList.Model
    }


type alias MergeStepData =
    { merged : List (List String), unmerged : List ( List String, List String, List String ) }


type State
    = DataInput { field : String, error : Maybe String }
    | PresortStep PresortStepData
    | MergeStep MergeStepData
    | Complete (List String)


type alias Model =
    ( State, List State )



-- UPDATE


type Msg
    = ChangeData String
    | SubmitData
    | DnDMsg DnDList.Msg
    | SubmitPresort
    | SelectLeft
    | SelectRight
    | Undo


update : Msg -> State -> ( State, Cmd Msg )
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
                    ( MergeStep (pairForMerging (sorted ++ unsorted) |> Debug.log "after pairing"), Cmd.none )

                ( _, firstUnsorted :: restUnsorted ) ->
                    ( PresortStep { presortModel | sorted = sorted ++ [ firstUnsorted ], unsorted = restUnsorted }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( MergeStep mergeStepData, SelectLeft ) ->
            let
                ({ merged, unmerged } as nextMerge) =
                    selectLeft mergeStepData
            in
            case ( merged, unmerged ) of
                ( _, [] ) ->
                    let
                        nextPair =
                            pairForMerging merged
                    in
                    case nextPair.merged of
                        singleList :: [] ->
                            ( Complete singleList, Cmd.none )

                        _ ->
                            ( MergeStep nextPair, Cmd.none )

                ( _, _ ) ->
                    ( MergeStep nextMerge, Cmd.none )

        ( MergeStep mergeStepData, SelectRight ) ->
            let
                ({ merged, unmerged } as nextMerge) =
                    selectRight mergeStepData
            in
            case ( merged, unmerged ) of
                ( _, [] ) ->
                    let
                        nextPair =
                            pairForMerging merged
                    in
                    case nextPair.merged of
                        singleList :: [] ->
                            ( Complete singleList, Cmd.none )

                        _ ->
                            ( MergeStep nextPair, Cmd.none )

                ( _, _ ) ->
                    ( MergeStep nextMerge, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateWithHistory : Msg -> Model -> ( Model, Cmd Msg )
updateWithHistory msg ( current, history ) =
    case ( msg, history ) of
        ( Undo, prevState :: prevHistory ) ->
            ( ( prevState, prevHistory ), Cmd.none )

        _ ->
            update msg current
                |> (\( m, c ) -> ( ( m, current :: history ), c ))


pairForMerging : List (List String) -> MergeStepData
pairForMerging sortedLists =
    let
        evenLists =
            ListExtra.removeIfIndex (\i -> modBy 2 i == 0) sortedLists
                |> Debug.log "evenLists"

        oddLists =
            ListExtra.removeIfIndex (\i -> modBy 2 i /= 0) sortedLists
                |> Debug.log "oddLists"

        newLists =
            ListExtra.zip oddLists evenLists
                |> List.map (\( l, r ) -> ( l, r, [] ))
                |> Debug.log "newLists"
    in
    if List.length oddLists == List.length evenLists then
        { merged = [], unmerged = newLists }

    else
        case ListExtra.last oddLists of
            Just lastOdd ->
                { merged = [ lastOdd ], unmerged = newLists }

            Nothing ->
                Debug.todo "Shouldn't happen!"


selectLeft : MergeStepData -> MergeStepData
selectLeft ({ merged, unmerged } as original) =
    case unmerged of
        [] ->
            original

        ( onlyLeft :: [], firstRight, firstProgress ) :: unmergedRest ->
            { original | merged = merged ++ [ firstProgress ++ [ onlyLeft ] ++ firstRight ], unmerged = unmergedRest }

        ( firstLeft :: restLeft, firstRight, firstProgress ) :: unmergedRest ->
            { original | unmerged = ( restLeft, firstRight, firstProgress ++ [ firstLeft ] ) :: unmergedRest }

        _ ->
            original


selectRight : MergeStepData -> MergeStepData
selectRight ({ merged, unmerged } as original) =
    case unmerged of
        [] ->
            original

        ( firstLeft, onlyRight :: [], firstProgress ) :: unmergedRest ->
            { original | merged = merged ++ [ firstProgress ++ [ onlyRight ] ++ firstLeft ], unmerged = unmergedRest }

        ( firstLeft, firstRight :: restRight, firstProgress ) :: unmergedRest ->
            { original | unmerged = ( firstLeft, restRight, firstProgress ++ [ firstRight ] ) :: unmergedRest }

        _ ->
            original



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


mergeView : ( List String, List String, List String ) -> Html Msg
mergeView ( left, right, merged ) =
    div []
        [ button [ onClick Undo ] [ text "Undo" ]
        , hr [] []
        , h1 [] [ text "Left" ]
        , button [ onClick SelectLeft ] [ text "select left" ]
        , ul [] (List.map (\s -> li [] [ text s ]) left)
        , h1 [] [ text "Merged" ]
        , ul [] (List.map (\s -> li [] [ text s ]) merged)
        , h1 [] [ text "Right" ]
        , button [ onClick SelectRight ] [ text "select right" ]
        , ul [] (List.map (\s -> li [] [ text s ]) right)
        ]


view : Model -> Html Msg
view ( current, history ) =
    case current of
        DataInput dataModel ->
            div []
                [ dataModel.error |> Maybe.withDefault "" |> text
                , textarea [ value dataModel.field, Html.Events.onInput ChangeData ] []
                , button [ onClick SubmitData ] [ text "Submit" ]
                , button [ onClick Undo ] [ text "Undo" ]
                ]

        PresortStep { unsorted, dnd } ->
            case unsorted of
                firstUnsorted :: _ ->
                    div []
                        [ ul [] (firstUnsorted |> List.indexedMap (itemView dnd))
                        , ghostView dnd firstUnsorted
                        , button [ onClick SubmitPresort ] [ text "Submit" ]
                        , button [ onClick Undo ] [ text "Undo" ]
                        ]

                [] ->
                    div [] [ text "Something's wrong!!" ]

        MergeStep ({ unmerged } as mergeModel) ->
            case unmerged of
                firstUnmerged :: _ ->
                    mergeView firstUnmerged

                [] ->
                    div [] [ text "Shouldn't happen!" ]

        Complete finishedList ->
            div []
                [ h1 [] [ text "Done" ]
                , ul [] (List.map (\i -> li [] [ text i ]) finishedList)
                , button [ onClick Undo ] [ text "Undo" ]
                ]


subscriptions : Model -> Sub Msg
subscriptions ( current, history ) =
    case current of
        PresortStep { dnd } ->
            system.subscriptions dnd

        _ ->
            Sub.none
