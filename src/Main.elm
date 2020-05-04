module Main exposing (..)

import Browser
import DnDList
import Html exposing (..)
import Html.Attributes as A exposing (class, value)
import Html.Events exposing (onClick)
import List.Extra as ListExtra
import List.Split exposing (..)
import List.Zipper as LZ exposing (..)


testData =
    List.range 1 23 |> List.map String.fromInt |> String.join "\n"


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( ( DataInput { field = testData, error = Nothing }, [] ), Cmd.none )
        , update = updateWithHistory
        , view = \model -> { title = "List Sort", body = [ view model ] }
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
    { items : Zipper (List String)
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

                    myChunked =
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
                case myChunked of
                    [] ->
                        ( DataInput { field = field, error = Just "5 items minimum" }, Cmd.none )

                    fstChunk :: restChunks ->
                        ( PresortStep { items = from [] fstChunk restChunks, dnd = system.model }, Cmd.none )

        ( PresortStep ({ items, dnd } as presortModel), DnDMsg dndMsg ) ->
            let
                ( newDnd, newUnsorted ) =
                    system.update dndMsg dnd (current items)
            in
            ( PresortStep { presortModel | dnd = newDnd, items = items |> mapCurrent (\_ -> newUnsorted) }
            , system.commands dnd
            )

        ( PresortStep ({ items } as presortModel), SubmitPresort ) ->
            case LZ.next items of
                Just nextItems ->
                    ( PresortStep { presortModel | items = nextItems }, Cmd.none )

                Nothing ->
                    ( MergeStep (pairForMerging (before items ++ [ current items ])), Cmd.none )

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

        oddLists =
            ListExtra.removeIfIndex (\i -> modBy 2 i /= 0) sortedLists

        newLists =
            ListExtra.zip oddLists evenLists
                |> List.map (\( l, r ) -> ( l, r, [] ))
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
    case ( left, right ) of
        ( leftItem :: leftList, rightItem :: rightList ) ->
            div [ class "grid-container" ]
                [ div [ class "left-item" ] [ button [ onClick SelectLeft ] [ text leftItem ] ]
                , div [ class "right-item" ] [ button [ onClick SelectRight ] [ text rightItem ] ]
                , div [ class "middle" ] [ button [ onClick SelectLeft ] [ text "same" ] ]
                , div [ class "left-list" ] [ ul [] (List.map (\s -> li [] [ text s ]) leftList) ]
                , div [ class "right-list" ] [ ul [] (List.map (\s -> li [] [ text s ]) rightList) ]
                , div [ class "question" ] [ h1 [] [ text "Which is greater?" ] ]
                ]

        _ ->
            div [] [ text "Shouldn't happen" ]


view : Model -> Html Msg
view ( current, _ ) =
    case current of
        DataInput dataModel ->
            div []
                [ dataModel.error |> Maybe.withDefault "" |> text
                , textarea [ value dataModel.field, Html.Events.onInput ChangeData ] []
                , button [ onClick SubmitData ] [ text "Submit" ]
                , button [ onClick Undo ] [ text "Undo" ]
                ]

        PresortStep { items, dnd } ->
            let
                currentItems =
                    LZ.current items
            in
            div []
                [ ul [] (currentItems |> List.indexedMap (itemView dnd))
                , ghostView dnd currentItems
                , button [ onClick SubmitPresort ] [ text "Submit" ]
                , button [ onClick Undo ] [ text "Undo" ]
                ]

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
