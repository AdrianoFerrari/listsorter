module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick)
import List.Extra as ListExtra
import List.Split exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox { init = DataInput { field = List.range 1 23 |> List.map String.fromInt |> String.join "\n", error = Nothing }, update = update, view = view }



-- MODEL


type MergeGroup a
    = InProgress ( List a, List a, List a )
    | Sorted (List a)


type Model
    = DataInput { field : String, error : Maybe String }
    | Presort (List (List String))
    | Merging (List (MergeGroup String))
    | Complete (List String)



-- UPDATE


type Msg
    = ChangeData String
    | SubmitData


update : Msg -> Model -> Model
update msg model =
    case ( model, msg ) of
        ( DataInput dataModel, ChangeData newField ) ->
            DataInput { dataModel | field = newField }

        ( DataInput { field }, SubmitData ) ->
            let
                items =
                    field
                        |> String.trim
                        |> String.split "\n"
            in
            if List.length items < 5 then
                DataInput { field = field, error = Just "5 items minimum" }

            else
                let
                    chunked =
                        chunksOfLeft 5 items
                in
                case modBy 5 <| List.length items of
                    1 ->
                        items
                            |> List.reverse
                            |> ListExtra.splitAt 6
                            |> (\( end, begin ) -> chunksOfLeft 3 end ++ chunksOfLeft 5 begin)
                            |> List.reverse
                            |> Presort

                    2 ->
                        items
                            |> List.reverse
                            |> ListExtra.splitAt 7
                            |> (\( end, begin ) -> (chunksOfRight 4 end |> List.reverse) ++ chunksOfLeft 5 begin)
                            |> List.map List.reverse
                            |> List.reverse
                            |> Presort

                    3 ->
                        items
                            |> List.reverse
                            |> ListExtra.splitAt 8
                            |> (\( end, begin ) -> chunksOfLeft 4 end ++ chunksOfLeft 5 begin)
                            |> List.map List.reverse
                            |> List.reverse
                            |> Presort

                    _ ->
                        Presort chunked

        _ ->
            model


view model =
    case model of
        DataInput dataModel ->
            div []
                [ dataModel.error |> Maybe.withDefault "" |> text
                , textarea [ value dataModel.field, Html.Events.onInput ChangeData ] []
                , button [ onClick SubmitData ] [ text "Submit" ]
                ]

        Presort lists ->
            ul [] (lists |> List.map (\l -> li [] (List.map (\s -> li [] [ text s ]) l)))

        _ ->
            div [] [ text "Other state" ]
