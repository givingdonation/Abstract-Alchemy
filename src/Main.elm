module Main exposing (main)
import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Array exposing (..)

main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model = {alchemySet: Array Char,
                    combinationTable: Array (Array Char),
                    page: Int,
                    elementSelectDisplay: Bool,
                    changeTable: Char -> Array (Array Char),
                    changeSet: Char -> Array Char}

init : Model
init = {alchemySet = fromList ['_','_'],
        combinationTable = fromList [fromList ['_'], fromList ['_']],
        page = 0,
        elementSelectDisplay = True,
        changeTable = always <| fromList [],
        changeSet = always <| fromList []}

zip a b = List.map2 Tuple.pair (toList a) <| toList b

view : Model -> Html Msg
view model = let pages = (let nextButtonEnable cond = button [onClick NextPage,
                                                              disabled <| not cond,
                                                              style "opacity" <| if cond then "1" else ".5"] [text "Next"]
                  in fromList [
                              div [] [h1 [] [text "Welcome to Abstract Alchemy!"],
                                      nextButtonEnable True]
                              ])
             in
  div []
    [ div [] [ case (get model.page pages) of
                   Nothing -> text "Out of Bounds Page"
                   Just x -> x
             , h2 [] [text "Combination Table"]
             ]
    , table [] [thead [] <| List.concat [[th [] []],
                       List.map (\(c, i) -> th [onClick <| ChangeSet i] [text <| String.fromChar c]) <| zip model.alchemySet <| initialize (length model.alchemySet) identity,
                       [th [] [button [onClick PopElement] [text "-"], button [onClick PushElement] [text "+"]]]
                       ],
                tbody [] <| toList <| Array.map
                    (\(alchemyElement,row) -> tr [] <| toList <| append (fromList [th [] [text <| String.fromChar alchemyElement]]) <| Array.map
                         (\c -> td [onClick <| ChangeTable 0 0] [text <| String.fromChar c])
                    row) <| fromList <| zip model.alchemySet model.combinationTable
               ]
    , div [] [text "Select an Element Below:", table [style "visibility" <| if model.elementSelectDisplay then "visible" else "collapse"] [
              tr [] (List.map (\c -> td [] [button [onClick <| CharSelected c] [text <| String.fromChar c]]) ['💨','🔥','🪨','🌊']),
              tr [] (List.map (\c -> td [] [button [onClick <| CharSelected c] [text <| String.fromChar c]]) ['\u{2697}','🔮','⏳','🧪']),
              tr [] (List.map (\c -> td [] [button [onClick <| CharSelected c] [text <| String.fromChar c]]) ['🧲','🪙','📜','\u{1F5DD}'])
             ]]
    ]

type Msg = NextPage | ChangeSet Int | PopElement | PushElement | ChangeTable Int Int | CharSelected Char

update : Msg -> Model -> Model
update msg model =
  case msg of
    NextPage ->
      {model | page = model.page + 1}
    _ ->
        model
