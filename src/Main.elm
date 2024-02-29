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

view : Model -> Html Msg
view model = let pages = (let nextButtonEnable cond = button [onClick Increment,
                                                              disabled <| not cond,
                                                              style "opacity" <| if cond then "1" else ".5"] [text "Next"]
                  in fromList [
                              div [] [h1 [] [text "Welcome to Abstract Alchemy!"],
                                      nextButtonEnable False]
                              ])
             in
  div []
    [ div [] [ case (get model.page pages) of
                   Nothing -> text "Out of Bounds Page"
                   Just x -> x
             , h2 [] [text "Combination Table"]
             ]
    , table [] [thead [] <| List.concat [[th [] []],
                       toList <| Array.map (\c -> th [onClick Wip] [text <| String.fromChar c]) model.alchemySet,
                       [th [] [button [onClick Wip] [text "-"], button [onClick Wip] [text "+"]]]
                       ],
                tbody [] <| toList <| Array.map
                    (\(alchemyElement,row) -> tr [] <| toList <| append (fromList [th [] [text <| String.fromChar alchemyElement]]) <| Array.map
                         (\c -> td [onClick Wip] [text <| String.fromChar c])
                    row) <| fromList <| List.map2 Tuple.pair (toList model.alchemySet) (toList model.combinationTable)
               ]
    , div [] [text "Select an Element Below:", table [style "visibility" <| if model.elementSelectDisplay then "visible" else "collapse"] [
              tr [] (List.map (\c -> td [] [button [onClick Wip] [text <| String.fromChar c]]) ['💨','🔥','🪨','🌊']),
              tr [] (List.map (\c -> td [] [button [onClick Wip] [text <| String.fromChar c]]) ['\u{2697}','🔮','⏳','🧪']),
              tr [] (List.map (\c -> td [] [button [onClick Wip] [text <| String.fromChar c]]) ['🧲','🪙','\u{1F4DC}','\u{1F5DD}'])
             ]]
    ]

type Msg = Increment | Wip

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      {model | page = 1}

    Wip ->
      model
