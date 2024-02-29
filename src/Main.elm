module Main exposing (main)
import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import List.Extra exposing (..)
import List exposing (..)
main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }

type Selection = TableSelect | SetSelect | Hidden

type alias Model = {alchemySet: List Char,
                    combinationTable: List (List Char),
                    page: Int,
                    elementSelectDisplay: Selection,
                    changeTable: Char -> List (List Char),
                    changeSet: Char -> List Char}

init : Model
init = {alchemySet = [],
        combinationTable = [],
        page = 0,
        elementSelectDisplay = Hidden,
        changeTable = always [],
        changeSet = always []}

zip = map2 Tuple.pair

view : Model -> Html Msg
view model = let pages = (let nextButtonEnable cond = button [onClick NextPage,
                                                              disabled <| not cond,
                                                              style "opacity" <| if cond then "1" else ".5"] [text "Next"]
                  in [
                              div [] [h1 [] [text "Welcome to Abstract Alchemy!"],
                                      nextButtonEnable <| [model.alchemySet] == model.combinationTable && model.alchemySet /= ['_']]
                              ])
             in let alchemyRange = range 0 (length model.alchemySet - 1) in
  div []
    [ div [] [ Maybe.withDefault (text "Out of Bounds Page") <| getAt model.page pages
             , h2 [] [text "Combination Table"]
             ]
    , table [] [thead [] <| concat [[th [] []],
                       List.map (\(c, i) -> th [onClick <| ChangeSet i] [text <| String.fromChar c]) <| zip model.alchemySet alchemyRange,
                       [th [] [button [onClick PopElement] [text "-"], button [onClick PushElement] [text "+"]]]
                       ],
                tbody [] <| List.map
                    (\(alchemyElement,row,n) -> tr [] <| append ([th [] [text <| String.fromChar alchemyElement]]) <| List.map
                         (\(c, n2) -> td [onClick <| ChangeTable n n2] [text <| String.fromChar c])
                    <| zip row alchemyRange) <| List.map3 (\x y z -> (x,y,z)) model.alchemySet model.combinationTable alchemyRange
               ]
    , div [] [text "Select an Element Below:", table [style "visibility" <| if model.elementSelectDisplay /= Hidden then "visible" else "collapse"] [
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
    ChangeSet x ->
        {model | changeSet = \c -> setAt x c model.alchemySet, elementSelectDisplay = SetSelect}

    ChangeTable x y ->
        {model | changeTable = \c -> setAt x (setAt y c (Maybe.withDefault [] (getAt x model.combinationTable))) model.combinationTable, elementSelectDisplay = TableSelect}
    PopElement ->
        {model |alchemySet = Maybe.withDefault [] <| List.Extra.init model.alchemySet, combinationTable = List.map (\row -> Maybe.withDefault [] <| List.Extra.init row) <| Maybe.withDefault [] <| List.Extra.init model.combinationTable}
    PushElement ->
        {model |alchemySet = List.append model.alchemySet ['_'], combinationTable = List.map (\row -> List.append row ['_']) <| List.append model.combinationTable <| singleton <| List.repeat (length model.alchemySet) '_'}
    CharSelected x ->
        let changedModel = if model.elementSelectDisplay == TableSelect then
              {model | combinationTable = model.changeTable x}
                else {model | alchemySet = model.changeSet x} in
        {changedModel| elementSelectDisplay = Hidden}
