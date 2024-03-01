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
                                p [] [text """This is a game where you make your own alchemy. You work on the combination table,
                                            where you can add new elements and show what they produce when they combine. The
                                            goal of this game to make your alchemy follow the right rules so you can move further."""],
                                p [] [text """Let's begin by adding a new element by clicking the plus,
                                            then click on the underscore on the top row, and then clicking
                                            on the element you want to add. You can then give a value to the
                                            combination of that element with itself, by clicking on the underscore.
                                            To begin let's make the simplest alchemy; make it so that the combination
                                            of your element with itself will be itself. So, to move on, your table should now be
                                            a 1 by 1 table with the combination of 1 element producing itself."""],
                        nextButtonEnable <| [model.alchemySet] == model.combinationTable && model.alchemySet /= ['_']],
                        div [] [h2 [] [text "Using the Table"],
                                p [] [text """Good, now let's play around with the table. Try adding, removing elements, and changing them around.
                                            To move on, fill out a 3 by 3 table completely."""],
                                nextButtonEnable <| length model.alchemySet == 3 && notMember '_' (List.concat model.combinationTable)],
                        div [] [h2 [] [text "Alchemical Rules - Closure"],
                                p [] [text """Let's try our first rule for alchemies, the rule of closure.
                                             The rule of closure is about producing only the same elements  that were combined. For example:"""],
                                blockquote [] [text "This alchemy has closure - ",table [] [thead [] [th [] [], th [] [text <| String.fromChar '💨'], th [] [text <| String.fromChar '🌊']]
                                                        ,tr [] [ th [] [text <| String.fromChar '💨'], td [] [text <| String.fromChar '💨'], th [] [text <| String.fromChar '🌊']]
                                                        ,tr [] [ th [] [text <| String.fromChar '🌊'], td [] [text <| String.fromChar '🌊'], th [] [text <| String.fromChar '🌊']]]],
                                blockquote [] [text "This alchemy does not - ",table [] [thead [] [th [] [], th [] [text <| String.fromChar '💨'], th [] [text <| String.fromChar '🌊']]
                                                        ,tr [] [ th [] [text <| String.fromChar '💨'], td [] [text <| String.fromChar '💨'], th [] [text <| String.fromChar '🌊']]
                                                        ,tr [] [ th [] [text <| String.fromChar '🌊'], td [] [text <| String.fromChar '🌊'], th [] [text <| String.fromChar '🔥']]]]
                               , p [] [text """To move on, make a 3 by 3 table where all the combinations produce one of
                                            the three elements you are combining."""],
                                nextButtonEnable <| (&&) (length model.alchemySet == 3 && notMember '_' model.alchemySet) <| foldl (&&) True <| List.map (\c -> member c model.alchemySet) <| List.concat model.combinationTable
                               ],
                        div [] [h2 [] [text "Alchemical Rules - Identity"]],
                        div [] [h2 [] [text "Alchemical Rules - Inverses"]],
                        div [] [h2 [] [text "Alchemical Rules - Commutativity"]],
                        div [] [h2 [] [text "All Alchemical Rules"]],
                        div [] [h2 [] [text "Mapping Alchemies"]],
                        div [] [h2 [] [text "4 by 4 table, part 1"]],
                        div [] [h2 [] [text "4 by 4 table, part 2"]],
                        div [] [h2 [] [text "4 by 4 table, part 3"]],
                        div [] [h2 [] [text "4 by 4 table, part 4"]],
                        div [] [h2 [] [text "Perform a Mapping"]],
                        div [] [h2 [] [text "Make a Mapping"]],
                        div [] [h2 [] [text "5 by 5 table"]]
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
                    (\(alchemyElement,row,n) -> tr [] <| append ([th [onClick <| ChangeSet n] [text <| String.fromChar alchemyElement]]) <| List.map
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
