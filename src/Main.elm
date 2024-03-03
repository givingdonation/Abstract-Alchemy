module Main exposing (main)
import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import List.Extra exposing (..)
import List exposing (..)


main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }


type Selection = TableSelect | SetSelect | Hidden


type alias Model =
    { alchemySet: List Char
    , combinationTable: List (List Char)
    , page: Int
    , elementSelectDisplay: Selection
    , changeTable: Char -> List (List Char)
    , changeSet: Char -> List Char
    , previousSet: List Char
--    , mappedSet: List Char
--    , changeMappingSet: Char -> List Char
    }


init : Model
init = {
    alchemySet = [],
    combinationTable = [],
    page = 0,
    elementSelectDisplay = Hidden,
    changeTable = always [],
    changeSet = always [],
    previousSet = []
--    mappedSet = [],
--    changeMappingSet = always []
    }



view : Model -> Html Msg
view model =
    let
        zip = map2 Tuple.pair

        staticTableTemplate aSet aTable = table [] [
               thead [] <| concat [
                    [th [] []],
                    List.map
                        (\c -> th [
                                     style "border-bottom" "5px solid #111111"
                                    ] [text <| String.fromChar c]
                        ) <| aSet
                   ],
               tbody []
                   <| List.map
                    (\(alchemyElement,row) -> tr []
                         <| append ([
                              th [
                                style "border-right" "5px solid #111111"
                              ] [text <| String.fromChar alchemyElement]
                             ])
                   <| List.map
                         (\c -> td [] [text <| String.fromChar c]) <| row)
                   <| zip aSet aTable
              ]

        sampleTable a b a2 b2 = [
                blockquote [] [
                    text "This alchemy is valid - ",
                    staticTableTemplate ['💨','🌊'] [['💨','🌊'],[a,b]]
                    ],
                blockquote [] [
                    text "This alchemy is not - ",
                    staticTableTemplate ['💨','🌊'] [['💨','🌊'],[a2,b2]]
                    ]
                ]

        identityElement = Maybe.withDefault 'I' <| head model.alchemySet

        nextButtonEnable cond =
            button [
                 onClick NextPage,
                 disabled <| not cond,
                 style "opacity" <| if cond then "1" else ".5"
                ] [text "Next"]

        groupCheck = (&&) ((&&) (notMember '_' model.alchemySet
                                     && Just model.alchemySet == head model.combinationTable
                                     && Just model.alchemySet == head (transpose model.combinationTable))
                  <| foldl (&&) True
                  <| List.map (\c -> member c model.alchemySet)
                  <| List.concat model.combinationTable)
                  <| foldl (&&) True
                  <| List.map (\row -> row == unique row) <| append model.combinationTable <| transpose model.combinationTable

        pages = [
         div [] [
              h1 [] [text "Welcome to Abstract Alchemy!"],
                  p [] [text """This is a game where you make your own alchemy. You work on the combination table,
                                where you can add new elements and show what they produce when they combine. The
                                goal of this game is to make your alchemy follow the right rules so you can move further."""],
                  p [] [text """Let's begin by adding a new element by clicking the plus.
                                Then click on the underscore in the top row, and then click
                                on the element you want to add. You can then give a value to the
                                combination of that element with itself, by clicking on the underscore in the table.
                                To begin let's make the simplest alchemy; make it so that the combination
                                of your element with itself will be itself. So, to move on, your table should now be
                                a 1 by 1 table with the combination of 1 element with itself producing itself."""],
                  nextButtonEnable <| [model.alchemySet] == model.combinationTable && model.alchemySet /= ['_']
             ],

         div [] [
              h2 [] [text "Using the Table"],
              p [] [text """Good, now let's play around with the table. Try adding, removing elements, and changing them around.
                            To move on, fill out a 3 by 3 table completely."""],
              nextButtonEnable
                  <| length model.alchemySet == 3 && notMember '_' (model.alchemySet ++ List.concat model.combinationTable)],

         div [] <| [h2 [] [text "Alchemical Rules - Closure"],
                                p [] [text """Let's try our first rule for alchemies, the rule of closure.
                                              The rule of closure is about producing only the same elements
                                              that were combined. For example:"""]
                                    ] ++ sampleTable '🌊' '🌊' '🌊' '🔥' ++ [
                                p [] [text """To move on, make a 3 by 3 table where all the combinations produce one of
                                            the three elements you are combining."""],
                                nextButtonEnable
                                    <| (&&) (length model.alchemySet == 3 && notMember '_' model.alchemySet)
                                    <| foldl (&&) True
                                    <| List.map (\c -> member c model.alchemySet) <| List.concat model.combinationTable
                               ],

         div [] <| [
              h2 [] [text "Alchemical Rules - Identity"],
              p [] [text """The next rule is the rule of identity. Our alchemy needs to have an identity element, which
                            is an element that when combined with any other element, the product will be that other element.
                            That means that the identity element is a "do nothing" element, since it doesn't cause the other
                            element it is combined with to change. For example:""" ]
                  ] ++ sampleTable '🌊' '🌊' '💨' '🌊' ++
                  [
                   p [] [ text """To move on, with your 3 by 3, make the first element of your alchemy an identity element.""" ],
                   nextButtonEnable
                        <| (&&) (length model.alchemySet == 3 && notMember '_' model.alchemySet)
                        <| Just model.alchemySet == head model.combinationTable
                        && Just model.alchemySet == head (transpose model.combinationTable)
                  ],

         div [] <| [
              h2 [] [text "Alchemical Rules - Inverses"],
              p [] [text """The rule of inverses requires that there are no duplicate elements in any rows or columns of
                            your table. This is because every element must occur exactly once for every row or column
                            since you must be able to cancel out any element and recombine the result to produce any other
                            element. For example:"""]
             ] ++ sampleTable '🌊' '💨' '🌊' '🌊' ++
             [
              p [] [text """To move on, with your 3 by 3, make sure every row and every column have no duplicate elements."""],
              nextButtonEnable
                  <| (&&) (length model.alchemySet == 3 && notMember '_' model.alchemySet)
                  <| foldl (&&) True
                  <| List.map (\row -> row == unique row) <| append model.combinationTable <| transpose model.combinationTable
             ],

         div [] <| [
              h2 [] [text "Alchemical Rules - Commutativity"],
              p [] [text """The rule of commutativity says that the order you combine elements in shouldn't matter.
                            This means that our table should be symmetric along the diagonal. For example:"""]
             ] ++ sampleTable '🌊' '💨' '💨' '💨' ++
              [
               p [] [text """To move on, make the 3 by 3 table have symmetry across the diagonal."""],
               nextButtonEnable
                   <| (&&) (length model.alchemySet == 3 && notMember '_' model.alchemySet)
                   <| model.combinationTable == transpose model.combinationTable
              ],

         div [] [
              h2 [] [text "All Alchemical Rules"],
              p [] [text """To move on, make a 3 by 3 table that follows all the rule together: Closure, Identity, Inverses, Commutativity.
                            There is exactly one table that follows all the rules for a 3 by 3 table."""],
              nextButtonEnable <| groupCheck && 3 == (length model.alchemySet)],

         div [] [
              h2 [] [text "Row/Column Swapping"],
              p [] [text """we can swap the columns or the rows of any alchemy, and everything will stay the same.
                        Here is a button that will swap the 2nd and 3rd columns and the 2nd and 3rd rows.
                        This essentially just swaps the order of the elements, but doesn't change what they do.""" ],
              button [onClick <| Messages <| [ NextPage, SwapRowAndColumn 1 2 ]] [text "Swap 2nd and 3rd; Next"]
             ],

         div [] [h2 [] [text "Mapping Alchemies"],
                p [] [text """An alchemy can be changed into another through a mapping.
                        Mappings take every element in one alchemy and change them
                        with a corresponding one in another alchemy. Two alchemies
                        are structured exactly the same way if they have a one-to-one Mapping
                        between them. To move on, use the following mapping to turn your
                        alchemy back into the original, before order swapping."""],
                div [] (List.map2 (\c c2 -> p [] [
                                             text
                                                 <| String.fromChar c
                                                     ++ " → "
                                                     ++ (String.fromChar c2)
                                            ]) model.previousSet <| swapAt 1 2 model.previousSet
                       ),
                nextButtonEnable <| model.alchemySet == model.previousSet && groupCheck && 3 == (length model.alchemySet)
                ],

         div [] [
              h2 [] [text "4 by 4 table, part 1"],
              p [] [text """Let's now get into some serious alchemy. We want to make a specific structure for your table.
                    To move on, make your table one that fills in the missing table from the example below and also follows all
                            previous rules."""],
              text "This is your template, fill in the '-' as you see fit while following the rules:",
              staticTableTemplate [identityElement, '-', '-', '-']
                  [repeat 4 '-'
                  , [ '-', identityElement, '-', '-' ]
                  , [ '-', '-', identityElement, '-' ]
                  ,repeat 4 '-'],
              nextButtonEnable
                  <| groupCheck
                    && Just identityElement == (getAt 1 <| Maybe.withDefault [] <| getAt 1 model.combinationTable)
                    && Just identityElement == (getAt 2 <| Maybe.withDefault [] <| getAt 2 model.combinationTable)
                    && 4 == (length model.alchemySet)
             ],

         div [] [h2 [] [text "4 by 4 table, part 2"]
            , p [] [text """To move on, make your table one that fills in the new missing table and also follows all
                            previous rules."""],
              text "This is your template, fill in the '-' as you see fit while following the rules:",
              staticTableTemplate [identityElement, '-', '-', '-']
                  [repeat 4 '-'
                  , [ '-', '-', '-', identityElement ]
                  ,repeat 4 '-'
                  ,repeat 4 '-'],
              nextButtonEnable
                  <| groupCheck && Just identityElement
                  == (getAt 3 <| Maybe.withDefault [] <| getAt 1 model.combinationTable) && 4 == (length model.alchemySet)
                ],

         div [] [h2 [] [text "4 by 4 table, part 3"]
            , p [] [text """To move on, make your table one that fills in the new missing table and also follows all
                            previous rules."""],
              text "This is your template, fill in the '-' as you see fit while following the rules:",
              staticTableTemplate [identityElement, '-', '-', '-']
                  [repeat 4 '-'
                  , [ '-', '-', identityElement, '-' ]
                  ,repeat 4 '-'
                  ,repeat 4 '-'],
              nextButtonEnable
                  <| groupCheck && Just identityElement
                  == (getAt 2 <| Maybe.withDefault [] <| getAt 1 model.combinationTable) && 4 == (length model.alchemySet)
                ],

         div [] [h2 [] [text "4 by 4 table, part 4"]
            , p [] [text """To move on, make your table one that fills in the new missing table and also follows all
                            previous rules."""],
              text "This is your template, fill in the '-' as you see fit while following the rules:",
              staticTableTemplate [identityElement, '-', '-', '-']
                  [repeat 4 '-'
                  ,repeat 4 '-'
                  , [ '-', '-', '-', identityElement ]
                  ,repeat 4 '-'],
              nextButtonEnable
                  <| groupCheck && Just identityElement
                  == (getAt 3 <| Maybe.withDefault [] <| getAt 2 model.combinationTable) && 4 == (length model.alchemySet)
                ],

         div [] [h2 [] [text "Mapping to part 2"]
                , p [] [text """These past 4 tables are the only possible ways of making an alchemy that follows the 4 rules,
                         however, some of these tables are identical to each other, just with a mapping. To move on,
                         perform a mapping from part 4's table into part 2's table, use the order swap to help you. As
                         a reminder, the part 2 table has its identities in the following places:"""]
                , staticTableTemplate model.previousSet
                  [identityElement :: repeat 3 '-'
                  , [ '-', '-', '-', identityElement ]
                  , [ '-', '-', identityElement, '-' ]
                  , [ '-', identityElement, '-', '-' ]
                  ]
                , button [onClick <| SwapRowAndColumn 1 2] [text "Swap 2nd and 3rd"]
                , nextButtonEnable
                    <| groupCheck && model.alchemySet == model.previousSet && Just identityElement
                  == (getAt 3 <| Maybe.withDefault [] <| getAt 1 model.combinationTable) && 4 == (length model.alchemySet)
                ],

         div [] [h2 [] [text "Mapping from part 2"]
                , p [] [text """Now lets make a mapping from part 2 to part 3. As a reminder,
                              the part 3 table has identities like this:"""]
                , staticTableTemplate model.previousSet
                  [identityElement :: repeat 3 '-'
                  , [ '-', '-', identityElement, '-' ]
                  , [ '-', identityElement, '-', '-' ]
                  , [ '-', '-', '-', identityElement ]
                  ]
                , button [onClick <| SwapRowAndColumn 2 3] [text "Swap 3rd and 4th"]
                , nextButtonEnable
                    <| groupCheck  && model.alchemySet == model.previousSet && Just identityElement
                  == (getAt 2 <| Maybe.withDefault [] <| getAt 1 model.combinationTable) && 4 == (length model.alchemySet)
                ],

         {-div [] [h2 [] [text "Mapping to part 1"]
                , p [] [text """The previous examples show that the structures of tables 2, 3, and 4 are identical.
                              Lets try to make a mapping from part 3 to part 1. As a reminder, heres the identities of
                              part 1."""]
                , staticTableTemplate model.previousSet
                  [identityElement :: repeat 3 '-'
                  , [ '-', identityElement, '-', '-' ]
                  , [ '-', '-', identityElement, '-' ]
                  , [ '-', '-', '-', identityElement ]
                  ]
                , button [onClick <| SwapRowAndColumn 1 2] [text "Swap 2nd and 3rd"]
                , button [onClick <| SwapRowAndColumn 2 3] [text "Swap 3rd and 4th"]
                , nextButtonEnable
                    <| groupCheck
                        && Just identityElement == (getAt 2 <| Maybe.withDefault [] <| getAt 2 model.combinationTable)
                        && Just identityElement == (getAt 1 <| Maybe.withDefault [] <| getAt 1 model.combinationTable)
                        && 4 == (length model.alchemySet)
                , button [onClick NextPage] [text "give up"]
                ],
-}
         div [] [h2 [] [text "5 by 5 table"]
                , p [] [text """The previous examples show that the structures of tables 2, 3, and 4 are identical.
                              Now try to make a 5 by 5 table, notice what way said table can be made, and if there are
                              any others."""]
                , button [onClick <| SwapRowAndColumn 1 2] [text "Swap 2nd and 3rd"]
                , button [onClick <| SwapRowAndColumn 2 3] [text "Swap 3rd and 4th"]
                , button [onClick <| SwapRowAndColumn 3 4] [text "Swap 4th and 5th"]
                , nextButtonEnable <| groupCheck && 5 == (length model.alchemySet)
                ]
            ]

        alchemyRange = range 0 (length model.alchemySet - 1)
    in
  div [] [div [] [Maybe.withDefault (h1 [] [text "The End"]) <| getAt model.page pages
                 , h2 [] [text "Combination Table"]
                 ]
         , table [] [
               thead [] <| concat [
                    [th [] []],
                    List.map
                        (\(c, i) -> th [
                                     onClick <| ChangeSet i,
                                     style "border-bottom" "5px solid #111111"
                                    ] [text <| String.fromChar c]
                        ) <| zip model.alchemySet alchemyRange,
                    [th [] [
                          button [onClick PopElement] [text "-"],
                          button [onClick PushElement] [text "+"]]
                    ]
                   ],
               tbody []
                   <| List.map
                    (\(alchemyElement,row,n) -> tr []
                         <| append ([
                              th [
                                onClick <| ChangeSet n,
                                style "border-right" "5px solid #111111"
                              ] [text <| String.fromChar alchemyElement]
                             ])
                   <| List.map
                         (\(c, n2) -> td [onClick <| ChangeTable n n2] [text <| String.fromChar c]) <| zip row alchemyRange)
                   <| List.map3 (\x y z -> (x,y,z)) model.alchemySet model.combinationTable alchemyRange
              ]
         , div [
               style "visibility"
                   <| if model.elementSelectDisplay /= Hidden then "visible" else "collapse"
              ] [
               text "Select an Element Below:",
               table [] [
                    tr [] (
                           List.map
                               (\c -> td [] [button [onClick <| CharSelected c] [text <| String.fromChar c]])
                               ['💨','🔥','🪨','🌊','🌌']
                          ),
                    tr [] (
                           List.map
                               (\c -> td [] [button [onClick <| CharSelected c] [text <| String.fromChar c]])
                               ['\u{2697}','🔮','⏳','🧪','🐉']
                          ),
                    tr [] (
                           List.map
                               (\c -> td [] [button [onClick <| CharSelected c] [text <| String.fromChar c]])
                               ['🧲','🪙','📜','\u{1F5DD}','🧂']
                          )
                   ]
              ]
         ]


type Msg
    = NextPage
    | ChangeSet Int
    | PopElement
    | PushElement
    | ChangeTable Int Int
    | CharSelected Char
    | SwapRowAndColumn Int Int
    | Messages (List Msg)


update : Msg -> Model -> Model
update msg model =
  case msg of
    NextPage ->
      {model | previousSet = model.alchemySet, page = model.page + 1}

    ChangeSet x ->
        {model | changeSet = \c -> setAt x c model.alchemySet, elementSelectDisplay = SetSelect}

    ChangeTable x y ->
        {model |
             changeTable =
             \c -> setAt x (setAt y c (Maybe.withDefault [] (getAt x model.combinationTable))) model.combinationTable,
             elementSelectDisplay = TableSelect
        }

    PopElement ->
        {model |
             alchemySet = Maybe.withDefault [] <| List.Extra.init model.alchemySet,
             combinationTable =
                List.map (\row -> Maybe.withDefault [] <| List.Extra.init row)
                <| Maybe.withDefault []
                <| List.Extra.init model.combinationTable
        }

    PushElement ->
        {model |
             alchemySet = List.append model.alchemySet ['_'],
             combinationTable =
                List.map (\row -> List.append row ['_'])
                <| List.append model.combinationTable
                <| singleton
                <| List.repeat (length model.alchemySet) '_'
        }

    CharSelected c ->
        let
            changedModel =
                if model.elementSelectDisplay == TableSelect then
                    {model | combinationTable = model.changeTable c}
                else {model | alchemySet = model.changeSet c}
        in
        {changedModel | elementSelectDisplay = Hidden}

    SwapRowAndColumn x y ->
        {model |
             alchemySet = swapAt x y model.alchemySet,
             combinationTable = swapAt x y <| transpose <| swapAt x y <| transpose model.combinationTable}

    Messages msgs ->
        foldl update model msgs
