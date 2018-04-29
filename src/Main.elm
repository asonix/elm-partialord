module Main exposing (Model, Msg, main, sorted)

{-| The Main module


# Other thing

@docs main
@docs sorted
@docs Model
@docs Msg

-}

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)


{-| Main function
-}
main : Program Never Model Msg
main =
    beginnerProgram
        { model =
            { values = values
            , rules = rules
            , first = ""
            , second = ""
            , third = ""
            }
        , view = view
        , update = update
        }


rules : List ( String, String )
rules =
    [ ( "main", "parse_options" )
    , ( "main", "tail_file" )
    , ( "main", "tail_forever" )
    , ( "tail_file", "pretty_name" )
    , ( "tail_file", "write_header" )
    , ( "tail_file", "tail" )
    , ( "tail_forever", "recheck" )
    , ( "tail_forever", "pretty_name" )
    , ( "tail_forever", "write_header" )
    , ( "tail_forever", "dump_remainder" )
    , ( "tail", "tail_lines" )
    , ( "tail", "tail_bytes" )
    , ( "tail_lines", "start_lines" )
    , ( "tail_lines", "dump_remainder" )
    , ( "tail_lines", "file_lines" )
    , ( "tail_lines", "pipe_lines" )
    , ( "tail_bytes", "xlseek" )
    , ( "tail_bytes", "start_bytes" )
    , ( "tail_bytes", "dump_remainder" )
    , ( "tail_bytes", "pipe_bytes" )
    , ( "file_lines", "dump_remainder" )
    , ( "recheck", "pretty_name" )
    ]


values : List String
values =
    [ "tail_file"
    , "parse_options"
    , "write_header"
    , "tail"
    , "recheck"
    , "dump_remainder"
    , "tail_forever"
    , "pretty_name"
    , "tail_lines"
    , "tail_bytes"
    , "main"
    , "start_lines"
    , "file_lines"
    , "pipe_lines"
    , "xlseek"
    , "start_bytes"
    , "pipe_bytes"
    ]


{-| Model
-}
type alias Model =
    { rules : List ( String, String )
    , values : List String
    , first : String
    , second : String
    , third : String
    }


{-| Message
-}
type Msg
    = AddRule
    | AddValue
    | SetFirst String
    | SetSecond String
    | SetThird String


view : Model -> Html Msg
view model =
    div []
        [ div
            [ css
                [ displayFlex
                , flexDirection row
                , justifyContent spaceAround
                ]
            ]
            [ div []
                [ h1 [] [ text "Rules" ]
                , ul
                    [ css [ textDecoration none ] ]
                    (List.map renderRule model.rules)
                ]
            , div []
                [ h1 [] [ text "Values" ]
                , ul
                    [ css [ textDecoration none ] ]
                    (List.map renderItem (sorted model.rules model.values))
                ]
            ]
        , div
            [ css
                [ displayFlex
                , justifyContent spaceAround
                , flexDirection row
                ]
            ]
            [ div []
                [ h2 [] [ text "Add Rule" ]
                , div
                    [ css
                        [ padding (Css.em 0.5)
                        , textAlign right
                        ]
                    ]
                    [ label
                        [ for "one"
                        , css [ padding (Css.em 0.5) ]
                        ]
                        [ text "This item" ]
                    , input
                        [ type_ "text"
                        , value model.first
                        , onInput SetFirst
                        , name "one"
                        ]
                        []
                    ]
                , div
                    [ css
                        [ padding (Css.em 0.5)
                        , textAlign right
                        ]
                    ]
                    [ label
                        [ for "two"
                        , css [ padding (Css.em 0.5) ]
                        ]
                        [ text "Comes before this item" ]
                    , input
                        [ type_ "text"
                        , value model.second
                        , onInput SetSecond
                        , name "two"
                        ]
                        []
                    ]
                , input
                    [ type_ "submit"
                    , onClick AddRule
                    ]
                    []
                ]
            , div
                []
                [ h2 [] [ text "Add value" ]
                , div
                    [ css
                        [ padding (Css.em 0.5)
                        , textAlign right
                        ]
                    ]
                    [ label
                        [ for "three"
                        , css [ padding (Css.em 0.5) ]
                        ]
                        [ text "Three" ]
                    , input
                        [ name "three"
                        , value model.third
                        , onInput SetThird
                        , type_ "text"
                        ]
                        []
                    ]
                , input
                    [ type_ "submit"
                    , onClick AddValue
                    ]
                    []
                ]
            ]
        ]


renderItem : String -> Html Msg
renderItem item =
    li [] [ text item ]


renderRule : ( String, String ) -> Html Msg
renderRule ( one, two ) =
    li []
        [ text one
        , text " | "
        , text two
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddRule ->
            { model
                | rules = ( model.first, model.second ) :: model.rules
                , first = ""
                , second = ""
            }

        AddValue ->
            { model
                | values = model.third :: model.values
                , third = ""
            }

        SetFirst v ->
            { model | first = v }

        SetSecond v ->
            { model | second = v }

        SetThird v ->
            { model | third = v }


{-| The sorted things
-}
sorted : List ( String, String ) -> List String -> List String
sorted rules values =
    List.sortWith (lessThan rules) values


lessThan : List ( String, String ) -> String -> String -> Order
lessThan mapping x y =
    if existsPair mapping x y then
        LT
    else if List.any (isLessThan mapping y) (getChildren mapping x) then
        LT
    else
        GT


isLessThan : List ( String, String ) -> String -> String -> Bool
isLessThan mapping one =
    (==) LT << flip (lessThan mapping) one


getChildren : List ( String, String ) -> String -> List String
getChildren mapping y =
    mapping
        |> List.filterMap (getChild y)


getChild : String -> ( String, String ) -> Maybe String
getChild y ( one, two ) =
    if y == one then
        Just two
    else
        Nothing


existsPair : List ( String, String ) -> String -> String -> Bool
existsPair mapping x y =
    List.filter (isPair x y) mapping
        |> List.isEmpty
        |> not


isPair : String -> String -> ( String, String ) -> Bool
isPair x y ( one, two ) =
    x == one && y == two
