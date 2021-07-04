{-
   Okay so my main goal, since I don't have pencil and paper, is to build up feature-by-feature into more complex parsing. We'll start with simple calculator arithmetic, and elaborate more deeply.

   So, initial goals:

   parse "3 + 7", and also with -, *, /
   parse the same, but with larger numbers
   parse the same, but with negative numbers

   Do all of this with a char-by-char UI? At least to get a feel for what that's like before discounting its power just because the elm parsing model already provides utilities for operating on larger chunks all at once.
-}


module Main exposing (Msg(..), main, update, view)

import Browser
import Debug
import Html exposing (Html, button, div, input, p, text, textarea)
import Html.Attributes exposing (class, cols, disabled, placeholder, rows, size, style, title, value)
import Html.Events as Events exposing (keyCode, onClick, onInput)
import Json.Decode as Decode
import List.Extra as ListE



-- MAIN


main : Program () Model Msg
main =
    Browser.document { init = init, update = update, view = viewToDocument view, subscriptions = \_ -> Sub.none }



-- MODEL


type alias Model =
    { codeText : String
    , tokenizerRules : List Rule
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { codeText =
            """87 + 672
"""
      , tokenizerRules = [ initialRule ]
      }
    , Cmd.none
    )


initialRule : Rule
initialRule =
    let
        singleMatcher : Matcher
        singleMatcher =
            { modifier = Nothing, matcherType = DigitMatcher }

        singleCase : Case
        singleCase =
            { name = Nothing
            , matchers = [ singleMatcher ]
            }
    in
    { name = "Number", cases = [ singleCase, singleCase ] }



-- UPDATE


type Msg
    = CodeTextAreaChanged String
    | CodeTextAreaTabPressed
      -- | CreateRuleClicked
      {- idx newName -}
    | TokenizerRuleNameChanged Int String {- idx caseIdx -}
    | TokenizerAddMatcher Int Int
    | NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( Debug.log "model update"
        (case Debug.log "message" msg of
            NoOp ->
                model

            CodeTextAreaChanged newCode ->
                { model | codeText = newCode }

            -- Do nothing, prevent focus-shift.
            CodeTextAreaTabPressed ->
                model

            TokenizerRuleNameChanged idx newName ->
                updateRule idx (\rule -> { rule | name = newName }) model

            TokenizerAddMatcher idx caseIdx ->
                updateRule idx
                    (updateCase caseIdx
                        (\rcase ->
                            { rcase
                                | matchers =
                                    rcase.matchers
                                        ++ [ { modifier = Nothing
                                             , matcherType = DigitMatcher
                                             }
                                           ]
                            }
                        )
                    )
                    model
        )
    , Cmd.none
    )


updateRule : Int -> (Rule -> Rule) -> Model -> Model
updateRule ruleIdx func model =
    { model | tokenizerRules = model.tokenizerRules |> ListE.updateAt ruleIdx func }


updateCase : Int -> (Case -> Case) -> Rule -> Rule
updateCase caseIdx func rule =
    { rule | cases = rule.cases |> ListE.updateAt caseIdx func }



-- {-| When no command should be sent
-- -}
-- pure : Model -> ( Model, Cmd msg )
-- pure model =
--     ( model, Cmd.none )
-- VIEW


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


viewToDocument : (Model -> Html Msg) -> Model -> Document Msg
viewToDocument viewFn model =
    { title = "VizParser", body = [ viewFn model ] }


view : Model -> Html Msg
view model =
    div
        [ class "app" ]
        [ currentCodeTextArea model.codeText
        , tokenizerBuilder model.tokenizerRules
        , characterTokensView model.codeText
        ]


currentCodeTextArea : String -> Html Msg
currentCodeTextArea codeText =
    textarea [ onInput CodeTextAreaChanged, value codeText, tabPressed, rows 10, cols 30 ] []


succeededIfTabKey : Int -> Decode.Decoder Int
succeededIfTabKey key =
    if key == 9 then
        Decode.succeed key

    else
        Decode.fail "non-tab"


tabPressed : Html.Attribute Msg
tabPressed =
    Decode.andThen succeededIfTabKey keyCode
        |> Decode.map (always CodeTextAreaTabPressed)
        |> Decode.map (\x -> ( x, True ))
        |> Events.preventDefaultOn "keydown"


{-| A view of each individual token in the output.
-}
characterTokensView : String -> Html Msg
characterTokensView codeText =
    let
        characters =
            String.toList codeText

        -- A list of all the lines of characters; different that split because we don't remove the newline character, but instead keep it (if it exists) at the end of that line's list.
        charactersByLine =
            characters
                |> ListE.groupWhile (\a _ -> a /= '\n')
                |> List.map (\( x, xs ) -> x :: xs)
    in
    div [ class "character-tokens-view" ] (charactersByLine |> List.map renderLineOfCharacters)


renderLineOfCharacters : List Char -> Html Msg
renderLineOfCharacters chars =
    div [ class "line-of-characters" ] (List.map renderCharacterToken chars)


{-| Renders programmer text as character blocks, with some special styling for blocks that
typically have special parsing treatment.
-}
renderCharacterToken : Char -> Html Msg
renderCharacterToken char =
    let
        token attrs children =
            div
                (attrs
                    ++ [ class "character-token" ]
                )
                [ div [] children ]
    in
    case char of
        '\n' ->
            token [ class "newline" ] [ text "\\n" ]

        -- '\t' ->
        --     token [ class "newline" ] [ text "TAB" ]
        -- This ensures spaces take up the  same layout space as invisible characters.
        ' ' ->
            token [ class "space" ] [ div [ style "visibility" "hidden" ] [ text "X" ] ]

        c ->
            token [] [ text (String.fromChar c) ]



-- VIEW: Tokenizer Builder


tokenizerBuilder : List Rule -> Html Msg
tokenizerBuilder rules =
    div [ class "tokenizer-builder" ] [ tokenizerControlsBar, tokenizerRules rules ]


tokenizerControlsBar : Html Msg
tokenizerControlsBar =
    div [ class "controls-bar" ]
        [ button [] [ text "Create Rule" ]
        , input [ placeholder "control rules here" ] []
        ]


tokenizerRules : List Rule -> Html Msg
tokenizerRules rules =
    div [] (List.indexedMap ruleView rules)


ruleView : Int -> Rule -> Html Msg
ruleView idx rule =
    let
        viewMatcher matcher =
            let
                matcherInput text =
                    input [ disabled True, class "matcher", value text, size <| String.length text ] []
            in
            case matcher.matcherType of
                DigitMatcher ->
                    matcherInput "Digit"

        -- Todo: add the optional name into the display
        viewCase caseIdx ruleCase =
            div [ class "case" ]
                (List.map viewMatcher ruleCase.matchers
                    ++ [ button [ title "Add Matcher", onClick <| TokenizerAddMatcher idx caseIdx ] [ text "+" ] ]
                )
    in
    div [ class "rule" ] <|
        List.concat
            [ [ input
                    [ value rule.name, onInput <| TokenizerRuleNameChanged idx, placeholder "Rule Name" ]
                    []
              ]
            , List.indexedMap
                viewCase
                rule.cases
            ]


type alias Rule =
    { name : String
    , cases : List Case
    }


{-| A case is one row of matchers, which might optionally have a name to associate them with. If a
single case matches, the whole Rule matches.
-}
type alias Case =
    { name : Maybe String
    , matchers : List Matcher
    }


{-| Matchers are the specific code that will match the token, they can be built-ins or custom. Matchers can be
modified in many ways, and a row of matchers all combine together to form a single case clause.
-}
type alias Matcher =
    { modifier : Maybe MatcherModifier
    , matcherType : MatcherType
    }


type MatcherModifier
    = StarModifier
    | PlusModifier


{-| A grouped matcher is just multiple individual matchers combined together in one chunk,
specifically so that you can apply a modifier to all of them.
-}
type MatcherType
    = DigitMatcher



-- | CustomMatcher String
-- | GroupedMatcher (List Matcher)
