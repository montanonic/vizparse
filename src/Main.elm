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
import Html exposing (Html, div, text, textarea)
import Html.Attributes exposing (class, cols, rows, style, value)
import Html.Events as Events exposing (keyCode, onInput)
import Json.Decode as Decode
import List.Extra as ListE



-- MAIN


main : Program () Model Msg
main =
    Browser.document { init = init, update = update, view = viewToDocument view, subscriptions = \_ -> Sub.none }



-- MODEL


type alias Model =
    { codeText : String
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { codeText =
            """87 + 672
"""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = CodeTextAreaChanged String
    | CodeTextAreaTabPressed
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
        )
    , Cmd.none
    )



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
        -- This ensures spaces take up the same layout space as invisible characters.
        ' ' ->
            token [ class "space" ] [ div [ style "visibility" "hidden" ] [ text "X" ] ]

        c ->
            token [] [ text (String.fromChar c) ]
