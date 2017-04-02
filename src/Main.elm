module Main exposing (..)

import Html exposing (Html, input, text, button)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onInput, onClick, onSubmit)

main =
    Html.beginnerProgram { model = model, view = view, update = update}

--
-- Model
--
type alias Item =
    { item : String
        , valor : Int
    }

type alias Orcamento =
    { blocos : Int
        , valor : Int
    }

type alias Model =
    { limiteOrcamento : Int
        , itens : List Item
        , precision : Int
        , squares : Int
        , form : Item
    }

form : Item
form = Item "" 0

model : Model
model = Model 0 [] 50 0 form


--
-- Update
--
type Msg
    = Calculate Int
    | ChangeOrcamento String
    | PostItem Item
    | UpdateItem String
    | UpdateValor String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Calculate p ->
            { model | squares = model.limiteOrcamento // p, precision = p }
        ChangeOrcamento p ->
            { model | limiteOrcamento = (Result.withDefault 0 (String.toInt p)), squares = (Result.withDefault 0 (String.toInt p)) // model.precision }
        PostItem p ->
            let
                novosItens = List.append model.itens [ p ]
            in
                { model | itens = novosItens, form = Item "" 0}
        UpdateItem p ->
            let
                form = model.form
                newForm = { form | item = p}
            in
                { model | form = newForm }
        UpdateValor p ->
            let
                form = model.form
                newForm = { form | valor = ((Result.withDefault 0 (String.toInt p))) }
            in
                { model | form = newForm }

--
-- View
--

drawSquares : Orcamento -> Int -> List (Html Msg) -> List (Html Msg)
drawSquares model iter result =
    if iter >= model.blocos then
        List.append result [ drawSquare model.valor ]
    else
        drawSquares model (iter + 1) (List.append result ([ drawSquare model.valor ]))

drawSquare : Int -> Html Msg
drawSquare squareValue =
    let
        style = Html.Attributes.style [
            ( "display", "inline-block" )
            , ( "width", "1.5em" )
            , ( "height", "1.5em" )
            , ( "line-height", "1.5em" )
            , ( "margin", "2px" )
            , ( "background-color", "#c44")
            , ( "text-align", "center")
            , ( "font-family", "monospace")
        ]
    in
        Html.div [ Html.Attributes.class "square", style ] [ Html.text (toString squareValue) ]

viewFormItem : Item -> Html Msg
viewFormItem model =
    let
        style = Html.Attributes.style [("width", "30%"), ("display", "inline-block")]
    in
        Html.div
            [ style ]
            [ Html.text "Item"
                , Html.input [onInput UpdateItem, Html.Attributes.value model.item] []
                , Html.br [] []
                , Html.text "Valor"
                , Html.input [onInput UpdateValor, Html.Attributes.value (model.valor |> toString)] []
                , Html.button [ onClick (PostItem model) ] [ text "adicionar" ]
            ]

viewItem : Item -> Html Msg
viewItem item =
    Html.li [] [ Html.text (item.item ++ " : " ++ (item.valor |> toString))]

viewItens : List Item -> Html Msg
viewItens items =
    let
        style = Html.Attributes.style [("width", "30%"), ("display", "inline-block")]
    in
        Html.div
            [ style ]
            [ Html.ul
                []
                (List.map viewItem items)
            ]

viewDados : Model -> Html Msg
viewDados model =
    let
        style = Html.Attributes.style [("width", "30%"), ("display", "inline-block")]
    in
        Html.div
            [ style ]
            [ Html.ul
                []
                [ Html.li
                    []
                    [ Html.text ("Orçamento: ")
                        , input [ placeholder "Orçamento", onInput ChangeOrcamento ] []
                    ]
                    , Html.li [] [ Html.text ("Precisão: " ++ toString model.precision) ]
                    , Html.li [] [ Html.text ("Squares: " ++ (model.squares |> toString)) ]
                    , Html.li [] [ Html.button [ onClick (ChangeOrcamento (model.limiteOrcamento |> toString)) ] [ text "Atualizar" ] ]
                ]
            ]

viewSquares : Orcamento -> Html Msg
viewSquares model =
    let
        style = Html.Attributes.style [("width", "500px")]
    in
        Html.div
            []
            [ Html.div [ style ] ( drawSquares model 1 []) ]


view : Model -> Html Msg
view model =
    Html.div
        []
        [ viewDados model
            , viewItens model.itens
            , viewFormItem model.form
            , viewSquares { blocos = model.squares, valor = model.precision }
            , model |> toString |> text
        ]


