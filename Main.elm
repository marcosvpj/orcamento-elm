import Html exposing (Html)
import Html.Attributes exposing (class)

--
-- Model
--
type alias Model = {
    limiteOrcamento : Int
    , itens : List String
    , precision : Int
    , squares : Int
}

model : Model
model = Model 100 [] 50 0


--
-- Update
--
type Msg 
    = Calculate Int

update : Msg -> Model -> Model
update msg model =
    case msg of
        Calculate p -> 
            { model | squares = model.limiteOrcamento // p, precision = p }


--
-- View
--

drawSquares : Model -> Int -> List (Html Msg) -> List (Html Msg)
drawSquares model iter result = 
    if model.precision * iter == model.limiteOrcamento then
        List.append result [ drawSquare model.precision ]
    else
        drawSquares model (iter + 1) (List.append result ([ drawSquare model.precision ]))

drawSquare : Int -> Html Msg
drawSquare v =
    Html.div [ Html.Attributes.class "square" ] [ Html.text (toString v) ]

view : Model -> Html Msg
view o = Html.div 
    [] 
    [   Html.text ("Orçamento: " ++ toString o.limiteOrcamento)
        , Html.div [] (drawSquares o 1 [])
    ]


--
-- Init
--
main = view model