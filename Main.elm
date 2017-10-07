module Main exposing (..)

import Result exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import BlockChain exposing (..)


type alias Model =
    { chain : BlockChain }


chain =
    genesis
        [ { sender = "Jørn", receiver = "Leif", amount = 10 }
        , { sender = "Leif", receiver = "Jørn", amount = 10 }
        ]


chain2 =
    add chain
        [ { sender = "Jørn", receiver = "Leif", amount = 20 }
        , { sender = "Leif", receiver = "Jørn", amount = 30 }
        ]


chain3 =
    add chain2
        [ { sender = "Jørn", receiver = "Leif", amount = 40 }
        , { sender = "Leif", receiver = "Jørn", amount = 5 }
        ]


model : Model
model =
    { chain =
        chain3
    }


type Msg
    = MutateTransactions Block Int String String


update : Msg -> Model -> Model
update msg model =
    case msg of
        MutateTransactions block transactionindex field value ->
            let
                newtransactions =
                    List.indexedMap
                        (\index candblock ->
                            if index == transactionindex then
                                case field of
                                    "amount" ->
                                        let
                                            newamount =
                                                String.toInt value
                                        in
                                            case newamount of
                                                Ok newamount ->
                                                    { candblock | amount = newamount }

                                                Err error ->
                                                    { candblock | amount = 0 }

                                    "sender" ->
                                        { candblock | sender = value }

                                    "receiver" ->
                                        { candblock | receiver = value }

                                    _ ->
                                        candblock
                            else
                                candblock
                        )
                        block.transactions
            in
                { model | chain = setTransactions model.chain block newtransactions }


view : Model -> Html Msg
view model =
    div [ style [ ( "max-width", "960px" ) ], class "mt2 mx-auto" ]
        (List.indexedMap (blockItem model.chain) model.chain)


blockItem : BlockChain -> Int -> Block -> Html Msg
blockItem chain index block =
    div []
        [ h3 [] [ text ((toString index) ++ ": " ++ block.hash) ]
        , div []
            (List.indexedMap (transactionLine chain block) block.transactions)
        ]


transactionLine : BlockChain -> Block -> Int -> Transaction -> Html Msg
transactionLine indexchain block transactionindex { sender, receiver, amount } =
    div []
        [ input [ type_ "text", value sender, onInput (MutateTransactions block transactionindex "sender") ]
            []
        , span [] [ text " sendt " ]
        , input [ type_ "text", value receiver, onInput (MutateTransactions block transactionindex "receiver") ]
            []
        , input [ type_ "number", value (toString amount), onInput (MutateTransactions block transactionindex "amount") ]
            []
        ]


main =
    Html.beginnerProgram
        { update = update, view = view, model = model }
