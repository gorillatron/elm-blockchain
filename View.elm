module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Model exposing (Model, model)
import Model exposing (Model, TransactionChain, TransactionBlock)
import Transaction exposing (Transaction)
import Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    div [ style [ ( "max-width", "960px" ) ], class "mt2 mx-auto" ]
        (List.indexedMap (blockItem model.chain) model.chain)


blockItem : TransactionChain -> Int -> TransactionBlock -> Html Msg
blockItem chain index block =
    div []
        [ h3 [] [ text ((toString index) ++ ": " ++ block.hash) ]
        , div []
            (List.indexedMap (transactionLine chain block) block.data)
        ]


transactionLine : TransactionChain -> TransactionBlock -> Int -> Transaction -> Html Msg
transactionLine indexchain block transactionindex { sender, receiver, amount } =
    div []
        [ input [ type_ "text", value sender, onInput (MutateTransactions block transactionindex "sender") ]
            []
        , span []
            [ text " sendt " ]
        , input [ type_ "text", value receiver, onInput (MutateTransactions block transactionindex "receiver") ]
            []
        , input [ type_ "number", value (toString amount), onInput (MutateTransactions block transactionindex "amount") ]
            []
        ]
