module Update exposing (..)

import Model exposing (Model, TransactionChain, TransactionBlock)
import BlockChain exposing (..)


update : Msg -> Model -> Model
update msg model =
    case msg of
        MutateTransactions block transactionindex field value ->
            let
                newtransactions =
                    List.indexedMap
                        (\index transaction ->
                            if index == transactionindex then
                                case field of
                                    "amount" ->
                                        let
                                            newamount =
                                                String.toInt value
                                        in
                                            case newamount of
                                                Ok newamount ->
                                                    { transaction | amount = newamount }

                                                Err error ->
                                                    { transaction | amount = 0 }

                                    "sender" ->
                                        { transaction | sender = value }

                                    "receiver" ->
                                        { transaction | receiver = value }

                                    _ ->
                                        transaction
                            else
                                transaction
                        )
                        block.data
            in
                { model | chain = setData model.chain block newtransactions }


type Msg
    = MutateTransactions TransactionBlock Int String String
