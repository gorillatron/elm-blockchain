module Model exposing (..)

import BlockChain exposing (..)
import Transaction exposing (Transactions, Transaction)


type alias TransactionChain =
    BlockChain Transactions


type alias TransactionBlock =
    Block Transactions


type alias Model =
    { chain : TransactionChain }


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
