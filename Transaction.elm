module Transaction exposing (..)


type alias Transactions =
    List Transaction


type alias Transaction =
    { sender : String
    , receiver : String
    , amount : Int
    }
