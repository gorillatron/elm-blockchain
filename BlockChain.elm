module BlockChain exposing (Block, BlockChain, Transactions, Transaction, genesis, add, setTransactions)

import Crypto.Hash exposing (sha256)


-- DATA STRUCTURES --


type alias Block =
    { transactions : Transactions
    , previousHash : Hash
    , hash : Hash
    }


type alias BlockChain =
    List Block


type alias Hash =
    String


type alias Transactions =
    List Transaction


type alias Transaction =
    { sender : String
    , receiver : String
    , amount : Int
    }



-- METHODS --
-- Generate a fresh blockchain


genesis : Transactions -> BlockChain
genesis transactions =
    [ createBlock "GENESIS" transactions ]



-- Add a set of Transactions as a Block to the Blockchain


add : BlockChain -> Transactions -> BlockChain
add chain transactions =
    case last chain of
        Nothing ->
            genesis transactions

        Just block ->
            chain ++ [ createBlock block.hash transactions ]



-- Create a block from a sett of transacttions
-- Creates a hash based on the transacations and the hash of the block comming before it
-- creating a link.


createBlock : Hash -> Transactions -> Block
createBlock previousHash transactions =
    let
        transactionsHash =
            sha256 <| toString transactions

        hash =
            sha256 <| toString <| ( previousHash, transactionsHash )
    in
        Block transactions previousHash hash



-- Sett transactions on a blockchain.
-- This will recalculate all the hashes of the blocks preceeding it.


setTransactions : BlockChain -> Block -> Transactions -> BlockChain
setTransactions chain block transactions =
    let
        mutatedtransactions =
            List.map
                (\candblock ->
                    if candblock == block then
                        { block | transactions = transactions }
                    else
                        candblock
                )
                chain
    in
        reHashAll mutatedtransactions



-- We fold through our block chain and rehashing it using the add function


reHashAll : BlockChain -> BlockChain
reHashAll chain =
    List.foldl
        (\next rest ->
            add rest next.transactions
        )
        []
        chain



-- Helper for getting the last item from a list


last : List a -> Maybe a
last list =
    List.head (List.reverse list)
