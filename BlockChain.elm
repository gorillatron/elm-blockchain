module BlockChain exposing (Block, BlockChain, genesis, add, setTransactions)

import Crypto.Hash exposing (sha256)


-- DATA STRUCTURES --


type alias Block a =
    { data : a
    , previousHash : Hash
    , hash : Hash
    }


type alias BlockChain a =
    List (Block a)


type alias Hash =
    String



-- METHODS --
-- Generate a fresh blockchain


genesis : a -> BlockChain a
genesis data =
    [ createBlock "GENESIS" data ]



-- Add a set of Transactions as a Block to the Blockchain


add : BlockChain a -> a -> BlockChain a
add chain data =
    case last chain of
        Nothing ->
            genesis data

        Just block ->
            chain ++ [ createBlock block.hash data ]



-- Create a block from a sett of transacttions
-- Creates a hash based on the transacations and the hash of the block comming before it
-- creating a link.


createBlock : Hash -> a -> Block a
createBlock previousHash data =
    let
        transactionsHash =
            sha256 <| toString data

        hash =
            sha256 <| toString <| ( previousHash, transactionsHash )
    in
        Block data previousHash hash



-- Sett transactions on a blockchain.
-- This will recalculate all the hashes of the blocks preceeding it.


setTransactions : BlockChain a -> Block a -> a -> BlockChain a
setTransactions chain block data =
    let
        mutatedtransactions =
            List.map
                (\candblock ->
                    if candblock == block then
                        { block | data = data }
                    else
                        candblock
                )
                chain
    in
        reHashAll mutatedtransactions



-- We fold through our block chain and rehashing it using the add function


reHashAll : BlockChain a -> BlockChain a
reHashAll chain =
    List.foldl
        (\next rest ->
            add rest next.data
        )
        []
        chain



-- Helper for getting the last item from a list


last : List a -> Maybe a
last list =
    List.head (List.reverse list)
