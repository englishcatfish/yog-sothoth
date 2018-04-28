# Arkham Horror TCG in Prolog

Arkham Horror: The Card Game is a cooperative game based on the Cthulhu mythos of H.P. Lovecraft.  During the game the players are working together against an Agenda deck and encounter deck that act as a timer for how long the players have to complete their objectives and direct opposition to their actions.  To prepare for this, players build decks using a pool of cards following deck building limits based on their chosen character.

## Objective

The primary goal of this project is to determine the difficulty (probability of winning) of a scenario given a set of investigators with their respective decks.  The semantics of game-play will be encoded in Prolog, which will allow for an exhaustive search of the entire playable statespace.  While this space might be intractable initially, we'll consolidate redundant and prune unnecessary branches where appropriate.  

An example of redundancy, for a player, their 3 actions:

- draw a card, gain a resource, gain a resource
- gain a resource, draw a card, gain a resource
- gain a resource, gain a resource, draw a card

The three paths will end up in the same new state (except for a few cases): player has a new card and gained 2 resources.  By consolidating to one path, doesn't matter which, the statespace explosion can be minimized.  Most paths can be consolidated via redundant permutations, essentially boiling down to a combination problem.

A secondary goal is to use this as a guiding tool that lists all possible actions given a specific gamestate.  This could be helpful for introductory purposes, which would lead to a better understanding of game mechanics.

## Building and running

Setup: Ubuntu 16.04 (sub-system on Windows 10)

Dependencies: swipl

Build: make

Execute: ./game
   