# 3110Project

A command line implementation of the classic board game "MONOPOLY" in OCaml!

Read the install file for instructions on how to play.

Authors:
- Nicolas Vega (nav23)
- Haashim Shah (hhs66)
- Matthew Simon (mls498)



Our project is a command line interface impementation of the classic board game
"MONOPOLY"!

# Installation
1. Download the zip file
2. Unzip the file 
3. Change directory into the unzipped directory (3110Project).

# Playing the game
1. Make the terminal full screen
2. type "make build" in to the command line
3. type "make play" in to the command line
4. The game should have started. Enter any distinct name for each player

The game will print a new board to the command line whenever a player 
takes a new action. 

Follow the prompts. Here are the main commands:

- "roll": rolls for the player and moves them n forward spaces where
      n is the number they rolled. 

- "roll n": Like command role except the amount of the roll can be specified.
      Example: "roll 10". We included this in order to make play testing easier
      for the grader (so that they can produce certain scenarios more easily).
      Do not pass in 0 or a negative number as this violates a precondition
      for the roll phase. 

- "buy": buys the current property the player is standing on.

- "pass": passes on purchasing the current property the player is on. Triggers 
      an auction.

- "buy [1-5] house/houses on [prop name]": buys specified number of houses on
      the property specified if possible.

- "sell [1-5] house/houses on [prop name]": sells the specified number of houses
      on the property specified if possible.

- "mortgage [prop name]": mortgages the property specified if possible

- "unmortgage [prop name]": unmortgages the property specified if possible

- "end turn": used to end the current player's turn during their final phase

- "pay": use when the player lands on a tax property or on another player's
      property to pay the required tax, rent, or jail fine.

- "check properties": use to check the current player's owned properties.

- "check money": use to check the current player's money balance.

- "declare bankruptcy": Can be used to retire the current player from the game.
      Typically used when player does not have enough money/assets to 
      pay a required amount/continue playing the game. Must be on landing pay.

- "trade with [player name]": initiates a trade offer to the player specified.
      Triggers the trading phase.

- "trade [property name|$amt] for [property name|$amt]": Command to complete a
      trade. Specify the property name(s) or cash amount. Make sure the cash
      has a "$" before the amount (example: $100). Trading a property for a 
      property or cash for a property is allowed but trading cash for cash is 
      not allowed.

- "cancel": exits trade phase without doing a trade. 

- "quit": ends the current monoploy game and exits the game.
