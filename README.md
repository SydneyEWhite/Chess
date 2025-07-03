**Copying and editing this code for personal or educational purposes is encouraged, but copying this code and representing it as your own work is forbidden.**

# Chess
This game of chess was coded in R as a portion of a final exam in Statistical Programming Languages at Humboldt in 2015. 

## Supported Functionality 
 - All piece movements
 - All illegal moves blocked
 - Castling
 - En Passant
 - Promotion
 - Stalemate
 - Check / Checkmate

## Possible Improvements
 - Besides Stalemate, no other logic to force a draw was incorporated (as it was not required for the project)
 - If a piece tries to move to capute an opposing piece while the current king is in check and capturing the piece would not remove the king from check. The capture will be printed in the terminal, but then an error will appear saying it's illgal to leave your king in check. The opposing piece is not actually captured even though that print statement still occurs. This has no impact on the game, there is just a subtle false output in the terminal.

Chess piece images were provided by Manuel Pfeuffer.
