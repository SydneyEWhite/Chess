# Sydney White
# June 2025

# sources: 
# I used chatgpt to help me start this code. I asked for an outline of code for a chess game and it gave me functions with some minimal coding inside. I've edited them extensively and deleted or replaced some functions. At least 90% of this code is hand written by me. 
# Throughout the code I used Chatgpt to find functionality or determine if something that exists in python also exists in R. In these instances, I write a comment next to the first instance of use (for example, I used "cat" for printing, and so I have a comment at the first use of "cat" to state that I found this using chatgpt). 
# I also utilized chat GPT for stuff similar to how I would type things into stack overflow in the old days. I'm sure there are probably one or two things that I forgot to cite but were based on answers for chatgpt.  
# I'm an extensive user of the "help" panel in R studio, but I generally don't site that since it's just documentation for the packages

# Thank you to my friends for helping me QA my code
# Paul Buttkuss
# Naomi Scheithe 
# Emma Higuera Nunez 

# NOTE: The only draw condition implemented in this code is stalemate since stalemate was the only draw condition mentioned in the instructions. Confirmed that this is correct by Manuel Pfeuffer.

# install relevant packages
install.packages("png")
install.packages("raster")
library(png)
library(raster)

# user should set their own working directory
# setwd("C:/Users/sw626b/Documents/School")

### Initialization of pieces
pieces <- c('white_king', 'white_queen', 'white_rook', 'white_bishop', 'white_knight', 'white_pawn', 'black_king', 'black_queen', 'black_rook', 'black_bishop', 'black_knight', 'black_pawn')
pieces_list <- list ()
for (i in 1:length (pieces)) {
  temp_array <- array(0, dim = c(45, 45, 4))
  temp_png <- readPNG(paste0 ('pieces/', pieces [i], '.png '))
  temp_array[,,1:3] <- temp_png[,,1]
  temp_array[,,4] <- (temp_png[,,1] + temp_png [,,2]) != 0
  pieces_list[[i]] <- temp_array
  names(pieces_list)[i] <- pieces[i]
}

### Initialization of game_state df with all pieces and initial positions
all_pieces <- c('white_king', 'white_queen', rep('white_rook',2), rep('white_bishop',2), rep('white_knight',2), rep('white_pawn',8), 'black_king', 'black_queen', rep('black_rook',2), rep('black_bishop',2), rep('black_knight',2), rep('black_pawn',8))
xpos <- c("e","d","a","h","c","f","b","g","e","d","a","h","c","f","b","g","e","d","a","h","c","f","b","g","e","d","a","h","c","f","b","g") # I know there is a better way to do this, but I'm procrastinating on the actual coding
ypos <- c(rep(1,8), rep(2,8), rep(8,8), rep(7,8)) 
moved <- rep(FALSE,32)
en_passant <- rep(FALSE,32)
game_state = data.frame(all_pieces, xpos, ypos, moved, en_passant)
colnames(game_state) <- c("pieces", "xpos", "ypos", "moved", "en_passant")
xpos_dict = list("a"=1, "b"=2, "c"=3, "d"=4, "e"=5, "f"=6, "g"=7, "h"=8) #I'm a python girly, so I refuse to name this a list

### Draw chessboard
draw_chessboard <- function(game_state) {
  board_colors <- matrix(rep(c(rep(c("#FFCE9E", "#D18B47"), times=4), rep(c("#D18B47", "#FFCE9E"), times=4)), times=4), nrow=8, byrow=TRUE) #matrix of colors for the board -- I believe next semester you should allow us to pick our own colors since I'll have to look at them while I code this monster
  image(1:8, 1:8, matrix(1:64, 8) , col=board_colors, xlim = c(0,9), ylim = c(0,9), xlab = "", ylab = "", axes=FALSE) #actual board
  axis(side=1, at = c(1,2,3,4,5,6,7,8), labels = c("a","b","c","d","e","f","g","h")) #yxaxis
  axis(side=2, at = c(1,2,3,4,5,6,7,8), labels = c(1,2,3,4,5,6,7,8)) #yaxis
  
  # plot the grainy pngs of chess pieces 
  apply(game_state, 1, function(row) { rasterImage(pieces_list[[row[["pieces"]]]], xleft=xpos_dict[[row[["xpos"]]]]-0.5, xright=xpos_dict[[row[["xpos"]]]]+0.5, ybottom=as.numeric(row[["ypos"]])-0.5, ytop=as.numeric(row[["ypos"]])+0.5)}) #some things are cast as numeric because the r dataframe has them as strings 
}

### function to intake the input from the user and determine if it is appropriate 
check_input <- function(current_player) {
  # use of "repeat" was taken from chatgpt after I asked if I should use a while loop to chatgpt to continually surface a question to a user if they give an invalid response
  repeat {
    cat(paste("It's", current_player, "turn. Enter your move (e.g., a2a4): ")) # using cat instead of printing was done because it was done by chatgpt
    input <- tolower(scan(what=character(), n=1, quiet=TRUE)) # help from chatgpt
    
    try({
      x1 <- substr(input, 1, 1)
      y1 <- as.integer(substr(input, 2, 2))
      x2 <- substr(input, 3, 3)
      y2 <- as.integer(substr(input, 4, 4))})
    
    if(nchar(input)!=4) {
    }
    # scrape needed coordinates
    else if (grepl("^[a-h][1-8][a-h][1-8]$", input)) { # using both the ^ to anchor to the front and $ to anchor to the back is from chatgpt
      return(list(x1=x1, y1=y1, x2=x2, y2=y2))
    # error message if end point is off the board
    } else if (((y1 < 0 || y1 > 8) || (y2 < 0 || y2 > 8)) || (!(x1 %in% letters[1:8]) || !(x2 %in% letters[1:8]))) {
      cat("Your move is illegal since it would put a piece outside of the board!\n") 
      next
    }
    
    # error message if move is invalid
    cat(sprintf("Your move %s is invalid and must have the form 'LetterDigitLetterDigit'! (without spaces)\n", input)) # sprintf was taken from chatgpt when I asked about formatting strings in r outputs 
  }
}

### This function checks to see if there is a legitimate piece on the starting tile
get_piece <- function(xpos, ypos, color, game_state) {
  piece_index <- which(game_state$xpos == xpos & game_state$ypos == ypos)
  piece <- game_state$pieces[piece_index]
  piece_color <- ifelse(grepl("^w", piece), "white", "black")
  
  if (length(piece_index) == 0) {
    cat("There exists no piece on your selected tile!\n")
    return(NULL)
  }
  
  if (piece_color != color) {
    cat(sprintf("The piece you chose does not belong to your color %s!\n", color))
    return(NULL)
  }
  
  return(piece)
}


### This function checks to see if a move is valid depending on the piece and color
is_valid_move <- function(x1, y1, x2, y2, piece, game_state, print_error = TRUE) {
  # Start and end are same
  if (x1 == x2 & y1 == y2) {
    if (print_error) cat("Your start and end position cannot be the same tile!\n")
    return(FALSE)
  }
  
  # defining variables to be used
  x1_letter <- x1
  x2_letter <- x2
  
  x1 <- xpos_dict[[x1]]
  x2 <- xpos_dict[[x2]]
  
  piece_color <- sub("_.*", "", piece)
  piece_type <- sub(".*_", "", piece)
  
  # pawn moves
  if(piece_type == 'pawn') { ## originally had separate ifs for black and white, but then it started to get really convoluted when I was adding more logic about moving diagonally to capture and not moving forward to capture, so I asked chatgpt to help me clean it up and used some suggestions (this was before adding en passant or anything crazy)
    end_block_piece <- game_state[game_state$xpos == x2_letter & game_state$ypos == y2, ]
    end_block_piece_exists <- (nrow(end_block_piece) > 0)
    
    en_passant_df <- game_state[game_state$en_passant == TRUE & sub("_.*", "", game_state$pieces) != piece_color,]
    if (exists("en_passant_df") && is.data.frame(en_passant_df) && nrow(en_passant_df) > 0) {en_passant_possible = 1} else {en_passant_possible = 0}
    
    direction <- ifelse(piece_color == "white", 1, -1)
    starting_row <- ifelse(piece_color == "white", 2, 7)
    
    # check general direction
    if (y2 < y1 && piece_color == "white") {
      if (print_error) cat("A white pawn can only move upwards!\n") 
      return(FALSE)
    } else if (y2 > y1 && piece_color == "black") {
      if (print_error) cat("A black pawn can only move downwards!\n") 
      return(FALSE)
      
      # check diagonal capture
    } else if (abs(x1-x2) == 1 && (y2-y1) == direction) {
      # check if en_passant possible
      if (en_passant_possible == 1) {
        if (en_passant_df$ypos == as.character(y1) & en_passant_df$xpos == x2_letter){
          if (print_error) cat("You have captured an opposing pawn via en passant! \n") 
          game_state <- game_state[!(((game_state$xpos == en_passant_df$xpos & game_state$ypos == en_passant_df$ypos) )), ]
          # check for normal diagonal capture
        } else if (!end_block_piece_exists || sub("_.*", "", end_block_piece$pieces) == piece_color) {
          if (print_error) cat("A pawn cannot change its column unless it can capture!\n")
          return(FALSE)}
        # check for normal diagonal capture
      } else if (!end_block_piece_exists || sub("_.*", "", end_block_piece$pieces) == piece_color) {
        if (print_error) cat("A pawn cannot change its column unless it can capture!\n")
        return(FALSE)}
      # check for generally illegal diagonal move
    } else if (abs(x1-x2) == 1 && (y2-y1) != direction) {
      if (print_error) cat("The chosen end tile cannot be reached by your chosen piece in one legal move !\n")
      return(FALSE)
      
      # move pawn forward
    } else if (abs(x1-x2) == 0) {    
      # one step forward 
      if (end_block_piece_exists) {
        if (print_error) cat("A pawn cannot move forward into an occupied space!\n")
        return(FALSE)
      } else if ((y2-y1) == direction) {
        # Two steps forward from starting position
      } else if ((y2-y1) == 2 * direction && y1 != starting_row) {
        if (print_error) cat("A pawn can only advance two tiles if it has not been moved before!\n")
        return(FALSE)
        # more steps forward #illegal
      } else if (abs(y2-y1) > 2) {
        if (print_error) cat("A pawn cannot move more than 2 tiles!\n")
        return(FALSE)
      } else if ((y2-y1) == 2 * direction && y1 == starting_row) {
        game_state[game_state$xpos == x1_letter & game_state$ypos == y1, c("en_passant")] <- TRUE # add marker to show that the pawn could be captured by en passant
      }
      
      # All other moves are illegal
    } else {
      if (print_error) cat("The chosen end tile cannot be reached by your chosen piece in one legal move !\n")
      return(FALSE)}
    
    # rook 
  } else if (piece_type == 'rook'){
    # straight
    if (!((x1==x2) | (y1==y2))) {
      if (print_error) cat("The Rooks can only move upward/downward or side to side!\n")
      return(FALSE)
    }
    
    # knight
  } else if (piece_type == 'knight'){
    # L-shape
    if (!(((abs(x1-x2) + abs(y1-y2) == 3) & (abs(x1-x2)<3)) & (abs(y1-y2)<3))) {
      if (print_error) cat("The knight can only move in an L shape while one side of the L has a length of 2 and the other one of 1!\n")
      return(FALSE)
    }
    
    # bishop 
  } else if (piece_type == 'bishop'){
    # diagonal
    if (!(abs(x1-x2) == abs(y1-y2))) {
      if (print_error) cat("The Bishops can only move diagonally!\n")
      return(FALSE)
    }
    
    # king
  } else if (piece_type == 'king'){
    # castling
    if((y1 == y2 & abs(x2 - x1) == 2) & print_error == TRUE) {     # print_error == true prevents it from being checked as a possibility when currently in check
      if (is_checked(game_state, piece_color)) {
        cat("You cannot castle out of, through, or into check!\n")
        return(FALSE)
      }
      return(check_castle(x1_letter, y1, x2_letter, y2, piece, game_state))
      # one space
    } else if (!(((abs(x1-x2) + abs(y1-y2)) <= 2) && ((abs(x1-x2) <=1) && (abs(y1-y2) <=1)))) {
      if (print_error) cat("The chosen end tile cannot be reached by the king in one legal move!\n")
      return(FALSE)
    }
    
    # queen
  } else if (piece_type == 'queen'){
    # rook or bishop
    if (!((abs(x1-x2) == abs(y1-y2) | ((x1==x2) | (y1==y2))))) {
      if (print_error) cat("The Queen can only move by line of sight!\n")
      return(FALSE)
    }
  }
  
  # Check if path is blocked
  clear_path <- check_path(x1, y1, x2, y2, piece_color, piece_type, game_state, print_error = print_error)
  if (clear_path == FALSE) {
    return(FALSE)
  }
  
  # Check capture
  game_state <- check_capture(x1_letter, y1, x2_letter, y2, piece_color, piece_type, game_state, print_error = print_error)
  
  # Return game_state if all valid
  return(game_state)
}

### This function checks the capture of another piece (called within is_valid_move)
check_capture <- function(x1, y1, x2, y2, piece_color, piece_type, game_state, print_error = TRUE) {
  # get end block piece
  end_block_piece <- game_state[game_state$xpos == x2 & game_state$ypos == y2, ]
  
  # if not end block piece, do nothing
  if (nrow(end_block_piece) == 0) {
    # Prevent capture of your own piece
  } else if (piece_color == sub("_.*", "", end_block_piece$pieces)) { 
    if (print_error) cat("You cannot move onto a tile that is already occupied by one of your pieces!\n") 
    return(FALSE)
    # successful capture
  } else {
    if (print_error) cat(sprintf("You have captured %s ! \n", paste(end_block_piece$pieces)))
    game_state <- game_state[!(((game_state$xpos == x2 & game_state$ypos == y2) & (game_state$pieces == end_block_piece$pieces))), ]
  }
  return(game_state)
}

# This function checks if the path is clear (called within is_valid_move)
check_path <- function(x1, y1, x2, y2, piece_color, piece_type, game_state, print_error = TRUE) {
  # create an empty dataframe (got this line from chatgpt when I was trying to find a way to create an empty dataframe -- Null had caused issues)
  blocked_path = game_state[0, ]
  
  # pawn
  if (piece_type == 'pawn'){
    if (abs(y2 - y1) == 2) {
      y_range <- seq(min(y1,y2)+1, max(y1,y2)-1)
      blocked_path = game_state[game_state$xpos == letters[x1] & game_state$ypos %in% y_range, ]
    }
    # rook 
  } else if (piece_type == 'rook'){
    if (x1==x2) { # check y if they stay in the same column
      if (abs(y1-y2) > 1) {
        y_range <- seq(min(y1,y2)+1, max(y1,y2)-1)
        blocked_path = game_state[game_state$xpos == letters[x1] & game_state$ypos %in% y_range, ] }
    } else if (y1==y2) { # check x if they stay in the same row
      if (abs(x1-x2) > 1) {
        x_range <- seq(min(x1,x2)+1, max(x1,x2)-1)
        x_letters <- letters[x_range]
        blocked_path = game_state[game_state$xpos %in% x_letters & game_state$ypos == y1, ] }
    }
    # knight
  } else if (piece_type == 'knight'){
    # L-shape - can jump
    # bishop
  } else if (piece_type == 'bishop'){ 
    # diagonal
    # got this section  from chatgpt because I was having issues iterating diagonally
    dx <- ifelse(x2 > x1, 1, -1)
    dy <- ifelse(y2 > y1, 1, -1)
    if (abs(x1 - x2) > 1) { 
      x_seq <- seq(x1 + dx, x2 - dx, by = dx)
      x_letters <- letters[x_seq]
      y_seq <- seq(y1 + dy, y2 - dy, by = dy)
      for (i in seq_along(y_seq)) {
        if (any((game_state$xpos == (x_letters[i])) & (game_state$ypos == (y_seq[i])))) {
          if (print_error) cat("The path is blocked by another piece!\n")
          return(FALSE)  # Blocked
        }
      }
    }
    # king
  } else if (piece_type == 'king'){
    # one space - not needed
    # castling
    if (abs(x2 - x1) == 2) {
      x_range <- seq(min(x1,x2)+1, max(x1,x2)-1)
      x_letters <- letters[x_range]
      blocked_path = game_state[game_state$xpos %in% x_letters & game_state$ypos == y1, ]
    }
    # queen
  } else if (piece_type == 'queen'){
    # rook like move
    if (x1==x2) {
      if (abs(y1-y2) > 1) {
        y_range <- seq(min(y1,y2)+1, max(y1,y2)-1)
        blocked_path = game_state[game_state$xpos == letters[x1] & game_state$ypos %in% y_range, ] }
    } else if (y1==y2) {
      if (abs(x1-x2) > 1) {
        x_range <- seq(min(x1,x2)+1, max(x1,x2)-1)
        x_letters <- letters[x_range]
        blocked_path = game_state[game_state$xpos %in% x_letters & game_state$ypos == y1, ] }
      # bishop like move
    } else {
      dx <- ifelse(x2 > x1, 1, -1) # from the bishop section
      dy <- ifelse(y2 > y1, 1, -1)
      if (abs(x1 - x2) > 1) { 
        x_seq <- seq(x1 + dx, x2 - dx, by = dx) 
        x_letters <- letters[x_seq]
        y_seq <- seq(y1 + dy, y2 - dy, by = dy)
        for (i in seq_along(y_seq)) {
          if (any((game_state$xpos == x_letters[i]) & (game_state$ypos == y_seq[i]))) {
            if (print_error) cat("The path is blocked by another piece!\n")
            return(FALSE)  # Blocked
          }
        }
      }
    }
  }
  
  # blocked_path is now a dataframe with any pieces that are blocking the requested path
  if (nrow(blocked_path) > 0) {
    #if (print_error) print(blocked_path)
    if (print_error) cat("The path is blocked by another piece!\n")
    return(FALSE)
  } else {return(TRUE)}
}

# function to check if game is over (called in chess function)
is_game_over <- function(game_state, current_player) { 
  
  # check checkmate 
  next_color <- ifelse(current_player == "white", "black", "white")
  remaining_pieces <- game_state[sub("_.*", "", game_state$pieces) == next_color, ] 
  
  for (row in 1:nrow(remaining_pieces)) {
    x1 <- remaining_pieces[row, "xpos"]
    y1 <- as.numeric(remaining_pieces[row, "ypos"]) 
    piece <- remaining_pieces[row, "pieces"]
    for (x2 in letters[1:8]) {
      for (y2 in (1:8)) {
        valid_move <- is_valid_move(x1, y1, x2, y2, piece, game_state, print_error = FALSE)
        if (!identical(valid_move, FALSE)) { # got the use of "identical" from chatgpt because my origional implementation of just having valid_move == False caused warnings since sometimes it was comparing a bool to a df
          proposed_state <- valid_move[valid_move$xpos == x1 & valid_move$ypos == y1, c("xpos", "ypos")] <- c(x2, y2)
          if(!is_checked(valid_move, next_color)) {
            return(FALSE)
          }
        }
      }
    }
  }
  
  # check stalemate -- from above we know there's no legal move
  # these next 3 lines are from chatgpt (slightly edited) because my brain has stopped functioning
  # Check if current_player is in check
  in_check <- (is_checked(game_state, next_color))
  if (!in_check) {
    return(0) #stalemate
  } else {
    return(1) #checkmate
  }
}

### function to check if the king is in check (called in chess function)
is_checked <- function(game_state, color) {
  king <- paste0(color, "_king")
  king_pos <- game_state[game_state$pieces == king, c("xpos", "ypos")]
  x2_king <- king_pos$xpos
  y2_king <- as.numeric(king_pos$ypos)
  
  # get all the opposing pieces
  opponent_color <- ifelse(color == "white", "black", "white")
  opponent_pieces <- game_state[sub("_.*", "", game_state$pieces) == opponent_color, ] 
  
  for (row in 1:nrow(opponent_pieces)) { # used https://campus.datacamp.com/courses/intermediate-r-for-finance/loops-3?ex=10 to figure out how to loop over df in r
    x1 <- opponent_pieces[row, "xpos"]
    y1 <- as.numeric(opponent_pieces[row, "ypos"]) # setting as numeric since df has character
    piece <- opponent_pieces[row, "pieces"]
    if (!identical(is_valid_move(x1, y1, x2_king, y2_king, piece, game_state, print_error = FALSE), FALSE)) {
      return(TRUE)
    }
  }  
  return(FALSE)
  
}

### function to check castle (called in is_valid_move)
check_castle <- function(x1, y1, x2, y2, piece, game_state){
  piece_color <- sub("_.*", "", piece)
  castle_side <- ifelse(xpos_dict[[x2]] - xpos_dict[[x1]] > 0, "h", "a")
  king_mid <- ifelse(xpos_dict[[x2]] - xpos_dict[[x1]] > 0, "f", "d")
  rook_end <- ifelse(xpos_dict[[x2]] - xpos_dict[[x1]] > 0, "f", "d")
  castle_row <- ifelse(piece_color == "white", 1, 8)
  
  # check end block
  if (nrow(game_state[game_state$xpos == x2 & game_state$ypos == y2, ]) != 0) {
    cat("The end space must be free when castling!\n") # googled if this was a rule because I did it while QAing and wasn't sure
    return(FALSE)
  }  
  
  # check the king
  if (game_state[(sub("_.*", "", game_state$pieces) == piece_color & sub(".*_", "", game_state$pieces)== 'king') & (game_state$xpos == x1 & game_state$ypos == y1), "moved"] == TRUE) {
    cat("You cannot castle anymore since you have moved your king already!\n")
    return(FALSE)
  }
  
  # check the rooks 
  if(nrow(game_state[(game_state$ypos == castle_row & sub(".*_", "", game_state$pieces)== 'rook') & (game_state$xpos == castle_side ), ]) == 0) {
    cat("There is no rook you can castle to!\n")
    return(FALSE)
  } else if (nrow(game_state[(game_state$ypos == castle_row & sub(".*_", "", game_state$pieces)== 'rook') & (game_state$xpos == castle_side & sub("_.*", "", game_state$pieces) == piece_color), ]) == 0) {
    cat("There is no rook of your color you can castle with!\n")
    return(FALSE)
  } else if (nrow(game_state[((game_state$ypos == castle_row & game_state$xpos == castle_side) & (sub(".*_", "", game_state$pieces)== 'rook' & sub("_.*", "", game_state$pieces) == piece_color)) & game_state$moved == FALSE, ]) == 0) {
    cat("This rook has already moved which means that you cannot use it to castle!\n")
    return(FALSE)
  }
  
  # check the path
  if (check_path(xpos_dict[[x1]], y1, xpos_dict[[x2]], y2, piece_color, "king", game_state, print_error = FALSE) == FALSE) {
    cat("There is a piece on the second column blocking the castle!\n")
    return(FALSE)
  }
  
  # check that the middle piece wouldn't put the king in check
  mid_castle_state <- game_state
  mid_castle_state[(mid_castle_state$xpos == x1 & mid_castle_state$ypos == y1), c("xpos", "ypos")] <- c(king_mid, castle_row)
  if (is_checked(mid_castle_state, piece_color)){
    cat("The king can't castle out of, through, or into check!\n")
    return(FALSE)
  }
  
  game_state[(game_state$xpos == x1 & game_state$ypos == y1) & sub(".*_", "", game_state$pieces) == 'king', c("xpos", "ypos")] <- c(x2, y2)
  game_state[(game_state$xpos == castle_side & game_state$ypos == castle_row) & sub(".*_", "", game_state$pieces) == 'rook', c("xpos", "ypos")] <- c(rook_end, castle_row)
  return(game_state)
  
}

### check if the pawn has made it to the other side of the board (called in chess function)
check_promotion <- function(x1, y1, x2, y2, piece, proposed_game_state){
  piece_color <- sub("_.*", "", piece)
  piece_type <- sub(".*_", "", piece)
  
  end_board <- ifelse(piece_color == "white", 8, 1)
  
  ### ask for the piece of choice
  if (piece_type == "pawn" & y2 == end_board) {
    repeat {
      cat(paste('What piece do you want your pawn to promote into? Type "queen", "rook", "bishop", or "knight".\n'))
      promoted_piece <- tolower(scan(what=character(), n=1, quiet=TRUE))
      
      if (!(promoted_piece %in% c("queen", "rook", "bishop", "knight"))) { 
        cat(sprintf('The piece %s, is not valid. Type "queen", "rook", "bishop", or "knight".\n', promoted_piece)) 
      } else {return(promoted_piece)}
    }
    
  } else {return(FALSE)}
}

### bringing everything together
chess <- function(game_state) {
  # Initialize game parameters
  current_player <- "white"
  moves <- 0
  while (TRUE) {
    draw_chessboard(game_state)
    
    # reset the en_passant to false since the turn for the opposing player has passed    
    game_state[sub("_.*", "", game_state$pieces) == current_player, c("en_passant")] <- c(FALSE)
    
    # Check Input
    coords <- check_input(current_player) 
    
    # Check Piece
    piece <- get_piece(coords$x1, coords$y1, current_player, game_state) 
    if (is.null(piece)) next # got the "next" functionality from chatgpt
    
    # check if the move is legit, then output a proposed game_state where captures are included
    proposed_game_state <- is_valid_move(coords$x1, coords$y1, coords$x2, coords$y2, piece, game_state) 
    if (identical(proposed_game_state, FALSE)) next
    
    # update proposed_game_state to have the piece actually moved
    proposed_game_state[proposed_game_state$xpos == coords$x1 & proposed_game_state$ypos == coords$y1, c("xpos", "ypos","moved")] <- c(coords$x2, coords$y2, TRUE)
    
    # check for promotion
    promoted_piece <- check_promotion(coords$x1, coords$y1, coords$x2, coords$y2, piece, game_state)
    if(!identical(promoted_piece, FALSE)) {
      proposed_game_state[(proposed_game_state$xpos == coords$x2 & proposed_game_state$ypos == coords$y2), c("pieces")] <- paste0(current_player, "_", promoted_piece)
    }
    
    # did you accidentally move into check? 
    if (is_checked(proposed_game_state, current_player)) {
      cat("This move would leave your king in check which is illegal! Choose another move!\n")
      next
    }

    # Execute the move 
    game_state <- proposed_game_state 
    moves <- moves + 1
        
    # did you put opponent into check?
    opponent <- ifelse(current_player == "white", "black", "white")
    if (is_checked(proposed_game_state, opponent)) {
      cat("Check!\n")
    }

    # Check end conditions (checkmate, stalemate)
    game_over <- is_game_over(game_state, current_player)
    if (!identical(game_over, FALSE)) {
      draw_chessboard(game_state)
      if (game_over == 0) {
        cat(sprintf("There are no more possible moves for %s after %s moves. The game ends in a draw!", opponent, moves))
        break
      } else if (game_over == 1) {
        cat(sprintf("Player %s checkmated %s in %s moves! Congratulations player %s!\n", current_player, opponent, moves, current_player))
        break
      }
    }
    
    # Swap player
    current_player <- ifelse(current_player == "white", "black", "white")
  }
}

chess(game_state)

