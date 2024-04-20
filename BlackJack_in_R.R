# Define the suits, ranks, and values
suits <- c("Hearts", "Diamonds", "Spades", "Clubs")
ranks <- c("Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King", "Ace")
values <- list(
  "Two" = 2, "Three" = 3, "Four" = 4, "Five" = 5, "Six" = 6, "Seven" = 7, "Eight" = 8, "Nine" = 9, 
  "Ten" = 10, "Jack" = 10, "Queen" = 10, "King" = 10, "Ace" = 11
)

# Global playing flag
playing <- TRUE

# Card class
Card <- setRefClass(
  "Card",
  fields = list(
    suit = "character",
    rank = "character"
  ),
  methods = list(
    initialize = function(suit, rank) {
      .self$suit <- suit
      .self$rank <- rank
    },
    toString = function() {
      return(paste(.self$rank, "of", .self$suit))
    }
  )
)

# Deck class
Deck <- setRefClass(
  "Deck",
  fields = list(
    deck = "list"
  ),
  methods = list(
    initialize = function() {
      .self$deck <- list()
      for (suit in suits) {
        for (rank in ranks) {
          card <- Card$new(suit, rank)
          .self$deck[[length(.self$deck) + 1]] <- card
        }
      }
    },
    shuffle = function() {
      .self$deck <- sample(.self$deck, length(.self$deck))
    },
    deal = function() {
      return(.self$deck[[length(.self$deck)]])
    }
  )
)

# Hand class
Hand <- setRefClass(
  "Hand",
  fields = list(
    cards = "list",
    value = "numeric",
    aces = "numeric"
  ),
  methods = list(
    initialize = function() {
      .self$cards <- list()
      .self$value <- 0
      .self$aces <- 0
    },
    addCard = function(card) {
      .self$cards[[length(.self$cards) + 1]] <- card
      .self$value <- .self$value + values[[card$rank]]
      if (card$rank == "Ace") {
        .self$aces <- .self$aces + 1
      }
    },
    adjustForAce = function() {
      while (.self$value > 21 && .self$aces > 0) {
        .self$value <- .self$value - 10
        .self$aces <- .self$aces - 1
      }
    }
  )
)

# Chips class
Chips <- setRefClass(
  "Chips",
  fields = list(
    total = "numeric",
    bet = "numeric"
  ),
  methods = list(
    initialize = function() {
      .self$total <- 100  # Default total is 100
      .self$bet <- 0
    },
    winBet = function() {
      .self$total <- .self$total + .self$bet
    },
    loseBet = function() {
      .self$total <- .self$total - .self$bet
    }
  )
)

# Function to take a bet
takeBet <- function(chips) {
  repeat {
    bet <- as.numeric(readline("How many chips would you like to bet? "))
    if (is.na(bet) || bet %% 1 != 0) {
      cat("Sorry, a bet must be an integer!\n")
    } else if (bet > chips$total) {
      cat("Sorry, your bet can't exceed", chips$total, "\n")
    } else {
      chips$bet <- bet
      break
    }
  }
}

# Function to hit
hit <- function(deck, hand) {
  hand$addCard(deck$deal())
  hand$adjustForAce()
}

# Function to prompt the player to hit or stand
hitOrStand <- function(deck, hand) {
  global <- FALSE
  repeat {
    x <- readline("Would you like to Hit or Stand? Enter 'h' or 's': ")
    x <- tolower(x)
    if (x == "h") {
      hit(deck, hand)
      break
    } else if (x == "s") {
      cat("Player stands. Dealer is playing.\n")
      playing <<- FALSE
      break
    } else {
      cat("Sorry, please try again.\n")
    }
  }
}

# Function to show some cards
showSome <- function(player, dealer) {
  cat("\nDealer's Hand:\n")
  cat(" <card hidden>\n")
  cat(dealer$cards[[2]]$toString(), "\n")
  cat("\nPlayer's Hand:\n")
  for (card in player$cards) {
    cat(card$toString(), "\n")
  }
}

# Function to show all cards
showAll <- function(player, dealer) {
  cat("\nDealer's Hand:\n")
  for (card in dealer$cards) {
    cat(card$toString(), "\n")
  }
  cat("Dealer's Hand =", dealer$value, "\n")
  cat("\nPlayer's Hand:\n")
  for (card in player$cards) {
    cat(card$toString(), "\n")
  }
  cat("Player's Hand =", player$value, "\n")
}

# Main game loop
while (TRUE) {
  # Opening statement
  cat('Welcome to BlackJack! Get as close to 21 as you can without going over!\n')
  cat('Dealer hits until she reaches 17. Aces count as 1 or 11.\n')
  
  # Create and shuffle the deck, deal two cards to each player
  deck <- Deck$new()
  deck$shuffle()
  
  playerHand <- Hand$new()
  playerHand$addCard(deck$deal())
  playerHand$addCard(deck$deal())
  
  dealerHand <- Hand$new()
  dealerHand$addCard(deck$deal())
  dealerHand$addCard(deck$deal())
  
  # Set up the player's chips
  playerChips <- Chips$new()  # Default value is 100
  
  # Prompt the player for their bet
  takeBet(playerChips)
  
  # Show cards (keeping one dealer card hidden)
  showSome(playerHand, dealerHand)
  
  # Main game loop
  while (playing) {
    # Prompt player to hit or stand
    hitOrStand(deck, playerHand)
    
    # Show cards (keeping one dealer card hidden)
    showSome(playerHand, dealerHand)
    
    # If player's hand exceeds 21, run player busts function
    if (playerHand$value > 21) {
      cat("\nPlayer busts! Dealer wins!\n")
      playerChips$loseBet()
      break
    }
  }
  
  # If player hasn't busted, play dealer's hand until dealer reaches 17
  if (playerHand$value <= 21) {
    while (dealerHand$value < 17) {
      hit(deck, dealerHand)
    }
    
    # Show all cards
    showAll(playerHand, dealerHand)
    
    # Determine winner
    if (dealerHand$value > 21) {
      cat("\nDealer busts! Player wins!\n")
      playerChips$winBet()
    } else if (dealerHand$value > playerHand$value) {
      cat("\nDealer wins!\n")
      playerChips$loseBet()
    } else if (dealerHand$value < playerHand$value) {
      cat("\nPlayer wins!\n")
      playerChips$winBet()
    } else {
      cat("\nIt's a tie! Push!\n")
    }
  }
  
  # Inform player of chips total
  cat("\nPlayer's winnings stand at", playerChips$total, "\n")
  
  # Ask if the player wants to play again
  newGame <- readline("Would you like to play another hand? Enter 'y' or 'n': ")
  if (tolower(substr(newGame, 1, 1)) == 'y') {
    playing <<- TRUE
    next
  } else {
    cat("Thanks for playing!\n")
    break
  }
}
