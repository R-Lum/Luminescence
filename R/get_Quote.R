#' Function to return essential quotes
#'
#' This function returns one of the collected essential quotes in the 
#' growing library. If called without any parameters, a random quote is 
#' returned.
#'
#' @param ID \code{\link{character}}, qoute ID to be returned.
#' @param author \code{\link{character}}, all quotes by specified author.
#' @return Returns a character with quote and respective (false) author.
#' @section Function version: 0.1.1
#' @author Michael Dietze, GFZ Potsdam (Germany)
#' @examples
#'
#' ## ask for an arbitrary qoute
#' get_Quote()
#'
get_Quote <- function(
  ID,
  author
) {

  ## definition of the ever growing quote data set
  quotes <- rbind(
    c("Anonymous student hotel employee", "Let me double check this."),
    c("The ordinary reviewer", "I love it when a plan comes together."),
    c("A tunnelling electron", "God does not play dice."),
    c("Goldfinger", "You cannot get this machine better and cheaper than from us."),
    c("A PhD supervisor", "Live long and in prosper."),
    c("A PhD supervisor", "You are not depressive, you simply have a crappy life."),
    c("A trapped charge", "I want to break free."),
    c("The R-package Luminescence manual", "Call unto me, and I will answer thee, and will shew thee great things, and difficult, which thou knowest not."),
    c("A stimulated feldspar grain", "I'm so excited and I just can't hide it."),
    c("The true age", "How many roads..."),
    c("The undecided OSL component", "Should I stay or should I go?"),
    c("A fluvially transported quartz grain at night", "Always look at the bright side of life."),
    c("An arctic sediment outcrop", "Marmor, Stein und Eisen bricht..."),
    c("A LexSyg user", "If anything can go wrong, it will."),
    c("A blue LED to a trapped electron", "Resistance is futile.")
    )
  
  ## Check input data
  if(missing(ID) == TRUE & missing(author) == TRUE) {
    ID <- sample(x = seq(from = 1, 
                         to = nrow(quotes)), 
                 size = 1)
  } else if(missing(ID) == TRUE) {
    ID <- seq(from = 1, 
              to = nrow(quotes))[quotes[,1] == author]
  }
  
  ## check for correct ID and generate qoute
  if(length(ID) < 1 | ID > nrow(quotes)) {
    quote.out <- "Sorry, but this was an impossible task!"
  } else {
    
    ## generate qoute(s)
    quote.out <- paste(quotes[ID,1], ": '", quotes[ID,2], "'", sep = "")
    
  }

  ## return quotes
  return(quote.out)
}