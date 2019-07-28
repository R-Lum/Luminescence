#' Function to return essential quotes
#'
#' This function returns one of the collected essential quotes in the
#' growing library. If called without any parameters, a random quote is
#' returned.
#'
#' @param ID [character] (*optional*): qoute ID to be returned.
#'
#' @param separated [logical] (*with default*): return result in separated form.
#'
#' @return Returns a character with quote and respective (false) author.
#'
#' @section Function version: 0.1.4
#'
#' @author Michael Dietze, GFZ Potsdam (Germany), Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS - Université Bordeaux Montaigne (France)
#'
#' @examples
#'
#' ## ask for an arbitrary qoute
#' get_Quote()
#'
#' @md
#' @export
get_Quote <- function(
  ID,
  separated = FALSE
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
    c("A common luminescence reader customer", "If anything can go wrong, it will."),
    c("A blue LED to a trapped electron", "Resistance is futile."),
    c("A trapped electron to a yellow LED", "Well, that's all you've got?"),
    c("A weathering rock", "Who wants to live forever?"),
    c("A new pIRIR derivative", "20,000 miles below the sea."),
    c("Robert Oppenheimer", "I want this thing to work by just pressing one button."),
    c("An arbitrary member of the CRAN team", "No shirt, no shoes, no service!"),
    c("Rubber mallet to steel cylinder", "Let's rock and roll."),
    c("A data import function", "Better late than never."),
    c("A luminescence lab staff member to its customer", "Tell me the age, I tell you the price."),
    c("The NSA", "O'zapft is."),
    c("The natural dose", "You only live once."),
    c("A Windows user", "An apple a day keeps the doctor away."),
    c("The authors of sTeve", "We love to entertain you."),
    c("Any arbitrary independent OSL device manufacturer", "Sure it will work, it was me who built it!"),
    c("Response to the reviewer", "You are right, it was just a guess."),
    c("An aliquot disc", "The answer [...] is: 48"),
    c("Push Pin", "Made of used sample carriers"),
    c("A motivated R-Team member", "We are doing this not just for statistical reasons, there is real science behind it!"),
    c("An enthusiastic cabaret artist", "Political elections are like brushing teeth: if you don't do it, things become brown."),
    c("An unbiased reviewer", "The data is too poor to be published in QG, try a higher ranked journal."),
    c("R Team member, asked about statistical details", "No idea, I'm just here for the visualisation."),
    c("An arbitrary unexperienced RLum-user", "Little by little, the bird builds its nest."),
    c("The answer to life, the universe and everything", "get_rightAnswer()"),
    c("Der Tatortreiniger", "Dreck ist nur Materie am falschen Ort."),
    c("Die Ex vom Tatortreiniger", "Das Ziel ist im Weg."),
    c("Bright grain to dim grains", "I'm so shiny!"),
    c("Fast component to slow component", "Life is short!"),
    c("Fast component to slow component", "What are you waiting for?"),
    c("Violet photon to deep trap electron", "Today I'm kicking you out of your comfort zone!"),
    c("Deep trap electron to infrared photon", "Don't bother me, I need to rest."),
    c("A single grain", "I feel so lonley."),
    c("Luminescence data to Bayesian process", "Don't you ever touch me again."),
    c("Quartz grain to heating plate", "Go ahead, I need a phase change.")
    )

  ## Check input data
  if(missing(ID) == TRUE) {
    ID <- sample(x = seq(from = 1,
                         to = nrow(quotes)),
                 size = 1)
  }

  ## check for correct ID and generate qoute
  if(length(ID) < 1 | ID > nrow(quotes)) {
    quote.out <- "Sorry, but this was an impossible task!"

  } else {

    ## generate qoute(s)
    if(separated == FALSE) {
      quote.out <- paste(quotes[ID,1], ": '", quotes[ID,2], "'", sep = "")
    } else {
      quote.out <- quotes[ID,]
    }
  }

  ## return quotes
  return(quote.out)
}
