game <- function(start = T) {

  if(!start) {cat('Well, you\'re no fun.'); stop_quietly()}

  cat('Awesome! Let\'s get started.')

  name <- readline(prompt = "Okay, then, what is your name? ")

  cat(paste0('Okay, ', name, '. This game consist of answering three simple lo',
             'gic\nquestions or riddles to get an entirely underwhelming pri',
             'ze.\n'))

  moveOn <- readline(prompt = 'Are you ready (y/n)? ')

  if(moveOn %in% c('n', 'N', 'no', 'No')) {cat('Bummer'); stop_quietly()}

  counter = 0

  while (counter < 3) {

    counter = riddle(counter)

  }

}

stop_quietly <- function() {

  opt <- options(show.error.messages = FALSE)

  on.exit(options(opt))

  stop()

}

riddle <- function(counter) {



}
