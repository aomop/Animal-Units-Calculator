gif_ui <- function(
    id,
    color = "orange",
    textcolor = NULL,
    label = "Cheer me up",
    position = "fixed; bottom: 20px; right: 20px;"
) {
  ns <- NS(id)
  
  get_contrast_text <- function(hex_color) {
    hex_color <- gsub("#", "", hex_color)
    if (nchar(hex_color) == 3) {
      hex_color <- paste(rep(strsplit(hex_color, "")[[1]], each = 2), collapse = "")
    }
    
    r <- strtoi(substr(hex_color, 1, 2), 16L)
    g <- strtoi(substr(hex_color, 3, 4), 16L)
    b <- strtoi(substr(hex_color, 5, 6), 16L)
    
    brightness <- (r * 299 + g * 587 + b * 114) / 1000
    if (brightness > 127.5) "black" else "white"
  }
  
  if (is.null(textcolor)) {
    textcolor <- tryCatch(get_contrast_text(color), error = function(e) "white")
  }
  
  style <- paste(
    sprintf("position: %s", position),
    sprintf("background-color: %s;", color),
    sprintf("color: %s;", textcolor),
    "border-radius: 5px; padding: 10px; z-index: 1000;"
  )
  
  tags$div(
    actionButton(ns("show_gif"), label, style = style),
    style = "z-index: 1000;"
  )
}

#' Cheerful GIF Button Server Logic
#'
#' @param id A character string used as the Shiny module namespace ID.
#'
#' @return Server-side logic for displaying a random GIF and message.
#' @export
gif_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    gif_list <- c("raccoon.gif", "dogcat.gif", "elephant.gif", "cattail.gif", "sesame_street.gif", "vibrations.gif")
    title_list <- c(
      "Wow, that button never saw it coming. Incredible work.",
      
      "You're really out here doing your best, huh?",
      
      "Another click? You're a machine.",
      
      "Great choice. And by 'great' I mean 'technically valid'.",
      
      "Your dedication to clicking this button is unmatched.",
      
      "At this point, the button fears you.",
      
      "You're the hero this loser emporium never asked for.",
      
      "Statistically speaking, you're probably doing a good job.",
      
      "That click had BDE.",
      
      "Amazing. Inspiring. Possibly accidental.",
      
      "They said it couldn't be done. But then you clicked this button.",
      
      "Honestly, I didn't think you'd click it again. Yet here we are.",
      
      "This button exists solely because of you. And maybe spite.",
      
      "Is it hot in here or did your click just set the app on fire?",
      
      "A bold move cotton",
      
      "I hope you're this decisive with actual analysis.",
      
      "You're doing great work today!", 
      
      "Those plants wont know what hit 'em! (because they don't have the capacity for thought)", 
      
      "You're really good at clicking buttons :)", 
      
      "You're so smart, I don't even have to remind you to triple rinse!",
      
      "'There are no bad plants, only misplaced ones'... there are bad people, though, but you're not one of 'em!"
    )
    
    prev_gif <- reactiveVal("")
    prev_title <- reactiveVal("")
    
    observeEvent(input$show_gif, {
      cat("Gif Button Clicked!\n")
      
      repeat {
        selected_gif <- sample(gif_list, 1)
        if (selected_gif != prev_gif() || prev_gif() == "") break
      }
      
      repeat {
        selected_title <- sample(title_list, 1)
        if (selected_title != prev_title() || prev_title() == "") break
      }
      
      prev_gif(selected_gif)
      prev_title(selected_title)
      
      showModal(modalDialog(
        title = selected_title,
        tags$img(src = file.path("cheerfulgif-assets", selected_gif), width = "100%"),
        footer = modalButton("Close"),
        easyClose = TRUE,
        size = "l"
      ))
      
      cat("Selected GIF:", selected_gif, "\n")
      cat("Selected Title:", selected_title, "\n")
    })
  })
}