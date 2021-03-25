library(shiny)
library(RColorBrewer)
library(shinythemes)
library(shinybusy)

ui <- fluidPage(

    navbarPage(
        "Monty Hall Probability Problem",
        tabPanel("Home")),
    
    theme = shinythemes::shinytheme("paper"),
    
    tabsetPanel(type = 'tabs',
                tabPanel("About", column(width = 10, includeHTML("www/about.html"))),
                tabPanel("Game", fluidRow(
                
                    column(width = 3,
                        # conditional panels change the UI for various points in the game
                        conditionalPanel( "input.select % 3 == 0",
                            selectInput("selectDoor", label = "Select Door", 
                            choices = list("Door 1" = "1", "Door 2" = "2", "Door 3" = "3"))
                        ), conditionalPanel( "input.select % 3 == 1", 
                            htmlOutput("current"),
                            selectInput("stay", label = "Select Strategy",
                            choices = list("Stay" = TRUE, "Switch" = FALSE))
                        ), conditionalPanel("input.select % 3 == 2", 
                            htmlOutput("winner")
                        ), actionButton("select", "Continue")
                    ),
                    column(width = 9, htmlOutput("doors"))
                )),
                
                
                tabPanel("Simulation of the game", fluidRow(
                    column(width = 4,
                           textOutput("game_simulate"),
                           sliderInput("n_iter", label = "Number of Trials", min = 1,
                                       max = 30000, value = 1000)),
                    column(width = 8, plotOutput("game_simulation"))
                ))
                
    )
)


server <- function(input, output) {
    
    generate_game <- function() {

        door_num <- as.character(c(1:3))
        prizes <- sample(c("car", "goat", "goat"), size = 3)
        doors <- cbind(door_num, prizes)
        return(doors)
    }
    values <- reactiveValues()
    values$door_choice <- c(0, 0, 0)
    
    images <- list("door" = "images/transparent_door.png",
                   "car" = "images/car.png",
                   "goat" = "images/goat1.jpg")

    game_update <- function() {

        if (input$select %% 3 == 0) {
            isolate({
                values$game <- generate_game()
                values$game <- cbind(values$game, rep(images[["door"]], 3))
            })
        } else if (input$select %% 3 == 1) {
            show_door <- as.character(min(which(c(values$game[,1] != input$selectDoor & values$game[,2] != "car"))))
            new_im <- images[[values$game[as.numeric(show_door),2]]]
            isolate({
                d <- as.numeric(input$selectDoor)
                values$door_choice[d] <- values$door_choice[[d]] + 1/2
                values$show_door <- show_door
                values$game[as.numeric(show_door),3] <- new_im
            })
        } else if (input$select %% 3 == 2) {
            if (!(as.logical(input$stay))) {
                picked_door <- values$game[,1] != values$show_door & values$game[,1] != input$selectDoor
            } else {
                picked_door <- values$game[,1] == input$selectDoor
            }
            isolate({
                values$picked_door <- picked_door
                values$game[,3] <- sapply(values$game[,2], function(x){images[[x]]})
            })
        }
        values$game
    }

    output$doors <- renderText({

        values$game <- game_update()
        HTML(paste0('<div><img src="', values$game[1,3],'" height="200" width="200">',
               '<img src="', values$game[2,3],'" height="200" width="200">',
               '<img src="', values$game[3,3],'" height="200" width="200">'))
    })
    
    output$current <- renderText({
        HTML(paste('<h6><b>Your current door: ', input$selectDoor, '</b></h6>'))
    })
    
    output$winner <- renderText({
        door <- which(values$picked_door)[[1]]
        prize <- values$game[values$picked_door, 2]
        HTML(paste('<h5>You picked', door, 'and won a', prize, ', thanks for playing</h5>'))
    })
    
    #Simulation of the game to see results according to n trials.
    output$game_simulation <- renderPlot({
        
        wins <- c(0, 0) 
        for (i in c(0:input$n_iter)) {
            prizes <- sample(c("car", "goat", "goat"), size = 3)
            door_num <- c(1:3)
            doors <- cbind(door_num, prizes)
            
            selected <- sample(c(1:3), 1)
            switch <- sample(c(TRUE, FALSE), 1)
            show <- min(which(doors[,1] != selected & doors[,2] != "car"))
            if (switch) {
                selected <- doors[,1] != selected & doors[,1] != show
            }
            if (doors[selected,2] == "car") {
                switch <- switch + 1
                wins[switch] <- wins[[switch]] + 1
            }
        }
        coul <- brewer.pal(5, "Set2") 
        barplot(wins/sum(wins), names.arg = c("Stayed", "Switched"), ylim = c(0, 1), 
                main = "Percentage of total wins", col = coul)
        
        
        
    })
    
    
    output$game_simulate <- renderText({
        "Here we simulate the game n number of times. Please drag the slider to the n number of times you want to simulate the game. 
        The following graphs show the corresponding probabilties of winning given n iterations of the game.\n"
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
