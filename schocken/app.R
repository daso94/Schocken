                #
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

######## Global Variables to synchronize game status across Players

# die values
A = matrix(data = rep(7, 18), ncol = 3)
# unveiled?
B = matrix(data = rep(FALSE, 18), ncol = 3)
# Points
P = rep(0,6)
# eligible to switch 6,6 to 1?
elig = rep(F, 6)
# rolls
rolls = rep(3, 6)


# Pictures of dice
pics = c('Alea_1.png', 'Alea_2.png', 'Alea_3.png', 'Alea_4.png', 'Alea_5.png', 'Alea_6.png', 'Alea_0.png')


# roll the dice 
roll = function(user_id, input){
    rolls[user_id] <<- (rolls[user_id] - 1)%%3
    shinyjs::hide(paste0("switch_die", user_id))
    sixes = 0
    elig[user_id] <<- FALSE
    if(!eval(parse(text = paste0("input$check", user_id, "1")))){
        A[user_id, 1] <<- sample(1:6, 1)
        B[user_id, 1] <<- FALSE
        
        if(A[user_id, 1] == 6){
            sixes = sixes + 1
        }
    }
    if(!eval(parse(text = paste0("input$check", user_id, "2")))){
        A[user_id, 2] <<- sample(1:6, 1)
        B[user_id, 2] <<- FALSE
        
        if(A[user_id, 2] == 6){
            sixes = sixes + 1
        }
    }
    if(!eval(parse(text = paste0("input$check", user_id, "3")))){
        A[user_id, 3] <<- sample(1:6, 1)
        B[user_id, 3] <<- FALSE
        
        if(A[user_id, 3] == 6){
            sixes = sixes + 1
        }
    }
    
    if(sixes > 1){
        elig[user_id] <<- TRUE
    }
    
    
}

# switch 6,6 --> 1
switch_die = function(user_id, input, session){
    if((!eval(parse(text = paste0("input$check", user_id, "1")))) & A[user_id, 1] == 6){
        A[user_id, 1] <<- 1
        updateCheckboxInput(session, paste0("check", user_id, "1"), value = TRUE)
    }
    else{
            A[user_id, 2] <<- 1
            updateCheckboxInput(session, paste0("check", user_id, "2"), value = TRUE)

        }
}
# unveil dice
unveil = function(user_id){
    B[user_id, 1] <<- TRUE
    B[user_id, 2] <<- TRUE
    B[user_id, 3] <<- TRUE
    
    if(elig[user_id]){
        shinyjs::show(paste0("switch_die", user_id))
    }
}


# update Points ("Bierdeckel")
update_P = function(user_id, pts){
    P[user_id] <<- pts
}


# Define UI
ui <- fluidPage(

    useShinyjs(),

    # Application title
    titlePanel("Schocken"),
    #pre("                       Welchen Würfel behalten?                                                                                                        Würfe         Punkte"),
    fluidRow(column(1,""),
             column(3,"Welchen Würfel behalten?"), 
             column(3,""),
             column(1, uiOutput("frame")),
             column(1,"Punkte")),
    
    sidebarLayout(
        
    sidebarPanel(
   # Player 1 Control Panel
 #   div(style="margin-bottom:10px"),
    fluidRow(column(3,actionButton("roll1", "Würfeln!"),
                    actionButton("unveil1", "Aufdecken!")), 
    #keep Die 1-3
    column(2, checkboxInput("check11","1")), 
    column(2, checkboxInput("check12","2")),
    column(2, checkboxInput("check13","3")),
    #Points
    column(3,textInput("pts1","Punkte",0))),
    
    div(style="margin-bottom:40px"),
    
    #Player 2 Control Panel
    fluidRow(column(3,actionButton("roll2", "Würfeln!"),actionButton("unveil2", "Aufdecken!")),
             column(2, checkboxInput("check21","1")), 
             column(2, checkboxInput("check22","2")),
             column(2, checkboxInput("check23","3")),
             column(3,textInput("pts2","Punkte",0))),
    
    div(style="margin-bottom:40px"),
    
    #Player 3 Control Panel
    fluidRow(column(3,actionButton("roll3", "Würfeln!"),actionButton("unveil3", "Aufdecken!")),
             column(2, checkboxInput("check31","1")), 
             column(2, checkboxInput("check32","2")),
             column(2, checkboxInput("check33","3")),
             column(3,textInput("pts3","Punkte",0))),
    
    div(style="margin-bottom:40px"),
    
    
    #Player 4 Control Panel
    fluidRow(column(3,actionButton("roll4", "Würfeln!"),actionButton("unveil4", "Aufdecken!")),
             column(2, checkboxInput("check41","1")), 
             column(2, checkboxInput("check42","2")),
             column(2, checkboxInput("check43","3")),
             column(3,textInput("pts4","Punkte",0))),
    
    div(style="margin-bottom:40px"),
    
    # Player 5 Control Panel
    fluidRow(column(3,actionButton("roll5", "Würfeln!"),actionButton("unveil5", "Aufdecken!")),
             column(2, checkboxInput("check51","1")), 
             column(2, checkboxInput("check52","2")),
             column(2, checkboxInput("check53","3")),
             column(3,textInput("pts5","Punkte",0))),
    
    div(style="margin-bottom:40px"),
    
    
    # Player 6 Control Panel
    fluidRow(column(3,actionButton("roll6", "Würfeln!"),actionButton("unveil6", "Aufdecken!")),
             column(2, checkboxInput("check61","1")), 
             column(2, checkboxInput("check62","2")),
             column(2, checkboxInput("check63","3")),
             column(3,textInput("pts6","Punkte",0))),
    
    div(style="margin-bottom:40px")),
    
    
    
   
   mainPanel(
       # credits
       #tags$a(href="https://commons.wikimedia.org/wiki/Category:Dice_faces#/", "Dice Images"),"by Nanami Kamimura and Handige Harry, licensed under CC BY-SA 4.0",
       div(style="margin-bottom:20px"),
       
       #Player 1 dice + Points
       fluidRow(column(1, uiOutput("dice11")), column(1, uiOutput("dice12")), column(1, uiOutput("dice13")),
                column(2, actionButton("switch_die1", "6,6 --> 1")), column(1, uiOutput("rolls1")),column(1, uiOutput("pts1"))),
       div(style="margin-bottom:70px"),
       
       #Player 2 dice + Points
       fluidRow(column(1, uiOutput("dice21")), column(1, uiOutput("dice22")), column(1, uiOutput("dice23")),
                column(2, actionButton("switch_die2", "6,6 --> 1")), column(1, uiOutput("rolls2")),column(1, uiOutput("pts2"))),
       div(style="margin-bottom:70px"),
       
       #Player 3 dice + Points
       fluidRow(column(1, uiOutput("dice31")), column(1, uiOutput("dice32")), column(1, uiOutput("dice33")),
                column(2, actionButton("switch_die3", "6,6 --> 1")), column(1, uiOutput("rolls3")),column(1, uiOutput("pts3"))),
       div(style="margin-bottom:70px"),
       
       #Player 4 dice + Points
       fluidRow(column(1, uiOutput("dice41")), column(1, uiOutput("dice42")), column(1, uiOutput("dice43")),
                column(2, actionButton("switch_die4", "6,6 --> 1")), column(1, uiOutput("rolls4")),column(1, uiOutput("pts4"))),
       div(style="margin-bottom:70px"),
       
       #Player 5 dice + Points
       fluidRow(column(1, uiOutput("dice51")), column(1, uiOutput("dice52")), column(1, uiOutput("dice53")),
                column(2, actionButton("switch_die5", "6,6 --> 1")), column(1, uiOutput("rolls5")),column(1, uiOutput("pts5"))),
       div(style="margin-bottom:70px"),
       
       #Player 6 dice + Points
       fluidRow(column(1, uiOutput("dice61")), column(1, uiOutput("dice62")), column(1, uiOutput("dice63")),
                column(2, actionButton("switch_die6", "6,6 --> 1")), column(1, uiOutput("rolls6")),column(1, uiOutput("pts6")))
       

       
       
   )
    )
)

# Define server logic
server <- function(input, output, session) {

    
#### hide button to switch 6,6 to 1 ######
    shinyjs::hide("switch_die1")
    shinyjs::hide("switch_die2")
    shinyjs::hide("switch_die3")
    shinyjs::hide("switch_die4")
    shinyjs::hide("switch_die5")
    shinyjs::hide("switch_die6")
############### Recative functions handling action buttons / inputs ##############
    
    #Player 1
    observeEvent(input$roll1, {roll(1, input)})
    observeEvent(input$unveil1, {unveil(1)})
    observeEvent(input$pts1, {update_P(1, input$pts1)})
    observeEvent(input$switch_die1, {switch_die(1, input, session)})
    
    
    
    #Player 2
    observeEvent(input$roll2, {roll(2, input)})
    observeEvent(input$unveil2, {unveil(2)})
    observeEvent(input$pts2, {update_P(2, input$pts2)})
    observeEvent(input$switch_die2, {switch_die(2, input, session)})
    
    
    #Player 3
    observeEvent(input$roll3, {roll(3, input)})
    observeEvent(input$unveil3, {unveil(3)})
    observeEvent(input$pts3, {update_P(3, input$pts3)})
    observeEvent(input$switch_die3, {switch_die(3, input, session)})
    
    #Player 4
    observeEvent(input$roll4, {roll(4, input)})
    observeEvent(input$unveil4, {unveil(4)})
    observeEvent(input$pts4, {update_P(4, input$pts4)})
    observeEvent(input$switch_die4, {switch_die(4, input, session)})
    
    #Player 5
    observeEvent(input$roll5, {roll(5, input)})
    observeEvent(input$unveil5, {unveil(5)})
    observeEvent(input$pts5, {update_P(5, input$pts5)})
    observeEvent(input$switch_die5, {switch_die(5, input, session)})
    
    #Player 6
    observeEvent(input$roll6, {roll(6, input)})
    observeEvent(input$unveil6, {unveil(6)})
    observeEvent(input$pts6, {update_P(6, input$pts6)})
    observeEvent(input$switch_die6, {switch_die(6, input, session)})
    
    
    
    
    
    
    reactive_B <- reactivePoll(10, session = session,
                                         checkFunc = function() { B },
                                         valueFunc = function() { B }
    )
    
    reactive_P <- reactivePoll(100, session = session,
                               checkFunc = function() { P },
                               valueFunc = function() { P }
    )
    
    reactive_A <- reactivePoll(100, session = session,
                               checkFunc = function() { A },
                               valueFunc = function() { A }
    )
    
    reactive_rolls <- reactivePoll(100, session = session,
                                   checkFunc = function() { rolls },
                                   valueFunc = function() { 3 - rolls }
    )
    
    
###### update outputs according to changes
    
    ## Player 1
    # load images of numbered dice / empty dice
    output$dice11 = renderUI(img(src=ifelse(!reactive_B()[1,1], pics[7], pics[reactive_A()[1,1]]), width = '50px', heigth = '50px'))
    output$dice12 = renderUI(img(src=ifelse(!reactive_B()[1,2], pics[7], pics[reactive_A()[1,2]]), width = '50px', heigth = '50px'))
    output$dice13 = renderUI(img(src=ifelse(!reactive_B()[1,3], pics[7], pics[reactive_A()[1,3]]), width = '50px', heigth = '50px'))
    # update points
    output$pts1 = renderUI(pre(reactive_P()[1]))
    # update rolls
    output$rolls1 = renderUI(pre(reactive_rolls()[1]))
    
    
    # Player 2
    output$dice21 = renderUI(img(src=ifelse(!reactive_B()[2,1], pics[7], pics[reactive_A()[2,1]]), width = '50px', heigth = '50px'))
    output$dice22 = renderUI(img(src=ifelse(!reactive_B()[2,2], pics[7], pics[reactive_A()[2,2]]), width = '50px', heigth = '50px'))
    output$dice23 = renderUI(img(src=ifelse(!reactive_B()[2,3], pics[7], pics[reactive_A()[2,3]]), width = '50px', heigth = '50px'))
    output$pts2 = renderUI(pre(reactive_P()[2]))
    output$rolls2 = renderUI(pre(reactive_rolls()[2]))
    
    
    #Player 3
    output$dice31 = renderUI(img(src=ifelse(!reactive_B()[3,1], pics[7], pics[reactive_A()[3,1]]), width = '50px', heigth = '50px'))
    output$dice32 = renderUI(img(src=ifelse(!reactive_B()[3,2], pics[7], pics[reactive_A()[3,2]]), width = '50px', heigth = '50px'))
    output$dice33 = renderUI(img(src=ifelse(!reactive_B()[3,3], pics[7], pics[reactive_A()[3,3]]), width = '50px', heigth = '50px'))
    output$pts3 = renderUI(pre(reactive_P()[3]))
    output$rolls3 = renderUI(pre(reactive_rolls()[3]))
    
    
    #Player 4
    output$dice41 = renderUI(img(src=ifelse(!reactive_B()[4,1], pics[7], pics[reactive_A()[4,1]]), width = '50px', heigth = '50px'))
    output$dice42 = renderUI(img(src=ifelse(!reactive_B()[4,2], pics[7], pics[reactive_A()[4,2]]), width = '50px', heigth = '50px'))
    output$dice43 = renderUI(img(src=ifelse(!reactive_B()[4,3], pics[7], pics[reactive_A()[4,3]]), width = '50px', heigth = '50px'))
    output$pts4 = renderUI(pre(reactive_P()[4]))
    output$rolls4 = renderUI(pre(reactive_rolls()[4]))
    
    #Player 5
    output$dice51 = renderUI(img(src=ifelse(!reactive_B()[5,1], pics[7], pics[reactive_A()[5,1]]), width = '50px', heigth = '50px'))
    output$dice52 = renderUI(img(src=ifelse(!reactive_B()[5,2], pics[7], pics[reactive_A()[5,2]]), width = '50px', heigth = '50px'))
    output$dice53 = renderUI(img(src=ifelse(!reactive_B()[5,3], pics[7], pics[reactive_A()[5,3]]), width = '50px', heigth = '50px'))
    output$pts5 = renderUI(pre(reactive_P()[5]))
    output$rolls5 = renderUI(pre(reactive_rolls()[5]))
    
    #Player 6
    output$dice61 = renderUI(img(src=ifelse(!reactive_B()[6,1], pics[7], pics[reactive_A()[6,1]]), width = '50px', heigth = '50px'))
    output$dice62 = renderUI(img(src=ifelse(!reactive_B()[6,2], pics[7], pics[reactive_A()[6,2]]), width = '50px', heigth = '50px'))
    output$dice63 = renderUI(img(src=ifelse(!reactive_B()[6,3], pics[7], pics[reactive_A()[6,3]]), width = '50px', heigth = '50px'))
    output$pts6 = renderUI(pre(reactive_P()[6]))
    output$rolls6 = renderUI(pre(reactive_rolls()[6]))

    

    output$frame <- renderUI({
        HTML(paste(
            p(HTML('&emsp;'),HTML('&emsp;'),HTML('&nbsp;'),HTML('&nbsp;'),"Würfe")
        )
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
