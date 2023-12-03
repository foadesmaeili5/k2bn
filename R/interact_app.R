#' Shiny App for Interactive Visualization of Bayesian Network
#'
#' This function creates a Shiny app for interactively exploring a Bayesian network.
#'
#' @param dag The Bayesian network represented as a directed acyclic graph (DAG).
#'
#' @return A Shiny app for the interactive exploration of the Bayesian network.
#' @export
#'
#' @examples
#' require(bnlearn)
#' require(ggplot2)
#' require(shiny)
#' require(visNetwork)
#' 
#' modelstring <-  
#'  paste0("[Age][Mileage][SocioEcon|Age]",
#'    "[GoodStudent|Age:SocioEcon]",
#'    "[RiskAversion|Age:SocioEcon][OtherCar|SocioEcon]",
#'    "[VehicleYear|SocioEcon:RiskAversion]",
#'    "[MakeModel|SocioEcon:RiskAversion]",
#'    "[SeniorTrain|Age:RiskAversion]",
#'    "[HomeBase|SocioEcon:RiskAversion]",
#'    "[AntiTheft|SocioEcon:RiskAversion]",
#'    "[RuggedAuto|VehicleYear:MakeModel]",
#'    "[Antilock|VehicleYear:MakeModel]",
#'    "[DrivingSkill|Age:SeniorTrain]",
#'    "[CarValue|VehicleYear:MakeModel:Mileage]",
#'    "[Airbag|VehicleYear:MakeModel]",
#'    "[DrivQuality|RiskAversion:DrivingSkill]",
#'    "[Theft|CarValue:HomeBase:AntiTheft]",
#'    "[Cushioning|RuggedAuto:Airbag]",
#'    "[DrivHist|RiskAversion:DrivingSkill]",
#'    "[Accident|DrivQuality:Mileage:Antilock]",
#'    "[ThisCarDam|RuggedAuto:Accident]",
#'    "[OtherCarCost|RuggedAuto:Accident]",
#'    "[MedCost|Age:Accident:Cushioning]",
#'    "[ILiCost|Accident]",
#'    "[ThisCarCost|ThisCarDam:Theft:CarValue]",
#'    "[PropCost|ThisCarCost:OtherCarCost]")
#' dag <- model2network(modelstring)
#' dag <- bn.fit(dag, insurance)
#' if (interactive()) {
#'   interact_app(dag)
#' }
#'
#' @importFrom shiny fluidPage column visNetworkOutput actionButton plotOutput
#' @importFrom shiny verbatimTextOutput renderPlot observe eventReactive
#' @importFrom Rgraphviz nodes arcs
#' @importFrom bnlearn bn.fit model2network
#' @importFrom visNetwork visNetworkProxy visGetSelectedNodes visOptions visEdges visEvents
#'
#' @export
interact_app <- function(dag) {
  ui <- fluidPage(
    column(
      width = 12,
      visNetworkOutput("plt", width = "1920px", height = "700px"),
      actionButton("btn", label = "show probabilty Plots")
    ),
    column(width = 12,
           plotOutput("myplt"),),
    verbatimTextOutput("txt"),
    lang = "fa",
    padding = 20
  )
  server <- function(input, output, session) {
    nodes <- data.frame(id = nodes(dag), label = nodes(dag))
    edges <- as.data.frame(arcs(dag))
    # hcModel <- hc(alarm)
    # hcFit <- bn.fit(hcModel,alarm)
    output$plt <- renderVisNetwork({
      visNetwork(nodes = nodes, edges = edges) %>%
        visOptions(highlightNearest = TRUE) %>%
        visNetwork::visEdges(arrows = "to") %>%
        visEvents(click = "function(nodes){
                  Shiny.onInputChange('click', nodes.nodes[0]);
                  ;}")
    })
    observe({
      input$btn
      visNetworkProxy("plt") %>% visGetSelectedNodes()
    })
    df <-
      eventReactive(input$btn, dag[[input$plt_selectedNodes]]$prob)
    observeEvent(input$btn,
                 output$myplt <- renderPlot({
                   pie_fitted_plot(dag, input$plt_selectedNodes)
                 }))
    # observeEvent(input$btn,
    #              output$return <- renderUI(
    #                lapply(1:length(xPlots), function(i) {
    #                  verbatimTextOutput(paste0("id",i))
    #                })
    #              ))
    output$txt <- renderPrint({
      df()
    })
    
  }
  shinyApp(ui, server)
}

