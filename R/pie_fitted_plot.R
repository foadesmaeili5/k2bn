# -------------------------------------------------------------------------
#' Pie Chart with Fitted Curve for Multiple Parents
#' 
#' @details
#' The `fitted_dag` argument should be a bn.fit object obtained by estimating
#' parameters using methods such as MLE or Bayesian methods. The `node` argument
#' specifies the name of the node for which the pie chart and fitted curve will 
#' be plotted. The function is particularly useful when there are multiple 
#' parents in the k2 algorithm.
#' 
#'
#' @param fitted_dag `bn.fit` object
#' @param node name of the node to plot the probabilities
#' @return `ggplot2` plot
#' @export
#'
#' @examples
#' require(bnlearn)
#' require(ggplot2)
#' data(insurance)
#  
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
#' dag = model2network(modelstring)
#' ins_fit <- bn.fit(dag,insurance)
#' graphviz.plot(dag)
#'
#'
#' pie_fitted_plot(ins_fit,"GoodStudent")

pie_fitted_plot <- function(fitted_dag, node) {
  x <- as.data.frame(fitted_dag[[node]]$prob)
  Fcet <- names(x)
  Fcet <- Fcet[Fcet != "Freq"]
  Fcet <- Fcet[-1]
  n <- names(x)
  n <- n[n != "Freq"]
  if (length(Fcet) > 3) {
    warning("number of parent nodes are larger than 3")
  }
  if (length(Fcet) > 2) {
    ggplot(x, aes(
      x = get(Fcet[1]),
      y = get("Freq"),
      fill = get(n[1])
    )) +
      xlab(Fcet[1]) +
      geom_bar(stat = "identity") +
      geom_text(hjust = 1,
                size = 3,
                aes(
                  x = get(Fcet[1]),
                  y = 0,
                  label = get(Fcet[1])
                )) +
      ylim(c(0, 1.35)) +
      labs(fill = n[1]) +
      ylab("") +
      ggtitle(paste(Fcet[2], "~", Fcet[3])) +
      facet_grid(get(Fcet[2]) ~ get(Fcet[3])) +
      coord_polar(theta = "y") +
      theme(
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
      )
  } else if (length(Fcet) > 1) {
    ggplot(x, aes(
      x = get(Fcet[1]),
      y = get("Freq"),
      fill = get(n[1])
    )) +
      geom_bar(stat = "identity") +
      geom_text(hjust = 1,
                size = 3,
                aes(
                  x = get(Fcet[1]),
                  y = 0,
                  label = get(Fcet[1])
                )) +
      ylim(c(0, 1.35)) +
      labs(fill = n[1]) +
      xlab(Fcet[1]) +
      ylab("") +
      ggtitle(Fcet[2]) +
      facet_grid( ~ get(Fcet[2])) +
      coord_polar(theta = "y") +
      theme(
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
      )
    
  } else if (length(Fcet) > 0) {
    ggplot(x, aes(
      x = get(Fcet[1]),
      y = get("Freq"),
      fill = get(n[1])
    )) +
      geom_bar(stat = "identity") +
      geom_text(hjust = 1,
                size = 3,
                aes(
                  x = get(Fcet[1]),
                  y = 0,
                  label = get(Fcet[1])
                )) +
      ylim(c(0, 1.35)) +
      labs(fill = n[1]) +
      xlab(Fcet[1]) +
      ylab("") +
      coord_polar(theta = "y") +
      theme(
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
      )
  } else {
    ggplot(x, aes(
      x = 1,
      fill = get(n[1]),
      y = get("Freq")
    )) +
      geom_bar(stat = "identity") +
      coord_polar(theta = "y") +
      labs(fill = n[1]) +
      ylab("") +
      coord_polar(theta = "y") +
      theme(
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
      )
  }
}

# -------------------------------------------------------------------------


