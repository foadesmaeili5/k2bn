% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interact_app.R
\name{interact_app}
\alias{interact_app}
\title{Shiny App for Interactive Visualization of Bayesian Network}
\usage{
interact_app(dag)
}
\arguments{
\item{dag}{The Bayesian network represented as a directed acyclic graph (DAG).}
}
\value{
A Shiny app for the interactive exploration of the Bayesian network.
}
\description{
This function creates a Shiny app for interactively exploring a Bayesian network.
}
\examples{
require(bnlearn)
require(ggplot2)
require(shiny)
require(visNetwork)

modelstring <-  
 paste0("[Age][Mileage][SocioEcon|Age]",
   "[GoodStudent|Age:SocioEcon]",
   "[RiskAversion|Age:SocioEcon][OtherCar|SocioEcon]",
   "[VehicleYear|SocioEcon:RiskAversion]",
   "[MakeModel|SocioEcon:RiskAversion]",
   "[SeniorTrain|Age:RiskAversion]",
   "[HomeBase|SocioEcon:RiskAversion]",
   "[AntiTheft|SocioEcon:RiskAversion]",
   "[RuggedAuto|VehicleYear:MakeModel]",
   "[Antilock|VehicleYear:MakeModel]",
   "[DrivingSkill|Age:SeniorTrain]",
   "[CarValue|VehicleYear:MakeModel:Mileage]",
   "[Airbag|VehicleYear:MakeModel]",
   "[DrivQuality|RiskAversion:DrivingSkill]",
   "[Theft|CarValue:HomeBase:AntiTheft]",
   "[Cushioning|RuggedAuto:Airbag]",
   "[DrivHist|RiskAversion:DrivingSkill]",
   "[Accident|DrivQuality:Mileage:Antilock]",
   "[ThisCarDam|RuggedAuto:Accident]",
   "[OtherCarCost|RuggedAuto:Accident]",
   "[MedCost|Age:Accident:Cushioning]",
   "[ILiCost|Accident]",
   "[ThisCarCost|ThisCarDam:Theft:CarValue]",
   "[PropCost|ThisCarCost:OtherCarCost]")
dag <- model2network(modelstring)
dag <- bn.fit(dag, insurance)
if (interactive()) {
  interact_app(dag)
}

}
