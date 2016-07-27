#----------------------#
#      Shiny App       #
#----------------------#

# set the unique working directory for the app


# add all the libraries
library(shiny)
library(leaflet)
library(RColorBrewer)


# funcion to center align objects in the absolute panel
alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
    style="margin-left:auto;margin-right:auto;"
  )
}

shinyApp(

ui = bootstrapPage(
tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
leafletOutput('myMap', width = "100%", height = "100%"),
absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE, draggable = TRUE, style = "opacity: 0.85", top = "0", left = "auto", right = 20, bottom = "auto", width = 330, height = "auto",
h3("Durham County Vaccination Clinic Location Optimization", align = "center"),
h6("This graph displays a modelling approach to identify the most effective locations for vaccination clinic placement during an outbreak of the Zika virus. Small circles present infected and vaccinated residents of Durham County. Blue markers indicate the schools chosen to be emergency response vaccination clinic sites.", align = "center"),
alignCenter(sliderInput("num", label = h5("Number of Days of Disease Spread", align = "center"), min = 0, max = 30, value = 5)),
alignCenter(selectInput("select", label = h5("Select Demographic Trait", align = "center"), choices = list("Race" = as.numeric(1), "Gender" = as.numeric(2), "Age" = as.numeric(3), "Income" = as.numeric(4)))),
plotOutput("barGraphI", height = 190),
plotOutput("barGraphV", height = 190)),
tags$div(id="cite", "Data compiled by RTI-International: U.S. Synthetic Population 2010 V.1.0. Simulation and modelling created by Lindsay Hirschhorn & Kelsey Sumner.") 
),





server = function(input, output) {
	
   
# read in people data and make matrix of their household latitudes and longitude
data = read.csv("Shiny Test Data")
newdata = data[sample(nrow(data),.25*(nrow(data))),]
latlongdf = as.data.frame(newdata[,13:14])

# Durham County Coordinates
durham_latitude = 36.0222
durham_longitude = -78.9031

# Clinic Placement Coordinates
clinic_latitude = c(35.90549, 35.97390, 36.05285, 36.17018)
clinic_longitude = c(-78.88895, -78.93500, -78.93497, -78.87801)
clinic_names = c("Lowes Grove Middle School", "Rogers-Herr Middle School", "Hillandale Elementary School", "Mangum Elementary School")

# plot a base map using leaflet and set the view to Durham County
# add circles with yellow = vaccinated & red = infected
 map <- function(vaccinated,infected){
 leaflet() %>% addTiles() %>% setView(lng = durham_longitude, lat = durham_latitude, zoom = 12) %>% addCircleMarkers(latlongdf[vaccinated,2],latlongdf[vaccinated,1], color = "yellow", radius = 1) %>% addCircleMarkers(latlongdf[infected,2], latlongdf[infected,1], color = "red", radius = 1) %>% addMarkers(lng = clinic_longitude, lat = clinic_latitude, popup = clinic_names) %>% addLegend("bottomleft", title = "Disease Spread Legend", colors = c("red","yellow"), values = c(which(input$num >= newdata[, 25] & newdata[, 25] > 0), which(input$num >= newdata[, 26] & newdata[, 26] > 0)), labels = c("Infected Individuals", "Vaccinated Individuals"))
 }


# output the map you created (25% sampled from whole data set)
output$myMap = renderLeaflet(map(which(input$num >= newdata[, 25] & newdata[, 25] > 0), which(input$num >= newdata[, 26] & newdata[, 26] > 0)))


### infected bar graphs
# function to choose which graph
chooseGraphInfection = function(demo) {
	
if (demo == 1) {
# output the vaccinated bar graph for race
	filler = newdata[which(input$num >= newdata[, 26] & newdata[, 26] > 0),8]
    whitehispanic = filler[which(filler == 1)]
    black = filler[which(filler == 2)]
    asian = filler[which(filler == 6)]
    aian = filler[which(filler == 3 | filler == 4 | filler == 5)]
    other = filler[which(filler == 0 | filler == 7 | filler == 8 | filler == 9)]
    final_vector = c(length(whitehispanic), length(black), length(asian), length(aian), length(other))
    barplot(height = final_vector, main = "Infection by Race", col = c("yellow", "red", "grey", "lightpink", "orange"), horiz = TRUE, angle = 45, las = 2, names.arg = c("White", "Black", "Asian", "AI/AN", "Other"))
}

if (demo == 2) {
# output the vaccinated bar graph created for gender
    filler = newdata[which(input$num >= newdata[, 26] & newdata[, 26] > 0),7]
    male = filler[which(filler == 1)]
    female = filler[which(filler == 2)]
    final_vector = c(length(male), length(female))
    barplot(height = final_vector, main = "Infection by Gender", col = c("lightblue", "pink"), names.arg = c("Male", "Female"))
 }

if (demo == 3){
# output the vaccinated bar graph created for Age
    filler = newdata[which(input$num >= newdata[, 26] & newdata[, 26] > 0),6]
    upto10 = filler[which(filler <= 10)]
    upto20 = filler[which(filler >= 11 & filler <= 20)]
    upto30 = filler[which(filler >= 21 & filler <= 30)]
    upto40 = filler[which(filler >= 31 & filler <= 40)]
    upto50 = filler[which(filler >= 41 & filler <= 50)]
    upto60 = filler[which(filler >= 51 & filler <= 60)]
    over60 = filler[which(filler > 60)]
    final_vector = c(length(upto10), length(upto20), length(upto30), length(upto40), length(upto50), length(upto60), length(over60))
    barplot(height = final_vector, main = "Infection by Age Group", col = c("yellow", "grey", "pink", "lightblue", "purple", "green", "orange"), angle = 45, las=2, names.arg = c("0 to 10", "11 to 20", "21 to 30", "31 to 40", "41 to 50", "51 to 60", "Over 60"))
    
}

if (demo == 4){
# output the vaccinated bar graph created for Income
    filler = newdata[which(input$num >= newdata[, 26] & newdata[, 26] > 0),21]
    belowpoverty = filler[which(filler == 1)]
    abovepoverty = filler[which(filler == 2)]
    final_vector = c(length(belowpoverty), length(abovepoverty))
    barplot(height = final_vector, main = "Infection by Poverty Level", col = c("yellow", "grey"), names.arg = c("Below Poverty Level", "Above Poverty Level"))
}

# end the choose graph function
}


### vaccinated bar graphs
# function to choose which graph
chooseGraphVaccination = function(chosen) {
if (chosen == 1){
# output the vaccinated bar graph for race
	filler = newdata[which(input$num >= newdata[, 25] & newdata[, 25] > 0),8]
    whitehispanic = filler[which(filler == 1)]
    black = filler[which(filler == 2)]
    asian = filler[which(filler == 6)]
    aian = filler[which(filler == 3 | filler == 4 | filler == 5)]
    other = filler[which(filler == 0 | filler == 7 | filler == 8 | filler == 9)]
    final_vector = c(length(whitehispanic), length(black), length(asian), length(aian), length(other))
    barplot(height = final_vector, main = "Vaccination by Race", col = c("yellow", "red", "grey", "lightpink", "orange"), horiz = TRUE, angle = 45, las = 2, names.arg = c("White", "Black", "Asian", "AI/AN", "Other"))
}

if (chosen == 2){
# output the vaccinated bar graph created for gender
    filler = newdata[which(input$num >= newdata[, 25] & newdata[, 25] > 0),7]
    male = filler[which(filler == 1)]
    female = filler[which(filler == 2)]
    final_vector = c(length(male), length(female))
    barplot(height = final_vector, main = "Vaccination by Gender", col = c("lightblue", "pink"), names.arg = c("Male", "Female"))
  }

if (chosen == 3){
# output the vaccinated bar graph created for Age
    filler = newdata[which(input$num >= newdata[, 25] & newdata[, 25] > 0),6]
    upto10 = filler[which(filler <= 10)]
    upto20 = filler[which(filler >= 11 & filler <= 20)]
    upto30 = filler[which(filler >= 21 & filler <= 30)]
    upto40 = filler[which(filler >= 31 & filler <= 40)]
    upto50 = filler[which(filler >= 41 & filler <= 50)]
    upto60 = filler[which(filler >= 51 & filler <= 60)]
    over60 = filler[which(filler > 60)]
    final_vector = c(length(upto10), length(upto20), length(upto30), length(upto40), length(upto50), length(upto60), length(over60))
    barplot(height = final_vector, main = "Vaccination by Age Group", col = c("yellow", "grey", "pink", "lightblue", "purple", "green", "orange"), angle = 45, las=2, names.arg = c("0 to 10", "11 to 20", "21 to 30", "31 to 40", "41 to 50", "51 to 60", "Over 60"))
}

if (chosen == 4){
# output the vaccinated bar graph created for Income
    filler = newdata[which(input$num >= newdata[, 25] & newdata[, 25] > 0),21]
    belowpoverty = filler[which(filler == 1)]
    abovepoverty = filler[which(filler == 2)]
    final_vector = c(length(belowpoverty), length(abovepoverty))
    barplot(height = final_vector, main = "Vaccination by Poverty Level", col = c("yellow", "grey"), names.arg = c("Below Poverty Level", "Above Poverty Level"))

}
# end the choose graph function
}

# output the two bar graphs
output$barGraphI = renderPlot({
(chooseGraphInfection(input$select))
})

output$barGraphV = renderPlot({
(chooseGraphVaccination(input$select))	
})



}

)
