library(plotly)
library(data.table)

generatePolar <- function (data, ...) {

#convert the data to suitable input for generatePolar.R (adjacency matrix with unfiltered data)
#unfiltered data = Edges[question]Numeric[,c(source, target, numericScoreList)]
  
inputData <- data[,c("source", "target", "numericScoreList")]
vars <- sort(unique(unlist(inputData[c("source","target")])))
polarData <- matrix(NA, nr=length(vars), nc=length(vars), dimnames=list(vars,vars))
for (i in 1:nrow(inputData)) {polarData[inputData[i,1],inputData[i,2]] <- inputData[i,3]}

descriptiveStats <- polarData

meanLine <- mean(colMeans(descriptiveStats, na.rm = TRUE))

polarData[is.na(polarData)] <- 0

lengthR <- nrow(polarData)
degree <- 360/lengthR
names <- rownames(polarData)

ids <- c(names, "")

fig <- plot_ly(
  type = 'scatterpolar',
  thetaunit = "degrees",
  fill = 'toself'
)
for (k in 1:nrow(polarData)) {
  fig <- fig %>% add_trace(
    r = polarData[k,], #replace all NA values with zero?
    theta0 = 0,
    dtheta = degree,
    text = names,
    showlegend = TRUE,
    ids <- ids,
    mode = "markers",
    opacity = 0.5,
    name = names[[k]],
    visible="legendonly",
    hoverinfo="r+text"
  ) 
  
}

for (i in (seq(0, 360, degree))) {
  fig <- fig %>% 
    # straight line from origin to edge
    add_trace(type = 'scatterpolar',
              r = c(-0.1,1,1),
              theta = c(0, i, i),
              mode = 'lines',
              showlegend=FALSE,
              text = "",
              line = list(color = 'gainsboro', width = 1))
}
#making the outline of the graph
fig <- fig %>%  
  # fill circle of radius <0
  add_trace(type = 'scatterpolar', 
            mode = 'lines', 
            r = 0, 
            theta =seq(0, 360, 0.1),
            line = list(color = 'grey'), 
            fill = 'toself',
            showlegend = F,
            fillcolor = 'white', 
            hovertext = "",
            opacity = 1,
            text = "")
fig <- fig %>%  
  # fill circle of radius 1
  add_trace(type = 'scatterpolar', 
            mode = 'lines', 
            r = 0.995, 
            theta =seq(0, 360, 1),
            line = list(color = 'grey'), 
            fill = 'none',
            showlegend = F,
            fillcolor = 'transparent', 
            hoverinfo = "none",
            opacity = 1,
            text = "")
fig <- fig %>% 
  # add mean circle
  add_trace(type = 'scatterpolar', 
            mode = 'lines', 
            r = meanLine, 
            theta =seq(0, 360, 1),
            line = list(color = 'green', width = 1, dash = 'dot'), 
            fill = 'none',
            showlegend = F,
            fillcolor = 'transparent',
            hoverinfo = "r",
            opacity = 1,
            text = ""
            )
fig <- fig %>%
  layout(
    autosize=T,
    polar = list(
      radialaxis = list(
        visible = F,
        range = c(-0.1,1),
        text = ""
      ),
      angularaxis = list(
        showticklabels = TRUE,
        # remove grid lines and ticks
        showgrid = FALSE,
        ticks = '',
        # if you want the axis to go the other way around
        # direction = 'clockwise',
        tickmode="array",
        tickvals = seq(0, (360-degree), degree),
        ticktext = names,
        hoverinfo = "none"
      )
    ),
     legend = list(orientation = "h",   # show entries horizontally
                   xanchor = "center",  # use center of legend as anchor
                   x = 0.5),
    showlegend = T
  )
  config(fig, fillFrame = TRUE)
  fig
}

#output data in the loop to see what is created

