## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(shiny)
# library(ggseg3d)

## -----------------------------------------------------------------------------
# ui <- fluidPage(
#   titlePanel("Brain Atlas Viewer"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput(
#         "hemi",
#         "Hemisphere",
#         choices = c("left", "right", "both"),
#         selected = "left"
#       )
#     ),
#     mainPanel(
#       ggseg3dOutput("brain", height = "500px")
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   output$brain <- renderGgseg3d({
#     hemi <- if (input$hemi == "both") NULL else input$hemi
# 
#     ggseg3d(hemisphere = hemi) |>
#       pan_camera("left lateral")
#   })
# }
# 
# shinyApp(ui, server)

## -----------------------------------------------------------------------------
# ggseg3dOutput("brain", width = "100%", height = "600px")

## -----------------------------------------------------------------------------
# renderGgseg3d({
#   ggseg3d() |>
#     set_dimensions(width = 800, height = 600)
# })

## -----------------------------------------------------------------------------
# server <- function(input, output, session) {
#   brain_data <- reactive({
#     tibble(
#       region = c("precentral", "postcentral", "insula"),
#       value = runif(3)
#     )
#   })
# 
#   output$brain <- renderGgseg3d({
#     ggseg3d(
#       .data = brain_data(),
#       atlas = dk(), # nolint [object_usage_linter]
#       colour_by = "value"
#     ) |>
#       pan_camera("left lateral")
#   })
# }

## -----------------------------------------------------------------------------
# ui <- fluidPage(
#   sidebarLayout(
#     sidebarPanel(
#       selectInput(
#         "view",
#         "View",
#         choices = c(
#           "left lateral",
#           "left medial",
#           "right lateral",
#           "right medial"
#         )
#       ),
#       selectInput("bg", "Background", choices = c("white", "black", "grey"))
#     ),
#     mainPanel(
#       ggseg3dOutput("brain")
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   output$brain <- renderGgseg3d({
#     ggseg3d() |>
#       pan_camera("left lateral")
#   })
# 
#   observeEvent(input$view, {
#     updateGgseg3dCamera("brain", input$view)
#   })
# 
#   observeEvent(input$bg, {
#     updateGgseg3dBackground("brain", input$bg)
#   })
# }

## -----------------------------------------------------------------------------
# library(shiny)
# library(ggseg3d)
# library(dplyr)
# 
# example_data <- tibble(
#   region = c(
#     "precentral",
#     "postcentral",
#     "insula",
#     "superior parietal",
#     "inferior parietal",
#     "supramarginal",
#     "cuneus",
#     "pericalcarine"
#   ),
#   thickness = c(2.5, 2.3, 3.1, 2.2, 2.4, 2.6, 1.8, 1.9),
#   volume = c(8500, 7200, 6800, 9100, 8800, 7500, 4200, 3800)
# )
# 
# ui <- fluidPage(
#   titlePanel("Brain Metrics Explorer"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("metric", "Metric", choices = c("thickness", "volume")),
#       selectInput(
#         "view",
#         "Camera View",
#         choices = c(
#           "left lateral",
#           "left medial",
#           "right lateral",
#           "right medial",
#           "left superior",
#           "left inferior"
#         )
#       ),
#       checkboxInput("edges", "Show edges", value = FALSE),
#       selectInput("bg", "Background", choices = c("white", "black", "grey90"))
#     ),
#     mainPanel(
#       ggseg3dOutput("brain", height = "600px")
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   output$brain <- renderGgseg3d({
#     p <- ggseg3d(
#       .data = example_data,
#       atlas = dk(), # nolint [object_usage_linter]
#       colour_by = input$metric,
#       text_by = input$metric
#     ) |>
#       pan_camera(input$view) |>
#       set_background(input$bg)
# 
#     if (input$edges) {
#       p <- p |> set_edges("black")
#     }
# 
#     p
#   })
# }
# 
# shinyApp(ui, server)

