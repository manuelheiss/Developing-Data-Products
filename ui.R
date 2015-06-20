library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel("String similarity measures"),
    sidebarPanel(
      h5("This app calculates the similarity for two entered strings. The similarity is a value between 0 and 1, a higher value indicates a higher degree of similarity. If similarity is 0 the two strings are not similar at all, if it is 1 they are identical. The similarity is calculated according to all selected similarity measures. Please note that for N-Gram, Jaccard and Cosine the size of the grams which those algorithms are working on must be set with a slider."),
      h4("Choose similarity measures to be applied:"),
      checkboxInput("Exact", "Exact"),
      checkboxInput("Jaro", "Jaro"),
      checkboxInput("JaroWinkler", "Jaro-Winkler"),
      checkboxInput("Levenshtein", "Levenshtein"),
      checkboxInput("DamerauLevenshtein", "Damerau-Levenshtein"),
      checkboxInput("NGram", "N-Gram"),
      conditionalPanel(condition = "input.NGram == true", sliderInput("nNGram", "Choose gram size:", min = 2, max = 5, value = 3)),
      checkboxInput("Cosine", "Cosine"),
      conditionalPanel(condition = "input.Cosine == true", sliderInput("nCosine", "Choose gram size:", min = 1, max = 5, value = 2)),
      checkboxInput("Jaccard", "Jaccard"),
      conditionalPanel(condition = "input.Jaccard == true", sliderInput("nJaccard", "Choose gram size:", min = 1, max = 5, value = 2)),
      checkboxInput("SoundEx", "SoundEx"),
      h4("Choose strings to be compared:"),
      textInput("string1", "String1:"),
      textInput("string2", "String2:"),
      actionButton("Calculate", "Calculate similarities")
    ),
    mainPanel(
      conditionalPanel(condition = "input.Exact == true", verbatimTextOutput("sim1")),
      conditionalPanel(condition = "input.Jaro == true", verbatimTextOutput("sim2")),
      conditionalPanel(condition = "input.JaroWinkler == true", verbatimTextOutput("sim3")),
      conditionalPanel(condition = "input.Levenshtein == true", verbatimTextOutput("sim4")),
      conditionalPanel(condition = "input.DamerauLevenshtein == true", verbatimTextOutput("sim5")),
      conditionalPanel(condition = "input.NGram == true", verbatimTextOutput("sim6")),
      conditionalPanel(condition = "input.Cosine == true",  verbatimTextOutput("sim7")),
      conditionalPanel(condition = "input.Jaccard == true", verbatimTextOutput("sim8")),
      conditionalPanel(condition = "input.SoundEx == true", verbatimTextOutput("sim9"))
    )
  )
)