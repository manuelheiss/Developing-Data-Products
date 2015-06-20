library(shiny)
library(stringdist)

shinyServer(
  function(input, output) {
    ###############################################################################
    # Exact
    ###############################################################################
    # Calculate similarity.
    simExact <- eventReactive(input$Calculate, {
      # Upper case both input strings.
      s1 <- toupper(input$string1)
      s2 <- toupper(input$string2)
      # Do similarity calculation. Similarity is just 1 if strings are equal and 0 else.
      paste("Exact similarity:\n", toString(ifelse(s1 == s2, 1, 0)))
    })
    # Only output this similarity if according checkbox is activated.
    output$sim1 <- renderText({
      if(input$Exact) {
        simExact()
      }
    })
    ###############################################################################
    # Jaro
    ###############################################################################
    # Calculate similarity.
    simJaro <- eventReactive(input$Calculate, {
      # Upper case both input strings.
      s1 <- toupper(input$string1)
      s2 <- toupper(input$string2)
      # Do similarity calculation using "stringdist". Parameter p = 0 means no Winkler bonus has to be applied.
      # Round calculated similarity to two digits and make sure it is between 0 and 1 with min and max operations,
      # this is neccessary since stringdist implementation is sloppy.
      paste("Jaro similarity:\n", toString(min(1, max(round(1 - stringdist(s1, s2, method = "jw", p = 0), 2), 0))))
    })
    # Only output this similarity if according checkbox is activated.
    output$sim2 <- renderText({
      if(input$Jaro) {
        simJaro()
      }
    })
    ###############################################################################
    # Jaro-Winkler
    ###############################################################################
    # Calculate similarity.
    simJaroWinkler <- eventReactive(input$Calculate, {
      # Upper case both input strings.
      s1 <- toupper(input$string1)
      s2 <- toupper(input$string2)
      # Do similarity calculation using "stringdist". Parameter p = 0.1 means Winkler bonus is applied.
      # Round calculated similarity to two digits and make sure it is between 0 and 1 with min and max operations,
      # this is neccessary since stringdist implementation is sloppy.
      paste("Jaro-Winkler similarity:\n", toString(min(1, max(round(1 - stringdist(s1, s2, method = "jw", p = 0.1), 2), 0))))
    })
    # Only output this similarity if according checkbox is activated.
    output$sim3 <- renderText({
      if(input$JaroWinkler) {
        simJaroWinkler()
      }
    })
    ###############################################################################
    # Levenshtein
    ###############################################################################
    # Calculate similarity.
    simLevenshtein <- eventReactive(input$Calculate, {
      # Upper case both input strings.
      s1 <- toupper(input$string1)
      s2 <- toupper(input$string2)
      # Determine length of shorter string for normalisation of Levenshtein score. Make sure it is > 1.
      minl <- max(min(nchar(input$string1), nchar(input$string2)), 1)
      # Do similarity calculation using "stringdist". Since stringdist only computes the Levenshtein score, we have
      # to normalise it by dividing the score by the length of the shorter string.
      # Round calculated similarity to two digits and make sure it is between 0 and 1 with min and max operations,
      # this is neccessary since stringdist implementation is sloppy.
      paste("Levenshtein similarity:\n", toString(min(1, max(round(1 - stringdist(s1, s2, method = "lv") / minl, 2), 0))))
    })
    # Only output this similarity if according checkbox is activated.
    output$sim4 <- renderText({
      if(input$Levenshtein) {
        simLevenshtein()
      }
    })
    ###############################################################################
    # Damerau-Levenshtein
    ###############################################################################
    # Calculate similarity.
    simDamerauLevenshtein <- eventReactive(input$Calculate, {
      # Upper case both input strings.
      s1 <- toupper(input$string1)
      s2 <- toupper(input$string2)
      # Determine length of shorter string for normalisation of Damerau-Levenshtein score. Make sure it is > 1.
      minl <- max(min(nchar(input$string1), nchar(input$string2)), 1)
      # Do similarity calculation using "stringdist". Since stringdist only computes the Damerau-Levenshtein score, we have
      # to normalise it by dividing the score by the length of the shorter string.
      # Round calculated similarity to two digits and make sure it is between 0 and 1 with min and max operations,
      # this is neccessary since stringdist implementation is sloppy.
      paste("Damerau-Levenshtein similarity:\n", toString(min(1, max(round(1 - stringdist(s1, s2, method = "dl") / minl, 2), 0))))
    })
    # Only output this similarity if according checkbox is activated.
    output$sim5 <- renderText({
      if(input$DamerauLevenshtein) {
        simDamerauLevenshtein()
      }
    })
    ###############################################################################
    # N-Gram
    ###############################################################################
    # Calculate similarity.
    simNGram <- eventReactive(input$Calculate, {
      # Upper case both input strings.
      s1 <- toupper(input$string1)
      s2 <- toupper(input$string2)
      # Calculate biggest applicable n for n-Gram similarity.
      n <- max(1, min(input$nNGram, min(nchar(s1), nchar(s2))))
      # Determine length of longer string for normalisation of N-Gram score. Make sure it is > 1.
      maxl <- max(max(nchar(input$string1), nchar(input$string2)), 1)
      # Do similarity calculation using "stringdist". Since stringdist only computes the N-Gram score, we have
      # to normalise it by dividing the score by the length of the longer string.
      # Round calculated similarity to two digits and make sure it is between 0 and 1 with min and max operations,
      # this is neccessary since stringdist implementation is sloppy.
      paste("N-Gram similarity:\n", toString(ifelse(nchar(input$string1) + nchar(input$string2) == 0, 1, min(1, max(round(1 - stringdist(s1, s2, method = "qgram", q = n) / maxl, 2), 0)))))
    })
    # Only output this similarity if according checkbox is activated.
    output$sim6 <- renderText({
      if(input$NGram) {
        simNGram()
      }
    })
    ###############################################################################
    # Cosine
    ###############################################################################
    # Calculate similarity.
    simCosine <- eventReactive(input$Calculate, {
      # Upper case both input strings.
      s1 <- toupper(input$string1)
      s2 <- toupper(input$string2)
      s2 <- toupper(input$string2)
      # Calculate biggest applicable n for Cosine similarity.
      n <- max(1, min(input$nCosine, min(nchar(s1), nchar(s2))))
      # Do similarity calculation using "stringdist".
      # Round calculated similarity to two digits and make sure it is between 0 and 1 with min and max operations,
      # this is neccessary since stringdist implementation is sloppy.
      paste("Cosine similarity:\n", toString(ifelse(nchar(input$string1) + nchar(input$string2) == 0, 1, min(1, max(round(1 - stringdist(s1, s2, method = "cosine", q = n), 2), 0)))))
    })
    # Only output this similarity if according checkbox is activated.
    output$sim7 <- renderText({
      if(input$Cosine) {
        simCosine()
      }
    })
    ###############################################################################
    # Jaccard
    ###############################################################################
    # Calculate similarity.
    simJaccard <- eventReactive(input$Calculate, {
      # Upper case both input strings.
      s1 <- toupper(input$string1)
      s2 <- toupper(input$string2)
      s2 <- toupper(input$string2)
      # Calculate biggest applicable n for Jaccard similarity.
      n <- max(1, min(input$nJaccard, min(nchar(s1), nchar(s2))))
      # Do similarity calculation using "stringdist".
      # Round calculated similarity to two digits and make sure it is between 0 and 1 with min and max operations,
      # this is neccessary since stringdist implementation is sloppy.
      paste("Jaccard similarity:\n", toString(ifelse(nchar(input$string1) + nchar(input$string2) == 0, 1, min(1, max(round(1 - stringdist(s1, s2, method = "jaccard", q = n), 2), 0)))))
    })
    # Only output this similarity if according checkbox is activated.
    output$sim8 <- renderText({
      if(input$Jaccard) {
        simJaccard()
      }
    })
    ###############################################################################
    # SoundEx
    ###############################################################################
    # Calculate similarity.
    simSoundEx <- eventReactive(input$Calculate, {
      # Upper case both input strings.
      s1 <- toupper(input$string1)
      s2 <- toupper(input$string2)
      # Do similarity calculation using "stringdist".
      # Round calculated similarity to two digits and make sure it is between 0 and 1 with min and max operations,
      # this is neccessary since stringdist implementation is sloppy.
      paste("SoundEx similarity:\n", toString(min(1, max(1 - stringdist(s1, s2, method = "soundex"), 0))))
    })
    # Only output this similarity if according checkbox is activated.
    output$sim9 <- renderText({
      if(input$SoundEx) {
        simSoundEx()
      }
    })
  }
)