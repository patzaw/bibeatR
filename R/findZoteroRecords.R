findZoteroRecords <- function(){

   ############################################################################@
   ## ui ----
   ui <- fluidPage(
      gadgetTitleBar("Find Zotero records"),
      fluidRow(
         column(
            width=3,
            textInput(
               inputId="request",
               label="Searched term",
               placeholder="An ID, a symbol or name"
            )
         ),
         column(
            width=3,
            uiOutput("uiBe")
         ),
         column(
            width=3,
            uiOutput("uiOrg")
         ),
         column(
            width=3,
            uiOutput("uiSource")
         )
      ),
      fluidRow(
         column(
            width=12,
            uiOutput(
               outputId="renderRes"
            )
         )
      ),
      fluidRow(
         column(
            width=7,
            checkboxInput(
               inputId="crossOrg",
               label=p(
                  strong("Cross species search"),
                  tags$small(
                     "(time consuming and not relevant for complex objects
                     such as GO functions)"
                  )
               ),
               value=FALSE
            )
         ),
         column(
            width=5,
            checkboxInput(
               inputId="showGeneAnno",
               label=p(
                  strong("Show gene annotation"),
                  tags$small(
                     "(not relevant for complex objects such as GO functions)"
                  )
               ),
               value=FALSE
            )
         )
      )
   )

   ############################################################################@
   ## server ----
   server <- function(input, output, session) {
   }


   ############################################################################@
   ## runGadget ----
   runGadget(
      ui, server,
      viewer = dialogViewer("Find Zotero records", height=560, width=850)
   )

}
