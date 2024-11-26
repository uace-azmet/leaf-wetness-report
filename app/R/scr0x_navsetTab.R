navsetTab = bslib::navset_tab(
  bslib::nav_panel(
    title = "Current Conditions", 
    #p("First tab content."), 
    bslib::layout_columns(
      cardsCurrentConditions[[1]], 
      cardsCurrentConditions[[2]], 
      cardsCurrentConditions[[3]], 
      cardsCurrentConditions[[4]], 
      cardsCurrentConditions[[5]])
  ),
  bslib::nav_panel(title = "Recent Data", p("Second tab content.")),
  bslib::nav_panel(title = "Data Variables", p("A table with data variables."))
)
