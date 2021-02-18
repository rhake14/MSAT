info_page <- function(id, style = "text-align:justify; margin-left:20%;margin-right:20%") {
  #messagef("Info page called with id %s and text %s", id, psychTestR::i18n(id, html = FALSE))
  psychTestR::one_button_page(shiny::div(psychTestR::i18n(id, html = TRUE), style = style),
                              button_text = psychTestR::i18n("CONTINUE"))
}
