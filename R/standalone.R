#source("R/MSAT.R")
options(shiny.error = browser)
debug_locally <- !grepl("shiny-server", getwd())


#' Standalone MSAT
#'
#' This function launches a standalone testing session for the MSAT.
#' This can be used for data collection, either in the laboratory or online.
#' @param title (Scalar character) Title to display during testing.
#' @param num_items (Scalar integer) Number of items to be adminstered.
#' @param with_id (Scalar boolean) Indicates, if ID should be asked for. Defaults to TRUE
#' @param with_feedback (Scalar boolean) Indicates if performance feedback will be given at the end of the test. Defaults to  FALSE
#' @param with_welcome (Scalar boolean) Indicates, if a welcome page shall be displayed.  Defaults to  TRUE
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' @param languages (Character vector)
#' Determines the languages available to participants.
#' Possible languages include English (\code{"en"}),
#' German (\code{"de"}),  Russian (\code{"ru"}), and Nederlands (\code{"nl"}).
#' The first language is selected by default
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param validate_id (Character scalar or closure) Function for validating IDs or string "auto" for default validation
#' which means ID should consist only of  alphanumeric characters.
#' @param ... Further arguments to be passed to \code{\link{MSAT}()}.
#' @export



MSAT_standalone  <- function(title = NULL,
                           num_items = 18L,
                           with_id = TRUE,
                           with_feedback = FALSE,
                           with_welcome = TRUE,
                           admin_password = "test123",
                           researcher_email = "robin.hake@uni-oldenburg.de",
                           languages = c("en", "de"),
                           dict = MSAT::MSAT_dict,
                           validate_id = "auto",
                           ...) {
  feedback <- NULL
  key <- NULL
  if(with_feedback) {
    feedback <- MSAT::MSAT_feedback_with_graph()
    #feedback <- MSAT::MSAT_feedback_with_score()
  }
  elts <- psychTestR::join(
    if(with_id)
      psychTestR::new_timeline(
        psychTestR::get_p_id(prompt = psychTestR::i18n("ENTER_ID"),
                             button_text = psychTestR::i18n("CONTINUE"),
                             validate = validate_id),
        dict = dict),
    if(with_welcome) MSAT_welcome_page(dict = dict),
    MSAT::MSAT(num_items = num_items,
      with_welcome =  FALSE,
      with_finish = FALSE,
      feedback = feedback,
      dict = dict,
      ...),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    MSAT_final_page(dict = dict)
  )
  if(is.null(title)){
    #extract title as named vector from dictionary
    title <-
      MSAT::MSAT_dict  %>%
      as.data.frame() %>%
      dplyr::filter(key == "TESTNAME") %>%
      dplyr::select(-key) %>%
      as.list() %>%
      unlist()
    names(title) <- tolower(names(title))
  }

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = FALSE,
                                   languages = tolower(languages)))
}

