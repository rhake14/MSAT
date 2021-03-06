#' Demo MSAT
#'
#' This function launches a demo for the RAT.
#'
#' @param num_items (Integer scalar) Number of items in the test.
#' @param feedback (Function) Defines the feedback to give the participant
#' at the end of the test. Defaults to a graph-based feedback page.
#' @param admin_password (Scalar character) Password for accessing the admin panel.
#' Defaults to \code{"demo"}.
#' @param researcher_email (Scalar character)
#' If not \code{NULL}, this researcher's email address is displayed
#' at the bottom of the screen so that online participants can ask for help.
#' Defaults to \email{longgold@gold.uc.ak},
#' the email address of this package's developer.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param language The language you want to run your demo in.
#' Possible languages include English (\code{"en"}), German (\code{"de"})).
#' The first language is selected by default
#' @param ... Further arguments to be passed to \code{\link{MSAT}()}.
#' @export
#'
MSAT_demo <- function(num_items = 3L,
                     feedback = MSAT::MSAT_feedback_with_score(),
                     admin_password = "test123",
                     researcher_email = "robin.hake@uni-oldenburg.de",
                     dict = MSAT::MSAT_dict,
                     language = "en",
                     ...) {
  elts <- psychTestR::join(
    MSAT_welcome_page(dict = dict),
    MSAT::MSAT(num_items = num_items,
             with_welcome = FALSE,
             feedback = feedback,
             dict = dict,
             ...),
      MSAT_final_page(dict = dict)
  )

  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = "Musical Scene Analysis Test",
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   demo = TRUE,
                                   languages = tolower(language)))
}
