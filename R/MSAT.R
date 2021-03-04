library(tidyverse)
library(psychTestR)


#printf   <- function(...) print(sprintf(...))
#messagef <- function(...) message(sprintf(...))
#' MSAT
#'
#' This function defines a MSAT  module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the MSAT in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#'
#' For demoing the MSAT, consider using \code{\link{MSAT_demo}()}.
#' For a standalone implementation of the MSAT,
#' consider using \code{\link{MSAT_standalone}()}.
#' @param num_items (Integer scalar) Number of items in the test.
#' @param with_welcome (Scalar boolean) Indicates, if a welcome page shall be displayed. Defaults to  TRUE
#' @param with_finish (Scalar boolean) Indicates, if a finish (not final!) page shall be displayed. Defaults to  TRUE
#' @param label (Character scalar) Label to give the MSAT results in the output file.
#' @param feedback (Function) Defines the feedback to give the participant
#' at the end of the test.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export
MSAT <- function(num_items = 18L,
                with_welcome = TRUE,
                with_finish = TRUE,
                label = "MSAT",
                feedback = MSAT_feedback_with_score(),
                dict = MSAT::MSAT_dict) {
  # audio_dir <- "https://cloudstorage.elearning.uni-oldenburg.de/s/dX27M6zYjydbM6c/download"
  # audio_dir <- "https://cloudstorage.elearning.uni-oldenburg.de/s/dX27M6zYjydbM6c"
  # audio_dir <- "https://cloudstorage.elearning.uni-oldenburg.de/s/kMkEtZmEBz2tDDq"
  # audio_dir <- "C:/Users/Hake Home PC/Dropbox/Uni Oldenburg/#3 - RStudio dateien/#4 - MSAT - main test R script/MSA/stimuli"
  audio_dir <- "https://drive.google.com/drive/folders/1LbMJRNrY3mTf75D_q7HpatT3mRrmfjTz?usp=sharing"
  # audio_dir <- "https://media.gold-msi.org/test_materials/EDT"
  stopifnot(purrr::is_scalar_character(label),
            purrr::is_scalar_integer(num_items) || purrr::is_scalar_double(num_items),
            purrr::is_scalar_character(audio_dir),
            psychTestR::is.timeline(feedback) ||
              is.list(feedback) ||
              psychTestR::is.test_element(feedback) ||
              is.null(feedback))
  # audio_dir <- gsub("/$", "", audio_dir)

  psychTestR::join(
    psychTestR::begin_module(label),
    if (with_welcome) MSAT_welcome_page(),
    psychTestR::new_timeline({
      main_test(label = label, num_items_in_test = num_items, audio_dir = audio_dir, dict = dict)
    }, dict = dict),
    scoring(),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    feedback,
    if(with_finish) MSAT_finished_page(),
    psychTestR::end_module())
}
