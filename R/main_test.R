scoring <- function(){
  psychTestR::code_block(function(state,...){
    #browser()
    results <- psychTestR::get_results(state = state,
                                       complete = FALSE,
                                       add_session_info = FALSE) %>% as.list()

    sum_score <- sum(purrr::map_lgl(results$MSAT, function(x) x$correct))
    num_question <- length(results$MSAT)
    perc_correct <- sum_score/num_question
    psychTestR::save_result(place = state,
                 label = "score",
                 value = perc_correct)
    psychTestR::save_result(place = state,
                             label = "num_questions",
                             value = num_question)

  })

}


main_test <- function(label, num_items_in_test, audio_dir, dict = MSAT::MSAT_dict) {
  elts <- c()
  item_bank <- MSAT::MSAT_item_bank
  item_sequence <- sample(1:nrow(item_bank), num_items_in_test)
  #browser()
  for(i in 1:length(item_sequence)){
    item <- MSAT::MSAT_item_bank[item_sequence[i],]
    emotion <- psychTestR::i18n(item[1,]$emotion_i18)
    #printf("Emotion %s ", emotion)
    item_page <-
      MSAT_item(label = item$item_number[1],
               correct_answer = item$correct[1],
               prompt = get_prompt(i, num_items_in_test, emotion),
               audio_file = item$audio_file[1],
               audio_dir = audio_dir,
               save_answer = TRUE)
    elts <- psychTestR::join(elts, item_page)
  }
  elts
}


item_page <- function(item_number, item_id, num_items_in_test, audio_dir, dict = MSAT::MSAT_dict) {
  item <- MSAT::MSAT_item_bank %>% filter(item_number == item_id) %>% as.data.frame()
  emotion <- psychTestR::i18n(item[1,]$emotion_i18)
  MSAT_item(label = item_id,
           correct_answer = item$correct[1],
           prompt = get_prompt(item_number, num_items_in_test, emotion),
           audio_file = item$audio_file[1],
           audio_dir = audio_dir,
           save_answer = TRUE)
  # psychTestR::audio_NAFC_page(label = sprintf("%s-%s", item_number, num_items_in_test),
  #                promp = get_prompt(item_number, num_items_in_test, emotion),
  #                choices = c("1", "2"),
  #                url = file.path(audio_dir, item$audio_file[1]))
}

get_prompt <- function(item_number, num_items_in_test, emotion, dict = MSAT::MSAT_dict) {
  shiny::div(
    shiny::h4(
      psychTestR::i18n(
        "PROGRESS_TEXT",
        sub = list(num_question = item_number,
                   test_length = if (is.null(num_items_in_test))
                     "?" else
                       num_items_in_test)),
      style  = "text_align:left"
    ),
    shiny::p(
      psychTestR::i18n("ITEM_INSTRUCTION",
                       sub = list(emotion = emotion)),
      style = "margin-left:20%;margin-right:20%;text-align:justify")
    )
}

MSAT_welcome_page <- function(dict = MSAT::MSAT_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
    body = shiny::div(
      shiny::h4(psychTestR::i18n("WELCOME")),
      shiny::div(psychTestR::i18n("INTRO_TEXT"),
               style = "margin-left:0%;display:block")
    ),
    button_text = psychTestR::i18n("CONTINUE")
  ), dict = dict)
}

MSAT_finished_page <- function(dict = MSAT::MSAT_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body =  shiny::div(
        shiny::h4(psychTestR::i18n("THANKS")),
        psychTestR::i18n("SUCCESS"),
                         style = "margin-left:0%;display:block"),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}
MSAT_final_page <- function(dict = MSAT::MSAT_dict){
  psychTestR::new_timeline(
    psychTestR::final_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("THANKS")),
        shiny::div(psychTestR::i18n("SUCCESS"),
                   style = "margin-left:0%;display:block"),
        button_text = psychTestR::i18n("CONTINUE")
      )
    ), dict = dict)
}

