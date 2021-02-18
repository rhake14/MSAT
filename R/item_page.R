
media_js <- list(
  media_not_played = "var media_played = false;",
  media_played = "media_played = true;",
  play_media = "document.getElementById('media').play();",
  show_media   = paste0("if (!media_played) ",
                        "{document.getElementById('media')",
                        ".style.visibility='inherit'};"),
  hide_media   = paste0("if (media_played) ",
                          "{document.getElementById('media')",
                          ".style.visibility='hidden'};"),
  show_media_btn = paste0("if (!media_played) ",
                          "{document.getElementById('btn_play_media')",
                          ".style.visibility='inherit'};"),
  hide_media_btn = paste0("document.getElementById('btn_play_media')",
                          ".style.visibility='hidden';"),
  show_responses = "document.getElementById('response_ui').style.visibility = 'inherit';"
)

#media_mobile_play_button <- shiny::tags$button(
#  shiny::tags$strong(psychTestR::i18n("CLICK_HERE_TO_PLAY")),
#  id = "btn_play_media",
#  style = "visibility: visible;height: 50px",
#  onclick = media_js$play_media
#)

media_mobile_play_button <- shiny::tags$p(
  shiny::tags$button(shiny::tags$span("\u25B6"),
                     type = "button",
                     id = "btn_play_media",
                     style = "visibility: hidden",
                     onclick = media_js$play_media)
)
get_audio_ui <- function(url,
                         type = tools::file_ext(url),
                         autoplay = TRUE,
                         width = 0,
                         wait = TRUE,
                         loop = FALSE) {
  #print(url)
  stopifnot(purrr::is_scalar_character(url),
            purrr::is_scalar_character(type),
            purrr::is_scalar_logical(wait),
            purrr::is_scalar_logical(loop))
  src    <- shiny::tags$source(src = url, type = paste0("audio/", type))
  script <- shiny::tags$script(shiny::HTML(media_js$media_not_played))
  audio  <- shiny::tags$audio(
    script,
    src,
    id = "media",
    preload = "auto",
    autoplay = if(autoplay) "autoplay",
    width = width,
    loop = if (loop) "loop",
    oncanplaythrough = media_js$show_media_btn,
    onplay = paste0(media_js$media_played, media_js$hide_media_btn),
    #onended = if (wait) paste0(media_js$show_responses, media_js$hide_media) else "null",
    onended = if (wait) media_js$show_responses else "null"
  )
  shiny::tags$div(audio, media_mobile_play_button)
}

get_audio_element <- function(url,
                              type = tools::file_ext(url),
                              wait = F,
                              autoplay = FALSE,
                              width = 200,
                              height = 50,
                              id = "media") {
  #print(url)
  stopifnot(purrr::is_scalar_character(url),
            purrr::is_scalar_character(type)
            )
  src    <- shiny::tags$source(src = url, type = paste0("audio/", type))
  script <- shiny::tags$script(shiny::HTML(media_js$media_not_played))
  audio  <- shiny::tags$audio(
    src,
    script,
    id = id,
    preload = "auto",
    controls = "controls",
    controlslist = "nodownload noremoteplayback",
    autoplay = if(autoplay) "autoplay",
    width = width,
    height = height,
    onplay = paste0(media_js$media_played, media_js$hide_media),
    onended = if (wait) paste0(media_js$show_responses, media_js$hide_media) else "null"
  )
  audio
}

audio_NAFC_page_flex <- function(label,
                                 prompt,
                                 choices,
                                 audio_url,
                                 save_answer = TRUE,
                                 on_complete = NULL,
                                 admin_ui = NULL) {
  stopifnot(purrr::is_scalar_character(label))
  #audio_ui <- get_audio_ui(audio_url, wait = T, loop = F)
  audio_ui <- get_audio_ui(audio_url, wait = T, loop = F, width = 200)
  #audio_ui <- get_audio_element(audio_url, autoplay = T, wait = T, width = 50)

  style <- NULL
  ui <- shiny::div(
    tagify(prompt),
    audio_ui,
    psychTestR::make_ui_NAFC(choices,
                 labels = choices,
                 hide = TRUE,
                 arrange_vertically = FALSE,
                 id = "response_ui")
    )
  get_answer <- function(input, ...) {
    answer <- as.numeric(gsub("answer", "", input$last_btn_pressed))
    correct <- MSAT::MSAT_item_bank[MSAT::MSAT_item_bank$item_number == label,]$correct == answer
    tibble(answer = answer,
         label = label,
         correct = correct)

  }
  validate <- function(answer, ...) !is.null(answer)
  #printf("[audio_NAFC_page_with_img] left")
  psychTestR::page(ui = ui, label = label,  get_answer = get_answer, save_answer = save_answer,
                   validate = validate, on_complete = on_complete, final = FALSE,
                   admin_ui = admin_ui)
}

MSAT_item <- function(label = "",
                     correct_answer,
                     prompt = "",
                     audio_file,
                     audio_dir = "",
                     save_answer = TRUE,
                     on_complete = NULL
                     ){

  page_prompt <- shiny::div(prompt)
  #printf("MSAT item_called for  %s", label)

  choices <- c("1", "2")
  audio_url <- file.path(audio_dir, audio_file)
  audio_NAFC_page_flex(label = label,
                       prompt = page_prompt,
                       audio_url = audio_url,
                       choices = choices,
                       save_answer = save_answer,
                       on_complete = on_complete)
}

