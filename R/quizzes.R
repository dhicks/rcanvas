#' Function to list all quizzes
#'
#' @param course_id Course ID
#'
#' @return data frame
#' @export
#'
#' @examples
#' #' get_quiz_list()
get_quiz_list <- function(course_id = NULL) {
  stopifnot(!is.null(course_id))

  url <- make_canvas_url("courses", course_id, "quizzes")

  args <- list(per_page = 100)

  process_response(url, args)

}

#' Get a single Quiz object
#'
#' @param course_id Course ID
#' @param quiz_id Quiz ID
#'
#' @return Data frame representation of the Quiz object
#' @export
get_quiz = function (course_id, quiz_id) {
  url = make_canvas_url('courses', course_id,
                        'quizzes', quiz_id)
  response = canvas_query(url)
  quiz = response %>%
    httr::content(as = 'text') %>%
    jsonlite::fromJSON()
  return(quiz)
}

