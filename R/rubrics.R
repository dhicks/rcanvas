#' Rubric functions
#' Functions for working with rubrics
#'
#' @param course_id A valid canvas course id
#' @return A dataframe of rubrics
#' @export
get_rubrics = function(course_id) {
    url <- make_canvas_url('courses', course_id, 'rubrics')
    args <- list(access_token = check_token(),
                 per_page = 100)
    response <- process_response(url, args)
    return(response)
}


#' Associate a rubric to an assignment
#'
#' TODO: Support all of the options in the API: <https://canvas.instructure.com/doc/api/rubrics.html#method.rubric_associations.create>
#'
#' @param course_id Course ID
#' @param rubric_id Rubric ID
#' @param association_id Assignment ID for the assignment to be associated with the rubric
#'
#' @return (Invisibly) dataframe representation of the RubricAssociation object
#' @export
associate_rubric = function(course_id,
                            rubric_id,
                            association_id) {
  url = make_canvas_url('courses', course_id,
                        'rubric_associations')
  args = sc(list(rubric_id = rubric_id,
                 association_id = association_id,
                 association_type = 'Assignment',
                 purpose = 'grading'
  ))
  names(args) <- sprintf("rubric_association[%s]", names(args))

  response = canvas_query(url, args, "POST")
  # message(Association created)
  association = response %>%
    httr::content(as = 'text') %>%
    jsonlite::fromJSON()
  return(invisible(association))
}

