#' Upload a file to a course
#'
#' @param course_id valid course id
#' @param file_name file name in your current directory.
#' Any UTF-8 name is allowed. Path components such as `/` and `\` will be treated as part of the filename,
#' not a path to a sub-folder.
#' @param parent_folder_id The id of the folder to store the file in.
#' If this and parent_folder_path are sent an error will be returned. If neither is given,
#' a default folder will be used.
#' @param parent_folder_path The path of the folder to store the file in. The path separator is the forward slash `/`, never a back slash.
#' The folder will be created if it does not already exist. This parameter only applies to file uploads
#' in a context that has folders, such as a user, a course, or a group. If this and parent_folder_id are
#' sent an error will be returned. If neither is given, a default folder will be used.
#' @param on_duplicate How to handle duplicate filenames. If "overwrite", then this file upload will
#' overwrite any other file in the folder with the same name. If "rename", then this file will be
#' renamed if another file in the folder exists with the given name. If no parameter is given,
#' This doesn't apply to file uploads in a context that doesn't have folders.
#'
#' @return invisible
#' @export
#'
#' @examples
#' upload_course_file(course_id = 13212, file_name = "activity.pdf")
upload_course_file <- function(course_id, file_name, parent_folder_id = NULL, parent_folder_path = "/", on_duplicate = "overwrite") {

  url <- make_canvas_url("courses", course_id, "files")
  upload_file(url, file_name, parent_folder_id, parent_folder_path, on_duplicate)

}

#' @export
upload_assignment_file <- function(course_id, assignment_id, user_id,
                                   file_name,
                                   parent_folder_id = NULL,
                                   parent_folder_path = "/",
                                   on_duplicate = "overwrite") {

  url <- make_canvas_url("courses", course_id, "assignments", assignment_id,
                         "submissions", user_id, "files")
  upload_file(url, file_name, parent_folder_id, parent_folder_path, on_duplicate)

}

upload_file <- function(url, file_name, parent_folder_id = NULL, parent_folder_path = "/", on_duplicate = "overwrite") {
  if (!is.null(parent_folder_id) && !is.null(parent_folder_path)) stop("Do not specify both parent folder id and parent folder path.")
  file_size <- file.info(file_name)$size
  args <- sc(list(name = file_name,
                  size = file_size,
                  parent_folder_id = parent_folder_id,
                  parent_folder_path = parent_folder_path,
                  on_duplicate = on_duplicate))
  upload_resp <- canvas_query(url, args, "POST")
  upload_content <- httr::content(upload_resp)
  upload_url <- upload_content$upload_url
  upload_params <- upload_content$upload_params
  upload_params <- append(upload_params,
                          list(file = httr::upload_file(file_name)))
  message(sprintf("File %s uploaded", file_name))
  invisible(httr::POST(url = upload_url,
                       body = upload_params))
}

#' Create a course folder
#'
#' @param course_id a valid course id
#' @param name name of the folder (required)
#' @param parent_folder_id The id of the folder to store the file in. If this and parent_folder_path
#' are sent an error will be returned. If neither is given, a default folder will be used.
#'
#' @return invisible
#' @export
#'
#' @examples
#' create_course_folder(34232, name = "activities")
create_course_folder <- function(course_id, name, parent_folder_id = NULL) {
  url <- paste0(canvas_url(),
                paste("courses", course_id, "folders", sep = "/"))
  args <- sc(list(name = name,
                  parent_folder_id = parent_folder_id))
  invisible(canvas_query(url, args, "POST"))
  message(sprintf("Folder %s created", name))
}

#' Create a course assignment
#'
#' @param course_id a valid course id
#' @param name the assignment name (only parameter required)
#' @param position integer - The position of this assignment in the group when displaying assignment lists.
#' @param submission_types string - List of supported submission types for the assignment. Unless the assignment is allowing online submissions, the array should only have one element. Options: online_quiz, none, on_paper, discussion_topic, external_tool, online_upload, online_text_entry, online_url, media_recording
#' @param allowed_extensions Allowed extensions if submission_types includes “online_upload”. E.g. "docx", "png".
#' @param turnitin_enabled boolean - Only applies when the Turnitin plugin is enabled for a course and the submission_types array includes “online_upload”. Toggles Turnitin submissions for the assignment. Will be ignored if Turnitin is not available for the course.
#' @param vericite_enabled boolean - Only applies when the VeriCite plugin is enabled for a course and the submission_types array includes “online_upload”. Toggles VeriCite submissions for the assignment. Will be ignored if VeriCite is not available for the course.
#' @param turnitin_settings string - 	Settings to send along to turnitin. See Assignment object definition for format.
#' @param integration_data string - Data used for SIS integrations. Requires admin-level token with the “Manage SIS” permission. JSON string required.
#' @param integration_id string - Unique ID from third party integrations
#' @param peer_reviews boolean - If submission_types does not include external_tool,discussion_topic, online_quiz, or on_paper, determines whether or not peer reviews will be turned on for the assignment.
#' @param automatic_peer_reviews boolean - Whether peer reviews will be assigned automatically by Canvas or if teachers must manually assign peer reviews. Does not apply if peer reviews are not enabled.
#' @param notify_of_update boolean - If true, Canvas will send a notification to students in the class notifying them that the content has changed.
#' @param group_category_id integer - If present, the assignment will become a group assignment assigned to the group.
#' @param grade_group_students_individually boolean - If this is a group assignment, teachers have the options to grade students individually. If false, Canvas will apply the assignment's score to each member of the group. If true, the teacher can manually assign scores to each member of the group.
#' @param external_tool_tag_attributes string - Hash of external tool parameters if submission_types is external_tool. See Assignment object definition for format.
#' @param points_possible number - The maximum points possible on the assignment.
#' @param grading_type string - The strategy used for grading the assignment. The assignment defaults to “points” if this field is omitted. Options: pass_fail, percent, letter_grade, gpa_scale, points
#' @param due_at datetime - The day/time the assignment is due. Must be between the lock dates if there are lock dates. Accepts times in ISO 8601 format, e.g. 2014-10-21T18:48:00Z.
#' @param lock_at datetime - The day/time the assignment is locked after. Must be after the due date if there is a due date. Accepts times in ISO 8601 format, e.g. 2014-10-21T18:48:00Z.
#' @param unlock_at datetime - The day/time the assignment is unlocked. Must be before the due date if there is a due date. Accepts times in ISO 8601 format, e.g. 2014-10-21T18:48:00Z.
#' @param description string - The assignment's description, supports HTML.
#' @param assignment_group_id number - The assignment group id to put the assignment in. Defaults to the top assignment group in the course.
#' @param muted boolean - Whether this assignment is muted. A muted assignment does not send change notifications and hides grades from students. Defaults to false.
#' @param assignment_overrides List of overrides for the assignment.
#' @param only_visible_to_overrides boolean - Whether this assignment is only visible to overrides (Only useful if 'differentiated assignments' account setting is on)
#' @param published boolean - Whether this assignment is published. (Only useful if 'draft state' account setting is on) Unpublished assignments are not visible to students.
#' @param grading_standard_id integer - The grading standard id to set for the course. If no value is provided for this argument the current grading_standard will be un-set from this course. This will update the grading_type for the course to letter_grade' unless it is already 'gpa_scale'.
#' @param omit_from_final_grade boolean - Whether this assignment is counted towards a student's final grade.
#' @param quiz_lti boolean - Whether this assignment should use the Quizzes 2 LTI tool. Sets the submission type to 'external_tool' and configures the external tool attributes to use the Quizzes 2 LTI tool configured for this course. Has no effect if no Quizzes 2 LTI tool is configured.
#'
#' @return
#' @export
#'
#' @examples
#' create_course_assignment(course_id = 432432, name = "Challenging Assignment")
#' create_course_assignment(course_id = 3432432, name = "R Packages, Review", peer_reviews = TRUE, points_possible = 100, omit_from_final_grade = TRUE)
create_course_assignment <- function(course_id, name, position = NULL, submission_types = NULL, allowed_extensions = NULL, turnitin_enabled = NULL,
                                    vericite_enabled = NULL, turnitin_settings = NULL, integration_data = NULL, integration_id = NULL, peer_reviews = NULL,
                                    automatic_peer_reviews = NULL, notify_of_update = NULL, group_category_id = NULL, grade_group_students_individually = NULL,
                                    external_tool_tag_attributes = NULL, points_possible = NULL, grading_type = NULL, due_at = NULL,
                                    lock_at = NULL, unlock_at = NULL, description = NULL, assignment_group_id = NULL, muted = NULL,
                                    assignment_overrides = NULL, only_visible_to_overrides = NULL, published = NULL, grading_standard_id = NULL,
                                    omit_from_final_grade = NULL, quiz_lti = NULL) {
  url <- make_canvas_url("courses", course_id, "assignments")
  args <- sc(list(name = name,
                  position = position,
                  submission_types = submission_types,
                  allowed_extensions = allowed_extensions,
                  turnitin_enabled = turnitin_enabled,
                  vericite_enabled = vericite_enabled,
                  turnitin_settings = turnitin_settings,
                  integration_data = integration_data,
                  integration_id = integration_id,
                  peer_reviews = peer_reviews,
                  automatic_peer_reviews = automatic_peer_reviews,
                  notify_of_update = notify_of_update,
                  group_category_id = group_category_id,
                  grade_group_students_individually = grade_group_students_individually,
                  external_tool_tag_attributes = external_tool_tag_attributes,
                  points_possible = points_possible,
                  grading_type = grading_type,
                  due_at = due_at,
                  lock_at = lock_at,
                  unlock_at = unlock_at,
                  description = description,
                  assignment_group_id = assignment_group_id,
                  muted = muted,
                  assignment_overrides = assignment_overrides,
                  only_visible_to_overrides = only_visible_to_overrides,
                  published = published,
                  grading_standard_id = grading_standard_id,
                  omit_from_final_grade = omit_from_final_grade,
                  quiz_lti = quiz_lti))
  names(args) <- sprintf("assignment[%s]", names(args))
  invisible(canvas_query(url, args, "POST"))
  message(sprintf("Assignment %s created.", name))
}

#' Create a course quiz
#'
#' @param course_id a valid course id
#' @param title The quiz title (required)
#' @param description a description of the quiz
#' @param quiz_type type of quiz. Options: practice_quiz, assignment, graded_survey, survey
#' @param assignment_group_id The assignment group id to put the assignment in. Defaults to the top assignment group in the course. Only valid if the quiz is graded, i.e. if quiz_type is “assignment” or “graded_survey”.
#' @param time_limit Time limit to take this quiz, in minutes. Set to null for no time limit. Defaults to null.
#' @param shuffle_answers If true, quiz answers for multiple choice questions will be randomized for each student. Defaults to false.
#' @param hide_results Dictates whether or not quiz results are hidden from students. If null, students can see their results after any attempt. If “always”, students can never see their results. If “until_after_last_attempt”, students can only see results after their last attempt. (Only valid if allowed_attempts > 1). Defaults to null. Options: always, until_after_last_attempt
#' @param show_correct_answers Only valid if hide_results=null If false, hides correct answers from students when quiz results are viewed. Defaults to true.
#' @param show_correct_answers_last_attempt Only valid if show_correct_answers=true and allowed_attempts > 1 If true, hides correct answers from students when quiz results are viewed until they submit the last attempt for the quiz. Defaults to false.
#' @param show_correct_answers_at Only valid if show_correct_answers=true If set, the correct answers will be visible by students only after this date, otherwise the correct answers are visible once the student hands in their quiz submission.
#' @param hide_correct_answers_at Only valid if show_correct_answers=true If set, the correct answers will stop being visible once this date has passed. Otherwise, the correct answers will be visible indefinitely.
#' @param allowed_attempts Number of times a student is allowed to take a quiz. Set to -1 for unlimited attempts. Defaults to 1.
#' @param scoring_policy Required and only valid if allowed_attempts > 1. Scoring policy for a quiz that students can take multiple times. Defaults to “keep_highest”. Options: keep_highest, keep_latest
#' @param one_question_at_a_time If true, shows quiz to student one question at a time. Defaults to false.
#' @param cant_go_back Only valid if one_question_at_a_time=true If true, questions are locked after answering. Defaults to false.
#' @param access_code Restricts access to the quiz with a password. For no access code restriction, set to null. Defaults to null.
#' @param ip_filter Restricts access to the quiz to computers in a specified IP range. Filters can be a comma-separated list of addresses, or an address followed by a mask.  Examples: "192.168.217.1"; "192.168.217.1/24"; "192.168.217.1/255.255.255.0". For no IP filter restrictions, set to null. Defaults to null.
#' @param due_at The day/time the quiz is due. Accepts times in ISO 8601 format, e.g. 2011-10-21T18:48Z.
#' @param lock_at The day/time the quiz is locked for students. Accepts times in ISO 8601 format, e.g. 2011-10-21T18:48Z.
#' @param unlock_at The day/time the quiz is unlocked for students. Accepts times in ISO 8601 format, e.g. 2011-10-21T18:48Z.
#' @param published Whether the quiz should have a draft state of published or unpublished. NOTE: If students have started taking the quiz, or there are any submissions for the quiz, you may not unpublish a quiz and will recieve an error.
#' @param one_time_results Whether students should be prevented from viewing their quiz results past the first time (right after they turn the quiz in.) Only valid if “hide_results” is not set to “always”. Defaults to false.
#' @param only_visible_to_overrides Whether this quiz is only visible to overrides (Only useful if 'differentiated assignments' account setting is on) Defaults to false.
#'
#' @return
#' @export
create_course_quiz <- function(course_id, title,
                               description = NULL,
                               quiz_type = NULL,
                               assignment_group_id = NULL,
                               time_limit = NULL,
                               shuffle_answers = NULL,
                               hide_results = NULL,
                               show_correct_answers = NULL,
                               show_correct_answers_last_attempt = NULL,
                               show_correct_answers_at = NULL,
                               hide_correct_answers_at = NULL,
                               allowed_attempts = NULL,
                               scoring_policy = NULL,
                               one_question_at_a_time = NULL,
                               cant_go_back = NULL,
                               access_code = NULL,
                               ip_filter = NULL,
                               due_at = NULL,
                               lock_at = NULL,
                               unlock_at = NULL,
                               published = NULL,
                               one_time_results = NULL,
                               only_visible_to_overrides = NULL) {
  url <- make_canvas_url("courses", course_id, "quizzes")
  args <- sc(list(title = title,
                  description = description,
                  quiz_type = quiz_type,
                  assignment_group_id = assignment_group_id,
                  time_limit = time_limit,
                  shuffle_answers = shuffle_answers,
                  hide_results = hide_results,
                  show_correct_answers = show_correct_answers,
                  show_correct_answers_last_attempt = show_correct_answers_last_attempt,
                  show_correct_answers_at = show_correct_answers_at,
                  hide_correct_answers_at = hide_correct_answers_at,
                  allowed_attempts = allowed_attempts,
                  scoring_policy = scoring_policy,
                  one_question_at_a_time = one_question_at_a_time,
                  cant_go_back = cant_go_back,
                  access_code = access_code,
                  ip_filter = ip_filter,
                  due_at = due_at,
                  lock_at = lock_at,
                  unlock_at = unlock_at,
                  published = published,
                  one_time_results = one_time_results,
                  only_visible_to_overrides = only_visible_to_overrides))
  names(args) <- sprintf("quiz[%s]", names(args))
  invisible(canvas_query(url, args, "POST"))
  message(sprintf("Quiz %s created.", title))
}
