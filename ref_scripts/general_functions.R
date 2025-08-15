github_push <- function(commit_msg = NULL) {
  if (is.null(commit_msg) || commit_msg == "") {
    stop("Please provide a commit message.")
  }
  
  project_dir <- getwd()
  git_dir <- file.path(project_dir, ".git")
  
  if (!dir.exists(git_dir)) {
    stop(paste0("Not a Git repo: ", project_dir, 
                "\nCheck that you're in the root of your Git project."))
  }
  
  message("Git repo detected at: ", project_dir)
  message("Staging all changes...")
  system("git add .")
  
  message("Committing with message: ", commit_msg)
  system(sprintf('git commit -m "%s"', commit_msg))
  
  message("Pushing to GitHub...")
  system("git push")
  
  message("Done! Changes pushed to GitHub.")
}

title_case <- function(x) {
  minor_words <- c("and", "or", "the", "of", "in", "on", "at", "by", "for", "to", "el", "la", "de", "du")
  sapply(x, function(val) {
    if (is.na(val)) return(NA_character_)
    val <- stringr::str_to_title(val)
    for (word in minor_words) {
      val <- gsub(paste0(" ", stringr::str_to_title(word), "\\b"),
                  paste0(" ", word), val)}
    val
  }, USE.NAMES = FALSE)
}