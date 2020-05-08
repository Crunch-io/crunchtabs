crunchtabs_redact <- function(response) {
    ## Remove multipart form fields because POST sources/ sends a tmpfile path
    ## that's different every time, so the request will never match.
    response$request$fields <- NULL
    ## So that the login request isn't tied to one user's creds, ignore it in mocks
    if (response$url == "https://app.crunch.io/api/public/login/") {
        response$request$options[["postfields"]] <- NULL
    }
    response %>%
        redact_auth()
}