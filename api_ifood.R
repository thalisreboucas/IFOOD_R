pacman::p_load(httr,curl,tidyverse)

clientId = "bc8d66a7-d279-49dd-8235-78dc57dd28b7"

UserCode <- function(clientId){
# Define the URL and parameters
url <- "https://merchant-api.ifood.com.br/authentication/v1.0/oauth/userCode"
headers <- c(
  "accept" = "application/json",
  "Content-Type" = "application/x-www-form-urlencoded"
)
data <- list(clientId = clientId[1])

# Make the POST request
response <- httr::POST(url, httr::add_headers(.headers=headers), body = data, encode = "form")

# Check the status code of the response
status_code <- httr::status_code(response)

# Check the content of the response
content <- httr::content(response)

return(content)
}

UserCode(clientId)
