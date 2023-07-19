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


token <- function(clientId,clientSecret){
  # Define the URL and parameters
  url <- "https://merchant-api.ifood.com.br/authentication/v1.0/oauth/token"
  headers <- c(
    "accept" = "application/json",
    "Content-Type" = "application/x-www-form-urlencoded"
  )
  data <- list(grantType = "client_credentials",
               clientId = clientId[1],
               clientSecret = clientSecret[1],
               authorizationCode = "",
               authorizationCodeVerifier = "",
               refreshToken = ""
               )
  
  # Make the POST request
  response <- httr::POST(url, httr::add_headers(.headers=headers), body = data, encode = "form")
  
  # Check the status code of the response
  status_code <- httr::status_code(response)
  
  # Check the content of the response
  content <- httr::content(response)
  
  return(content)
}

clientId = "35e0b0f4-6f91-473d-acd1-98b7b310924e"
clientSecret = "fx7vre8dxhpyztxdxmoyvwu5nv9zp9pi92eji4b72fmevu67npnvkura2xwh9lzlu2hast5e6m9vfnpkp12efmxsbha38n7y9c2"

token(clientId,clientSecret)

get_restaurants <- function(token) {
  # Define the URL and headers
  url <- "https://merchant-api.ifood.com.br/merchant/v1.0/merchants"
  headers <- c(
    "accept" = "application/json",
    "Authorization" = paste0("Bearer"," ",token[1])
  )
  
  # Make the GET request
  response <- httr::GET(url, httr::add_headers(.headers = headers))
  
  # Check the status code of the response
  status_code <- httr::status_code(response)
  
  # Check the content of the response
  content <- httr::content(response)
  return(content)
}

get_restaurants(a$accessToken[1])

