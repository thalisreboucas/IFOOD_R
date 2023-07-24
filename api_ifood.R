pacman::p_load(httr,tidyverse)

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

token <- token(clientId,clientSecret)$accessToken

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

get_restaurants(token)

id <- get_restaurants(token)[[1]]$id



get_merchant <- function(id_restaurant){
  # Define the URL and headers
  url <- "https://merchant-api.ifood.com.br/merchant/v1.0/merchants/"
  headers <- c(
    "accept" = "application/json",
    "Authorization" = paste0("Bearer"," ",token[1])
  )
  
  # Make the GET request
  response <- httr::GET(paste0(url,id_restaurant), httr::add_headers(.headers = headers))
  
  # Check the status code of the response
  status_code <- httr::status_code(response)
  
  # Check the content of the response
  content <- httr::content(response)
  return(content)
}
get_merchant(id)

get_merchant_status <- function(id_restaurant){
  # Define the URL and headers
  url <- "https://merchant-api.ifood.com.br/merchant/v1.0/merchants/"
  url_I <- paste0(url,id_restaurant) 
  url_final <- paste0(url_I,"/status")
  headers <- c(
    "accept" = "application/json",
    "Authorization" = paste0("Bearer"," ",token[1])
  )
  
  # Make the GET request
  response <- httr::GET(url_final, httr::add_headers(.headers = headers))
  
  # Check the status code of the response
  status_code <- httr::status_code(response)
  
  # Check the content of the response
  content <- httr::content(response)
  return(content)
}
get_merchant_status(id)


get_merchant_operation <- function(id_restaurant,operation){
  # Define the URL and headers
  url <- "https://merchant-api.ifood.com.br/merchant/v1.0/merchants/"
  url_I <- paste0(url,id_restaurant) 
  url_final <- paste0(url_I,"/status","/",operation)
  headers <- c(
    "accept" = "application/json",
    "Authorization" = paste0("Bearer"," ",token[1])
  )
  
  # Make the GET request
  response <- httr::GET(url_final, httr::add_headers(.headers = headers))
  
  # Check the status code of the response
  status_code <- httr::status_code(response)
  
  # Check the content of the response
  content <- httr::content(response)
  return(content)
}

get_merchant_operation(id,"string")

