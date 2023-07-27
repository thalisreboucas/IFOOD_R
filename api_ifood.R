pacman::p_load(httr,tidyverse)

###################
#clientId = "bc8d66a7-d279-49dd-8235-78dc57dd28b7"
#UserCode <- function(clientId){
# Define the URL and parameters
#url <- "https://merchant-api.ifood.com.br/authentication/v1.0/oauth#/userCode"
#headers <- c(
#  "accept" = "application/json",
#  "Content-Type" = "application/x-www-form-urlencoded"
#)
#data <- list(clientId = clientId[1])
#
## Make the POST request
#response <- httr::POST(url, httr::add_headers(.headers=headers), body = #data, encode = "form")
#
## Check the status code of the response
#status_code <- httr::status_code(response)
#
## Check the content of the response
#content <- httr::content(response)
#
#return(content)
#}
#UserCode(clientId)#####
#########0Auth#############
clientId = "35e0b0f4-6f91-473d-acd1-98b7b310924e"
clientSecret = "fx7vre8dxhpyztxdxmoyvwu5nv9zp9pi92eji4b72fmevu67npnvkura2xwh9lzlu2hast5e6m9vfnpkp12efmxsbha38n7y9c2"
get_acessToken <- function(clientId,clientSecret){
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
  response <- httr::POST(url,
                         httr::add_headers(.headers=headers),
                         body = data, encode = "form")
  
  # Check the content of the response
  content <- httr::content(response) |> as.data.frame()
  
  # Check if the response is successful (status code 200)
  if (httr::status_code(response) == 200) {
    # Return the content of the response
    return(content)
  } else {
    # Return an error message if the request was not successful
    stop(content$message)
 }
}
acessToken <- get_acessToken(clientId,clientSecret)

#########Merchant##########
get_merchant <- function(acessToken) {
  # Define the URL and headers
  url <- "https://merchant-api.ifood.com.br/merchant/v1.0/merchants"
  headers <- c(
    "accept" = "application/json",
    "Authorization" = paste0("Bearer"," ",acessToken[1])
  )
  
  # Make the GET request
  response <- httr::GET(url, httr::add_headers(.headers = headers))
  
  # Check the content of the response
  content <- httr::content(response) |> as.data.frame()

  # Check the content of the response
  content <- httr::content(response)  |> 
    purrr::pluck(1) |>
    purrr::map_df(1) 
  
  # Check if the response is successful (status code 200)
  if (httr::status_code(response) == 200) {
    # Return the content of the response
    return(content)
  } else {
    # Return an error message if the request was not successful
    stop(content$message)
  }
}
get_merchant(acessToken) 
id <- get_merchant(acessToken)$id

get_merchant_status <- function(id_restaurant){
  # Define the URL and headers
  url <- "https://merchant-api.ifood.com.br/merchant/v1.0/merchants/"
  url_I <- paste0(url,id_restaurant) 
  url_final <- paste0(url_I,"/status")
  headers <- c(
    "accept" = "application/json",
    "Authorization" = paste0("Bearer"," ",acessToken[1])
  )
  
  # Make the GET request
  response <- httr::GET(url_final, httr::add_headers(.headers = headers))
  
  # Check the status code of the response
  status_code <- httr::status_code(response)
  
  # Check the content of the response
  content <- httr::content(response) |> 
             purrr::pluck(1) |> 
             purrr::map_df(1)
  # Check if the response is successful (status code 200)
  if (httr::status_code(response) == 200) {
    # Return the content of the response
    return(content[4,])
  } else {
    # Return an error message if the request was not successful
    stop(content$message)
  }
}
get_merchant_status(id)

get_merchant_operation <- function(id_restaurant,operation){
  # Define the URL and headers
  url <- "https://merchant-api.ifood.com.br/merchant/v1.0/merchants/"
  url_I <- paste0(url,id_restaurant) 
  url_final <- paste0(url_I,"/status","/",operation)
  headers <- c(
    "accept" = "application/json",
    "Authorization" = paste0("Bearer"," ",acessToken[1])
  )
  
  # Define the query parameters
  query_parameters <- list(
    types = "PLC,REC,CFM",
    groups = "ORDER_STATUS,DELIVERY"
  )
  
  # Make the GET request
  response <- httr::GET(url_final, 
                        httr::add_headers(.headers = headers),
                        query = query_parameters)
  
  
  # Check the content of the response
  content <- httr::content(response)  |> 
    purrr::pluck(1) |>
    purrr::map_df(1) 

  # Check if the response is successful (status code 200)
  if (httr::status_code(response) == 200) {
    # Return the content of the response
    return(content)
  } else {
    # Return an error message if the request was not successful
    stop(content$message)
  
  } 
}
get_merchant_operation(id,"string")

# Call the function to orders
get_order <- function(){
  # Define the URL and headers
  url <- "https://merchant-api.ifood.com.br/order/v1.0/events:polling?"
  headers <- c(
    "accept" = "application/json",
    "Authorization" = paste0("Bearer"," ",acessToken[1])
  )
  
  # Define the query parameters
  query_parameters <- list(
    types = "PLC,REC,RTP,DSP,CON,CAN,CFM",
    groups = "ORDER_STATUS,
              CANCELLATION_REQUEST,
              DELIVERY,
              DELIVERY_ONDEMAND,
              ORDER_MODIFIER,
              ORDER_TAKEOUT"
  )
  
  # Make the GET request
  response <- httr::GET(url, 
                        httr::add_headers(.headers = headers),
                        query = query_parameters)
  
  
  content <- httr::status_code(response)
  
  # Check if the response is successful (status code 200)
  if (httr::status_code(response) == 200) {
    # Return the content of the response
    return(content)
  } else {
    # Return an error message if the request was not successful
    stop(content)
  }
}
get_order()

# Function to make the API call and return the response content
post_ifood_acknowledgment <- function(id_restaurant,id_order) {
  # Define the URL and headers
  url <- "https://merchant-api.ifood.com.br/order/v1.0/events/acknowledgment"
  headers <- c(
    "accept" = "*/*",
    "Authorization" = paste0("Bearer"," ",acessToken[1]),
    "Content-Type" = "application/json"
  )
  
  # Define the JSON data
  json_data <-  paste0('[{"id":' ,
                id_restaurant,
                 '}','{"id":',
                id_order,
                '"}]')
  
  # Make the POST request
  response <- httr::POST(url, httr::add_headers(.headers = headers), body = json_data)
  
  # Check if the response is successful (status code 200)
  if (httr::status_code(response) == 200) {
    # Return the content of the response
    return(httr::content(response))
  } else {
    # Return an error message if the request was not successful
    stop(content$message)
  }
}
post_ifood_acknowledgment()

# sales for processing and order
get_ifood_sales_processing <- function(merchant_id, begin_date, end_date){
# putting date in function
  # Define the URL and headers
  url <-  paste0("https://merchant-api.ifood.com.br/financial/v2.0/merchants/", merchant_id, "/sales")
  headers <- c(
    "accept" = "application/json",
    "Authorization" = paste0("Bearer"," ",acessToken[1])
  )
  
  #query_parameters <- list(
  #  beginLastProcessingDate = begin_date,
  #  endLastProcessingDate = end_date)
  
  query_parameters <- list(
    beginOrderDate = begin_date,
    endOrderDate = end_date)
  
  
  # Make the GET request
  response <- httr::GET(url, 
                        httr::add_headers(.headers = headers),
                        query = query_parameters)
  
  # Check the content of the response
  content <- httr::content(response)
  
  # Check if the response is successful (status code 200)
  if (httr::status_code(response) == 200) {
    # Return the content of the response
    return(content)
    #return(content)
  } else {
    # Return an error message if the request was not successful
    stop(content)
 }
} 
get_ifood_sales_processing(id,"2023-07-26","2023-07-30")


