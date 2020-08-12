##################################################################

library(tidyverse)
library(anytime)
library(lubridate)
#devtools::install_github("ropenscilabs/roomba")
library(roomba)

##################################################################

subreddit_posts_prof <-
  read_csv("subreddit-posts-Professors-may2020-jun2020.csv") %>%
  mutate(post_date_time = post_date_time %>%
           as.numeric() %>%
           anytime(asUTC = TRUE) %>%
           as_datetime %>%
           ymd_hms() %>%
           with_tz(tzone = "US/Eastern")
  )


subreddit_posts_prof$num_comments # max number of comments (i.e., responses) is 330, in row 688

post_id_x <- subreddit_posts_prof$post_id[688]
# https://www.reddit.com/r/Professors/comments/gkrh0g/
# https://www.reddit.com/r/Professors/comments/gkrh0g/.json?limit=100&after=

get_post_json <-
  function(subreddit, x) {
    full_json <-
      httr::GET(
        url = paste0("https://www.reddit.com/r/", subreddit,
                     "/comments/",
                     x,
                     "/.json"),
        httr::add_headers("User-Agent" = Sys.getenv('reddit_user_agent')),
        httr::authenticate(Sys.getenv('reddit_client_id'),
                           Sys.getenv('reddit_client_token')
        )
      )

    httr::content(full_json, as = "parsed")
  }

parsed_json <-
  get_post_json("Professors", post_id_x)

#json_data <- parsed_json[[2]]$data$children[[1]]
#json_data_more <- parsed_json[[2]]$data$children[[50]]

get_one_response <-
  function(json_data) {
    if(json_data$kind == 'more') {
      return(NULL)
    } else {
      return(tibble::tibble(
        type = json_data$kind,
        response_id = json_data$data$id,
        response_author = json_data$data$author,
        response_datetime = json_data$data$created_utc,
        response_text = json_data$data$body,
        response_score = json_data$data$score,
        response_url = json_data$data$permalink,
        parent_id = json_data$data$parent_id)
      )
    }
  }  # end of function


get_responses_on_level <-
  function(x) {
    if(length(x) == 0) {
      return(NULL)
    } else {
      new_tibble <- NULL
      new_tibble <- map_df(x, ~ bind_rows(new_tibble, get_one_response(.x)))
      return(new_tibble)
    }
  }

##################################################################

#data_list <- list(data_in_json = parsed_json[[2]]$data$children, data_in_tibble = NULL)

get_responses_recursive <-
  function(data_list) {
    output <-
      list(data_in_json = data_list$data_in_json,
           data_in_tibble = data_list$data_in_tibble
      )

    for(i in 1:length(output$data_in_json)) {

      if(!is.null(output$data_in_json[[i]])) {

        if(is.null(output$data_in_tibble)) {
          output$data_in_tibble <-
            get_one_response(output$data_in_json[[i]])
        } else {
          output$data_in_tibble <-
            dplyr::bind_rows(output$data_in_tibble,
                             get_one_response(output$data_in_json[[i]])
            )
        }

        if(length(output$data_in_json[[i]]$data$replies$data$children) > 0) {
          new_level <- output
          new_level$data_in_json <- new_level$data_in_json[[i]]$data$replies$data$children
          get_responses_recursive(new_level)
        }

        #if(output$data_in_json[[i]]$data$replies != "") {
        #new_level <- output
        #new_level$data_in_json <- new_level$data_in_json[[i]]$data$replies$data$children
        #get_responses_recursive(new_level)
        #}

      }

    }  # end of FOR loop
    return(output)
  }  # end of function

starting_data <- list(data_in_json = parsed_json[[2]]$data$children, data_in_tibble = NULL)

tmp <- get_responses_recursive(starting_data)

tmp_end <-
  parsed_json[[2]]$data$children[[1]]$data$replies$data$children[[1]]$data$replies$data$children[[1]]$data$replies$data$children[[1]]
length(tmp_end$data$replies$data$children)

##################################################################










##################################################################
## I don't remember what I was trying to do after this
##################################################################


#json_level <- json_level[[1]]$data$replies$data$children[[1]]$data$replies$data$children[[1]]$data$replies$data$children
#i<-2

    get_responses_recursive <-
      function(json_level, existing_data = NULL) {

        for(i in 1:length(json_level)) {
          print(i)

          ##### Check A #####

          json2tibble <- function(json) {
            if(json$kind != "more") {
              tibble::tibble(
                type = json$kind,
                response_id = json$data$id,
                response_author = json$data$author,
                response_datetime = json$data$created_utc,
                response_text = json$data$body,
                response_score = json$data$score,
                response_url = json$data$permalink,
                parent_id = json$data$parent_id
              )
            }
          }

          if(json_level[[i]]$kind == 'more') {
            output_data <- existing_data
            return(output_data)
            message("Accessed 0 new responses due to check A")
          } else {
            if(is.null(existing_data)) {
              output_data <-
                json2tibble(json_level[[i]])
            } else {
              output_data <-
                dplyr::bind_rows(existing_data,
                                 json2tibble(json_level[[i]])
                                 )
            }
            message("Accessed 1 new response in check A.")

            ##### Check B #####

            if(json_level[[i]]$data$replies == "") {
              message("Accessed ", nrow(output_data), " new responses total in check B.")
              return(output_data)

            } else {
              new_level <-
                json_level[[i]]$data$replies$data$children
              get_responses_recursive(new_level, existing_data = output_data)
            }  # end of `else` for check B

          }  # end of `else` for check A
          print(output_data)
          return(output_data)
        }  # end of `for`` loop

      }  # end of function


    starting_level <- parsed_json_data[[2]]$data$children
    response_data <- get_responses_recursive(starting_level)









    ##################################################################

    response_data <-
      bind_rows(response_data,
                get_responses_on_level(jump_level(current_level[[1]]))
      )

    response_data <-
      bind_rows(response_data,
                get_responses_on_level(jump_level(current_level[[2]]))
      )

    response_data <-
      bind_rows(response_data,
                get_responses_on_level(jump_level(current_level[[3]]))
      )

    response_data <-
      bind_rows(response_data,
                get_responses_on_level(jump_level(current_level[[50]]))
      )


    next_level <- map(current_level, ~ get_responses_on_level(.x$data$replies$data$children))
    if(length(next_level))





    length(first_replies)
    jump_level(first_replies[[50]])

    if(parsed_json_data[[2]]$data$children[[1]]$kind == 't1') {
      new_tibble <-
        map_df(parsed_json_data[[2]]$data$children,
               ~ bind_rows(new_tibble, json2tibble(.x))
        )
    }

    if(parsed_json_data[[2]]$data$children[[1]]$kind == 't1') {
      new_tibble <-
        map_df(parsed_json_data[[2]]$data$children,
               ~ bind_rows(new_tibble, json2tibble(.x))
        )
    }

    more_replies <- NULL

    first_reply_data$type == "t1"
    json2tibble(parsed_json_data[[2]]$data$children[[1]])$type == "t1"
    if(json2tibble(x)[[i]]$type == 't1') {}

    if(length(more_replies) > 0) {

    }

  }




parsed_json_data[[2]]$data$children[[1]]$kind
parsed_json_data[[2]]$data$children[[50]]$kind


unthread <- function(x) {
  ifelse(length(x$data$children) > 1,
         x$data$children,
         x$data$replies$data$children)
}




get_first_reply_data <-
  function(parsed_json_data) {
    first_replies <- parsed_json_data[[2]]$data$children
    new_tibble <- tibble()
    map_df(first_replies,
           ~ bind_rows(new_tibble, json2tibble(.x))
           )
  }

first_replies_data <- get_first_reply_data(parsed_json)



parsed_json_data[[2]]$data$children[[1]]$data$replies$data$children[[1]]$data$replies$data$children[[1]]$data$replies$data$children

get_remaining_reply_data <-
  function(parsed_json_data) {
    first_replies <- parsed_json_data[[2]]$data$children
    next_replies <-
      map(first_replies, jump_level)

    new_tibble <- tibble()


    tmp <- jump_to_replies(parsed_json_data[[2]]$data$children[[1]])
    if(x) {replies <- jump_to_replies(x)}
    new_tibble <- tibble()
    map_df(replies, ~ bind_rows(new_tibble, json2tibble(.x)))

  }

get_remaining_reply_data(parsed_json)

parsed_json_data <- parsed_json
rm(parsed_json_data)

parsed_json_data[[2]]$data$children[[1]]$data$replies$data$children

get_reply_data <-
  function(parsed_json_data) {
    x <- unthread(parsed_json_data)
    if(parsed_json_data[[2]]$data$children[[1]]$kind == 't1') {
      output <- tibble::tibble(
        type = level[[1]][[1]]$kind,
        response_id = level[[1]][[1]]$data$id,
        response_author = level[[1]][[1]]$data$author,
        response_datetime = level[[1]][[1]]$data$created_utc,
        response_text = level[[1]][[1]]$data$body,
        response_score = level[[1]][[1]]$data$score,
        response_url = level[[1]][[1]]$data$permalink,
        parent_id = level[[1]][[1]]$data$parent_id
      )
    }
    output
  }










##################################################################

subreddit_post <-
  httr::GET(
    url = paste0("https://www.reddit.com/r/", subreddit,
                 "/comments/",
                 post_id_x,
                 "/.json?limit=",
                 max_responses,
                 "&after=",
                 starting_point),
    httr::add_headers("User-Agent" = Sys.getenv('reddit_user_agent')),
    httr::authenticate(Sys.getenv('reddit_client_id'),
                       Sys.getenv('reddit_client_token')
    )
  )


#####

subreddit_post_parsed[[1]]$kind
length(subreddit_post_parsed[[1]]$data$children)  # length = 1

post_metadata <-
  subreddit_post_parsed[[1]]$data$children[[1]]

subreddit_post_parsed[[1]]$data$children[[1]]$kind  # kind = t3

post_df <-
  tibble::tibble(type = post_metadata$kind,
                 post_id = post_metadata$data$id,
                 post_author = post_metadata$data$author,
                 post_datetime = post_metadata$data$created_utc,
                 post_title = post_metadata$data$title,
                 post_text = post_metadata$data$selftext,
                 post_score = post_metadata$data$score,
                 post_upvote_ratio = post_metadata$data$upvote_ratio,
                 post_n_comments = post_metadata$data$num_comments,
                 post_url = post_metadata$data$permalink
)

#####

subreddit_post_parsed[[2]]$kind
length(subreddit_post_parsed[[2]]$data$children)   # length = 25

map_chr(subreddit_post_parsed[[2]]$data$children, function(x) x$kind)

subreddit_post_parsed[[2]]$data$children[[1]]$kind  # kind = t1

level <- list()
level[[1]] <-
  subreddit_post_parsed[[2]]$data$children

length(level[[1]])  # in this case, n=25

level[[1]][[1]]$kind

response_df <-
  tibble::tibble(
    type = level[[1]][[1]]$kind,
    response_id = level[[1]][[1]]$data$id,
    response_author = level[[1]][[1]]$data$author,
    response_datetime = level[[1]][[1]]$data$created_utc,
    response_text = level[[1]][[1]]$data$body,
    response_score = level[[1]][[1]]$data$score,
    response_url = level[[1]][[1]]$data$permalink,
    parent_id = level[[1]][[1]]$data$parent_id
)
response_df

## t1 is a response; t3 is a post
# need to split parent_id out into "type" (either post 't1' or response 't3') and "id"


#####

level[[2]] <-
  level[[1]][[1]]$data$replies$data$children
length(level[[2]])  # in this case, n=1

level[[2]][[1]]$kind

response_df <-
  tibble::add_row(
    response_df,
    type = level[[2]][[1]]$kind,
    response_id = level[[2]][[1]]$data$id,
    response_author = level[[2]][[1]]$data$author,
    response_datetime = level[[2]][[1]]$data$created_utc,
    response_text = level[[2]][[1]]$data$body,
    response_score = level[[2]][[1]]$data$score,
    response_url = level[[2]][[1]]$data$permalink,
    parent_id = level[[2]][[1]]$data$parent_id
  )
response_df

#####

level[[3]] <-
  level[[2]][[1]]$data$replies$data$children
length(level[[3]])  ## in this case, n=1

level[[3]][[1]]$kind != more  ## if kind == "more", this may be as deep as can go
                      ## need to check to see if can scrape response IDs

level[[3]][[1]]$data$children
length(level[[3]][[1]]$data$children)  ## n = 3 (more responses, but no data)

#####

level[[1]][[2]]$kind

response_df <-
  tibble::add_row(
    response_df,
    type = level[[1]][[2]]$kind,
    response_id = level[[1]][[2]]$data$id,
    response_author = level[[1]][[2]]$data$author,
    response_datetime = level[[1]][[2]]$data$created_utc,
    response_text = level[[1]][[2]]$data$body,
    response_score = level[[1]][[2]]$data$score,
    response_url = level[[1]][[2]]$data$permalink,
    parent_id = level[[1]][[2]]$data$parent_id
  )
response_df

#####

level[[2]] <-
  level[[1]][[2]]$data$replies$data$children
length(level[[2]])  # in this case, n=3

level[[2]][[1]]$kind

response_df <-
  tibble::add_row(
    response_df,
    type = level[[2]][[1]]$kind,
    response_id = level[[2]][[1]]$data$id,
    response_author = level[[2]][[1]]$data$author,
    response_datetime = level[[2]][[1]]$data$created_utc,
    response_text = level[[2]][[1]]$data$body,
    response_score = level[[2]][[1]]$data$score,
    response_url = level[[2]][[1]]$data$permalink,
    parent_id = level[[2]][[1]]$data$parent_id
  )
response_df

#####

## check if $data$replies == ""
level[[2]][[1]]$data$replies == ""  ## n = 0

level[[2]][[2]]$data$replies == ""

level[[3]] <-
  level[[2]][[2]]$data$replies$data$children
length(level[[3]])  ## in this case, n=1

level[[3]][[1]]$kind  ## if kind == "more", this may be as deep as can go
## need to check to see if can scrape response IDs

response_df <-
  tibble::add_row(
    response_df,
    type = level[[3]][[1]]$kind,
    response_id = level[[3]][[1]]$data$id,
    response_author = level[[3]][[1]]$data$author,
    response_datetime = level[[3]][[1]]$data$created_utc,
    response_text = level[[3]][[1]]$data$body,
    response_score = level[[3]][[1]]$data$score,
    response_url = level[[3]][[1]]$data$permalink,
    parent_id = level[[3]][[1]]$data$parent_id
  )
response_df

#####

level[[3]][[1]]$data$replies == ""

level[[4]] <-
  level[[3]][[1]]$data$replies$data$children
length(level[[4]])  ## in this case, n=1

level[[4]][[1]]$kind  ## if kind == "more", this may be as deep as can go
## need to check to see if can scrape response IDs

response_df <-
  tibble::add_row(
    response_df,
    type = level[[4]][[1]]$kind,
    response_id = level[[4]][[1]]$data$id,
    response_author = level[[4]][[1]]$data$author,
    response_datetime = level[[4]][[1]]$data$created_utc,
    response_text = level[[4]][[1]]$data$body,
    response_score = level[[4]][[1]]$data$score,
    response_url = level[[4]][[1]]$data$permalink,
    parent_id = level[[4]][[1]]$data$parent_id
  )
response_df

#####

level[[4]][[1]]$data$replies == ""

level[[5]] <-
  level[[4]][[1]]$data$replies$data$children
length(level[[5]])  ## in this case, n=1

level[[5]][[1]]$kind != "more"  ## if kind == "more", this may be as deep as can go
## need to check to see if can scrape response IDs

level[[5]][[1]]$data$children
length(level[[5]][[1]]$data$children)  ## n = 1 (more responses, but no data)





#####

res <- purrr::map(res, function(x) x$data)
res <- purrr::map(res, function(x) purrr::map(x,function(y) ifelse(is.null(y), NA, y)))
res <- purrr::map(res, tibble::as.tibble)
res <- purrr::reduce(res, dplyr::full_join)

#purrr::transpose

##################################################################



res <-
  httr::GET(
    url = paste0("https://www.reddit.com/r/", subreddit,
                 "/new.json?limit=100&after=",
                 starting_point),
    httr::add_headers("User-Agent" = Sys.getenv('reddit_user_agent')),
    httr::authenticate(Sys.getenv('reddit_client_id'),
                       Sys.getenv('reddit_client_token')
    )
  )
res$status_code
res <-httr::content(res, as = "parsed")
res <- res$data$children
res <- purrr::map(res, function(x) x$data)
res <- purrr::map(res, function(x) purrr::map(x,function(y) ifelse(is.null(y), NA, y)))
res <- purrr::map(res, tibble::as.tibble)
res <- purrr::reduce(res, dplyr::full_join)

#purrr::transpose




##################################################################

subreddit_responses <-
  httr::GET(
    url = paste0("https://www.reddit.com/r/", subreddit,
                 "/comments/", post_id,
                 "/.json?limit=100&after=",
                 starting_point),
    httr::add_headers("User-Agent" = Sys.getenv('reddit_user_agent')),
    httr::authenticate(Sys.getenv('reddit_client_id'),
                       Sys.getenv('reddit_client_token')
    )
  )

subreddit_response_page <- jsonlite::fromJSON(httr::content(subreddit_responses, as = "text"))

#subreddit_response_page <- jsonlite::fromJSON(url)

subreddit_data <- starting_data

if (!exists("subreddit_data")) {
  subreddit_data <- tibble::as_tibble(subreddit_response_page$data$children)
} else {
  subreddit_data <-
    dplyr::bind_rows(subreddit_data,
                     tibble::as_tibble(subreddit_response_page$data$children$data)
    )
}

message(paste(nrow(subreddit_data), "posts found so far..."))

if (nrow(subreddit_data) >= max_responses |
    starting_point == paste0("t3_", tail(subreddit_data$id, 1))
) {

  message(paste("Done retrieving. Returned", nrow(subreddit_data), "posts in total."))
  return(subreddit_data)

} else {

  message("Going to sleep for a minute...")
  Sys.sleep(60)  # Wait 60 seconds before running again to keep below API rate limit
  message("Waking up...")

  get_posts(subreddit = subreddit_data$subreddit[1],
            starting_data = subreddit_data,
            starting_point = paste0("t3_", tail(subreddit_data$id, 1)),
            max_responses = max_responses
  )
}










responses1$status_code
responses1 <- httr::content(responses1, as = "parsed")
responses1 <- responses1[[2]]$data$children

tmp <- purrr::map(responses1, function(x) x$data)
tmp[[3]]$replies

# res1[[2]]$data$children[[3]]$data$replies$data$children[[1]]$data$replies$data$children[[1]]$data$replies$data$children[[1]]$data$replies$data$children[[1]]$data$replies$data$children[[1]]

res <- purrr::map(res, function(x) purrr::map(x,function(y) ifelse(is.null(y), NA, y)))
res <- purrr::map(res, tibble::as.tibble)
res <- purrr::reduce(res, dplyr::full_join)






res2 <-
  httr::GET(
    url = paste0("https://www.reddit.com/api/morechildren"),
    httr::add_headers("User-Agent" = Sys.getenv('reddit_user_agent'),
                      api_type = 'json',
                      children = res$id,
                      #limit = 100,
                      #after = starting_point,
                      limit_children = FALSE
    ),
    httr::authenticate(Sys.getenv('reddit_client_id'),
                       Sys.getenv('reddit_client_token')
    )
  )
res2 <- jsonlite::fromJSON(httr::content(res2, as = "text"))
res2 <- tibble::as_tibble(res2$data$children$data)

full_name <- "t5_2qqcs"  # r/Teachers  or res$name[1]
id36 <- "2qqcs"

url = "https://www.reddit.com/r/Teachers/comments/grrbgc"
