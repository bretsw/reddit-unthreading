##################################################################

library(tidyverse)
library(anytime)
library(lubridate)

#devtools::install_github("ropenscilabs/roomba")  # <------ currently returns an error and cannot install
#devtools::install_github("jrosen48/roomba")  # only have to install once
library(roomba)


##################################################################
##  Function definitions
##################################################################

##########
get_post_json <-
  function(subreddit, post_id) {
    full_json <-
      httr::GET(
        url = paste0("https://www.reddit.com/r/", subreddit,
                     "/comments/",
                     post_id,
                     "/.json"),
        httr::add_headers("User-Agent" = Sys.getenv('reddit_user_agent')),
        httr::authenticate(Sys.getenv('reddit_client_id'),
                           Sys.getenv('reddit_client_token')
        )
      )

    httr::content(full_json, as = "parsed")
  }


##########
get_responses_roomba <-
  function(json_data)  {
    responses <-
      roomba::roomba(json_data,
                     cols = c("subreddit", "id", "author", "created_utc", "body",
                              "score", "permalink", "parent_id"),
                     keep = any)
    responses <-
      mutate(responses,
             post_id = json_data[[1]]$data$children[[1]]$data$id)

    return(responses)
  }

##########
clean_responses <-
  function(response_tibble) {
    cleaned_tibble <-
      response_tibble %>%
      filter(!is.na(subreddit),
             body != "[deleted]") %>%
      separate(parent_id, into = c("parent_type", "parent_id"), sep = "_") %>%
      mutate(parent_type =
               ifelse(parent_type == 't3',
                      'post',
                      'response')
      ) %>%
      mutate(response_date_time =
               created_utc %>%
               as.numeric() %>%
               anytime(asUTC = TRUE) %>%
               as_datetime %>%
               ymd_hms() %>%
               with_tz(tzone = "US/Eastern")
      ) %>%
      select(subreddit,
             post_id,
             response_id = id,
             response_author = author,
             response_date_time,
             response_text = body,
             parent_type,
             parent_id,
             response_score = score,
             response_link = permalink)

    return(cleaned_tibble)
  }

##########
get_responses_for_one_post <-
  function(subreddit, post_id) {
    post_json_data <- get_post_json(subreddit, post_id)
    responses <- get_responses_roomba(post_json_data)
    responses <- clean_responses(responses)
    return(responses)
  }

##########
get_responses <-
  function(posts_tibble) {
    posts_tibble <- filter(posts_tibble, num_comments > 0)
    subreddit <- posts_tibble$subreddit[1]
    new_tibble <- NULL
    new_tibble <- map_df(posts_tibble$post_id,
                         ~ bind_rows(new_tibble,
                                     get_responses_for_one_post(subreddit,
                                                                .x)
                         )
    )
    return(new_tibble)
  }


##########
merge_posts_response <-
  function(posts_tibble, responses_tibble) {
    new_tibble <-
      posts_tibble  %>%
      select(subreddit,
             post_id,
             post_author = author,
             post_date_time,
             post_title = title,
             post_text = selftext,
             num_responses = num_comments,
             post_score = score,
             post_link = permalink) %>%
      left_join(responses_tibble, by = c('subreddit', 'post_id'))

    return(new_tibble)
  }


##################################################################
## Examples
##################################################################

posts_tibble <-
  read_csv("r-Professors-posts-may2020-jun2020.csv") %>%
  mutate(post_date_time = post_date_time %>%
           as.numeric() %>%
           anytime(asUTC = TRUE) %>%
           as_datetime %>%
           ymd_hms() %>%
           with_tz(tzone = "US/Eastern")
  )

posts_tibble$num_comments %>%
  table() %>%
  as_tibble() %>%
  rename(num_comments = ".",
         count = n) %>%
  mutate(num_comments = as.numeric(num_comments)) %>%
  arrange(desc(count)) %>%
  head(20)

# max number of comments (i.e., responses) is 330, in row 688
# https://www.reddit.com/r/Professors/comments/gkrh0g/
# https://www.reddit.com/r/Professors/comments/gkrh0g/.json?limit=100&after=


example1 <-
  get_post_json("Professors", posts_tibble$post_id[688]) %>%
  get_responses_roomba() %>%
  clean_responses()
dim(example1)
table(example1$parent_type)

example2 <-
  get_responses_for_one_post("Professors", posts_tibble$post_id[688])
dim(example2)
table(example2$parent_type)




responses_tibble <- get_responses(posts_tibble) ## This will take a while to run!!!
#beepr::beep(8)
dim(posts_tibble); dim(responses_tibble)

merged_tibble <- merge_posts_response(posts_tibble, responses_tibble)
dim(merged_tibble)

#write_csv(merged_tibble, "r-Professors-posts-responses-may2020-jun2020.csv")

