library("fivethirtyeight")
library("tidyverse")

bechdel_2010s <- bechdel %>%
  as_tibble() %>%
  filter(clean_test == "ok") %>%
  filter(year > 2010) %>%
  mutate(profitability = intgross_2013 / budget_2013) %>%
  arrange(desc(profitability)) %>%
  select(imdb, title, year, profitability)


download.file(url = "https://datasets.imdbws.com/title.ratings.tsv.gz",
              destfile = "data-raw/imdb-ratings.tsv.gz")

R.utils::gunzip("data-raw/imdb-ratings.tsv.gz")

imdb_ratings <- read_tsv("data-raw/imdb-ratings.tsv", na = "\\N", quote = '')

bechdel_ratings_2010s <- bechdel_2010s %>%
  left_join(imdb_ratings,
            by = c("imdb" = "tconst"))

bechdel_ratings_2010s <- bechdel_ratings_2010s %>%
  select(title, year, profitability, averageRating) %>%
  rename(imdb_rating = averageRating)

bechdel_ratings_2010s %>%
  write_csv("data/bechdel-ratings-2010s.csv")
