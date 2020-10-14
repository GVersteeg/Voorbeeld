
## using the standard iris dataset
library(datasets)
data("iris")
df <- iris

## Variant: FOR-LOOP
for (i in seq_along(df$Sepal.Length)) {
  df$Length.Dif[i] <- df$Sepal.Length[i] - df$Petal.Length[i]
  df$Width.Dif[i] <- df$Sepal.Width[i] - df$Petal.Width[i]
  if (df$Species[i] == "setosa") {
        df$Length.Dif[i] <- 0
        df$Width.Dif[i] <- 0
  }
}
df <- df[which(df$Species != "versicolor"),]
df <- df[,c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
            "Length.Dif", "Width.Dif")]

## Variant: DPLYR
library(dplyr)
df <- df %>% 
  mutate(Length.Dif = Sepal.Length - Petal.Length) %>%
  mutate(Width.Dif = Sepal.Width - Petal.Width) %>%
  mutate(Length.Dif = if_else(Species == "setosa", 0, Length.Dif)) %>%
  mutate(Width.Dif = if_else(Species == "setosa", 0, Width.Dif)) %>%
  filter(Species != "versicolor") %>% 
  select(-Species)
                                
library(dplyr)
df <- mutate(df, Length.Dif = Sepal.Length - Petal.Length)
df <- mutate(df, Width.Dif = Sepal.Width - Petal.Width)
df <- mutate(df, Length.Dif = if_else(Species == "setosa", 0, Length.Dif))
df <- mutate(df, Width.Dif = if_else(Species == "setosa", 0, Width.Dif))
df <- mutate(df, Species != "versicolor")
df <- mutate(df, -Species)

## MUTATE
library(data.table)
data(mtcars)
df <- mtcars
df <- setDT(df, keep.rownames = "car")
g2l <- 3.7854
m2k <- 1.6093
df_1 <- df %>% 
  mutate(kpl = round((mpg * m2k / g2l), digits = 1))

data(mtcars)
df <- mtcars
df <- setDT(df, keep.rownames = "car")
m2k <- 1.6093
m2km <-function(x) round(x / m2k, digits = 2)
df_1 <- df %>% 
        mutate_at(vars(matches("qsec")), m2km)

iris %>% mutate_at(vars(matches("Sepal")), log)

# The _if() variants apply a predicate function (a function that
# returns TRUE or FALSE) to determine the relevant subset of
# columns. Here we divide all the numeric columns by 100:
starwars %>% mutate_if(is.numeric, scale2, na.rm = TRUE)

# mutate_if() is particularly useful for transforming variables from
# one type to another
iris %>% mutate_if(is.factor, as.character)
iris %>% mutate_if(is.double, as.integer)
## GROUP_BY
data(mtcars)
df <- mtcars
dfg <- group_by(df, cyl)
dfu <- ungroup(dfg)

data(mtcars)
df <- mtcars
df2g <- group_by(df, cyl, am)
df1g <- summarize(df2g, n = sum(am))

dfg <- group_by(df, cyl)
dfga <- group_by(dfg, am, add = TRUE)

dfs <- df %>% 
        group_by(cyl) %>% 
        summarise(gem = mean(disp))
dfs

head(df)
arrange(df, cyl, desc(mpg))

dfs <- df %>% 
        group_by(cyl) %>% 
        summarise(n = n())
dfs

dfs <- df %>% 
        count(cyl)
dfs

data("billboard")
df_bb <- billboard
df_bb_long <- df_bb %>%
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  )


# download sample data; adjust destfile filepath as necessary.
if (!file.exists("data/fishdata.csv")) {
   download.file(
     url = 'https://github.com/Myfanwy/ReproducibleExamples/raw/master/encounterhistories/fishdata.csv',
     destfile = "data/fishdata.csv"
   )
}
df_fish$value[df_fish$value == 0] <- NA
colnames(df_fish) <- c("TagID","station", "seen")
df_fish <- df_fish[complete.cases(df_fish),]

df_fish_wide <- df_fish %>% 
  pivot_wider(
    names_from = station,
    values_from = seen,
    values_fill = list(seen = 0)
  )

## MISSING VALUES
data("airquality")
df <- airquality
df[is.na(df)] <- 0

data("airquality")
df <- airquality
df_cc <- df[complete.cases(df),]
df_ncc <- df[!complete.cases(df),]

data("airquality")
df <- airquality
df_cc <- df[complete.cases(df),]
df_cc2 <- df[complete.cases(df[,2:4]),]

## HTML-FILES

## Stap 1. ophalen web-page ---------------------------------- #
library(readr)
library(rvest)
base_url <- "https://nl.wikipedia.org/wiki/"
page_url <- "Lijst_van_grootste_metropolen_van_Noord-Amerika"
url <- paste0(base_url, page_url)
wp <- read_html(url)

## Stap 2. Parse XML-object ---------------------------------- #
xpad <- '//*[@id="mw-content-text"]/div/table[2]'
dt <- html_nodes(wp, xpath = xpad)
dt1 <- html_nodes(wp, ".wikitable")

## Stap 3. Zet om naar data frame ---------------------------- #
df <- html_table(dt, fill = TRUE)[[1]]
df1 <- html_table(dt1, fill = TRUE)[[1]]

