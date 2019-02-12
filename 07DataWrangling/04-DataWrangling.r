### -----------------------------
### Data Wrangling
### Hui Lin @Netlify
### http://scientistcafe.com
### -----------------------------

## Data Wrangling
## -----------------------------
# Should have loaded it 
# library(readr)

sim.dat <- read_csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/SegData.csv ")
head(sim.dat)

# dplyr - display

tbl_df(sim.dat)
glimpse(sim.dat)

# dplyr - subset

filter(sim.dat, income >300000) %>%
  tbl_df()

# Pipe operator

# piping with .
"Hello World" %>% substring(7, 11) %>% grepl("Wo", .)

# without pipe
ave_exp <- filter( 
  summarise(
    group_by( 
      filter(
        sim.dat, 
        !is.na(income)
      ), 
      segment
    ), 
    ave_online_exp = mean(online_exp), 
    n = n()
  ), 
  n > 200
) 

# with pipe
avg_exp <- sim.dat %>% 
  filter(!is.na(income)) %>% 
  group_by(segment) %>% 
  summarise( 
    ave_online_exp = mean(online_exp), 
    n = n() ) %>% 
  filter(n > 200)

# Subset - select rows
dplyr::distinct(sim.dat)

# random sample rows
dplyr::sample_frac(sim.dat, 0.5, replace = TRUE) 
dplyr::sample_n(sim.dat, 10, replace = TRUE) 

# select rows by position
# It is equivalent to `sim.dat[10:15,]`
dplyr::slice(sim.dat, 10:15) 

# select the order top n entries
dplyr::top_n(sim.dat,2,income)

# Subset - select columns

# select by column name
dplyr::select(sim.dat,income,age,store_exp)

# select columns whose name contains a character string
dplyr::select(sim.dat, contains("_"))

# select columns whose name ends with a character string
# similar there is "starts_with"
dplyr::select(sim.dat, ends_with("e"))

# select columns Q1,Q2,Q3,Q4 and Q5
select(sim.dat, num_range("Q", 1:5)) 

# select columns whose names are in a group of names
dplyr::select(sim.dat, one_of(c("age", "income")))

# select columns between age and online_exp
dplyr::select(sim.dat, age:online_exp)

# select all columns except for age
dplyr::select(sim.dat, -age)

# Summarize

dplyr::summarise(sim.dat, avg_online = mean(online_trans)) 
# apply function anyNA() to each column
# you can also assign a function vector such as: c("anyNA","is.factor")
dplyr::summarise_all(sim.dat, funs_(c("anyNA")))

sim.dat %>% group_by(segment) %>% summarise_all(funs_(c("anyNA")))

# Create new variable

# `mutate()`: compute and append one or more new columns:
dplyr::mutate(sim.dat, total_exp = store_exp + online_exp)

# min_rank=rank(ties.method = "min")
# mutate_each() means apply function to each column
dplyr::mutate_all(sim.dat, funs(min_rank)) 

# `transmute()`: delete the original columns and only keep the new ones
dplyr::transmute(sim.dat, total_exp = store_exp + online_exp) 

## Merge

(x<-data.frame(cbind(ID=c("A","B","C"),x1=c(1,2,3))))
(y<-data.frame(cbind(ID=c("B","C","D"),y1=c(T,T,F))))

# join to the left
# keep all rows in x
left_join(x,y,by="ID")
# get rows matched in both data sets
inner_join(x,y,by="ID")
# get rows in either data set
full_join(x,y,by="ID")
# filter out rows in x that can be matched in y 
# it doesn't bring in any values from y 
semi_join(x,y,by="ID")
# the opposite of  semi_join()
# it gets rows in x that cannot be matched in y
# it doesn't bring in any values from y
anti_join(x,y,by="ID")

## Tidy and Reshape Data 
# Take a baby subset of our exemplary clothes consumers data to illustrate:
(sdat<-sim.dat[1:5,1:6])

# `reshape2` example
# (mdat <- melt(sdat, measure.vars=c("store_exp","online_exp"),
#              variable.name = "Channel",
#              value.name = "Expense"))

# compare the online and in store expense between male and female based on the house ownership
# dcast(mdat, house + gender ~ Channel, sum)

## `tidyr` package

# practice functions we learnt before
sdat<-sim.dat[1:5,]%>%
  dplyr::select(age,gender,store_exp,store_trans)
sdat %>% tbl_df()


msdat<-tidyr::gather(sdat,"variable","value",store_exp,store_trans)
msdat %>% tbl_df()

# pipe version
sdat%>%gather("variable","value",store_exp,store_trans)

# melt version
melt(sdat, measure.vars=c("store_exp","store_trans"),
     variable.name = "variable",
     value.name = "value")

# spread
msdat %>% spread(variable,value)

##  `separate()` and `unite()`

# You can use `sep=` 
# By default, it is "`_`"
sepdat<- msdat %>% 
  separate(variable,c("Source","Type"))
sepdat %>% tbl_df()

sepdat %>% 
  unite("variable",Source,Type,sep="_")

sepdat %>% 
  unite("variable",Source,Type,sep="_")
