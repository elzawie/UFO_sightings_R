#Checking current working directory
getwd()


#Installing packages: ggplot2, plyr, scales
install.packages('ggplot2')
install.packages('plyr')
install.packages('scales')

#Loading packages
library(ggplot2)
library(plyr)
library(scales)

#The goal of the below data analysis is to show the number of UFO sightings in the following months, broken down into states


#Reading datafile "ufo_report.tsv" into a data frame

ufo <- read.delim("ufo_report.tsv", 
                  sep = "\t", stringsAsFactors = FALSE, 
                  header = FALSE, na.strings = "")


#Checking additional information about our dataframe

summary(ufo)
head(ufo) #6 first observations
tail(ufo) #6 last observations


#Creating missing variable(column) names
names(ufo)<-c("DateOccurred", "DateReported", "Location", 
              "Shape", "Duration", "Description")


#Formatting date
head(ufo[which(nchar(ufo$DateOccurred) != 8 | nchar(ufo$DateReported) !=8), 1])

#All incorrect rows
bad.rows<-ufo[which(nchar(ufo$DateOccurred) != 8 | 
                      nchar(ufo$DateReported) !=8 | 
                      is.na(ufo$DateOccurred) | 
                      is.na(ufo$DateReported)), 1:6]

#Checking the indices of incorrect rows
good.rows<-ifelse(nchar(ufo$DateOccurred) != 8 | 
                    nchar(ufo$DateReported) != 8 | 
                    is.na(ufo$DateOccurred) | 
                    is.na(ufo$DateReported), FALSE, TRUE)


summary(good.rows)


#Removing incorrect rows
ufo<-ufo[good.rows, ]

#Converting date format into a correct one
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format = "%Y%m%d")
ufo$DateReported <- as.Date(ufo$DateReported, format = "%Y%m%d")


#Tidying and structuring Location variable
#We want to create new columns : city and state
#Checking the format of Location variable
head(ufo$Location)


#We are using strsplit function
head(strsplit(ufo$Location, ","))
strsplit(ufo$Location, ",")[[1]]
strsplit(ufo$Location, ",")[[1]][2]


#Using regular expressions to remove spacings
gsub("^ ", "", strsplit(ufo$Location, ",")[[1]])
gsub("^ ", "", strsplit(ufo$Location, ",")[[1]])[2]


#Creating get.location function
#Creating list of cities and states in USA
get.location <- function(l)
{
  split.location <- tryCatch(strsplit(l, ",")[[1]],
                             error = function(e) return(c(NA, NA)))
  clean.location <- gsub("^ ", "", split.location)
  if(length(clean.location)>2){
    return(c(NA, NA))
  }
  else{
    return(clean.location)
  }
}


#Applying get.location function on out Location column
city.state <- lapply(ufo$Location, get.location)


#Converting cities and states list into a matrix, this will enable us to insert it into our dataframe
location.matrix <- do.call(rbind, city.state)


#Adding new columns into ufo dataframe
ufo<-transform(ufo, USCity=location.matrix[,1], 
               USState=location.matrix[,2], 
               stringsAsFactors = FALSE)


#Creating new dataframe "ufo_new" which we will use for further analysis
ufo_new <- ufo

summary(ufo_new)

#Changing date format
ufo_new$DateOccurred <- as.Date(ufo_new$DateOccurred, format = "%Y-%m-%d")
ufo_new$DateReported <- as.Date(ufo_new$DateReported, format = "%Y-%m-%d")

summary(ufo_new)

#Creating new column "USState" containing state abbreviations and denoting all states from outside of USA as 'na' values.
ufo_new$USState <- state.abb[match(ufo_new$USState, state.abb)]

#Removing observations from outside of USA
ufo.us <- subset(ufo_new, !is.na(USState))

#Starting the analysis
head(ufo.us)
summary(ufo.us)

#Plotting histogram
quick.hist <- ggplot(ufo.us, aes(x = DateOccurred)) + geom_histogram() + 
  scale_x_date(date_breaks = "10 years", labels = date_format("%Y")) + 
  labs(x = "Observation date", y = "Counts")

show(quick.hist)

#Saving histogram into PDF file
ggsave(plot = quick.hist, filename = file.path("quick_hist.pdf"), height = 6, width = 8)

#Narrowing down the observation period
ufo.us1 <- subset(ufo.us, DateOccurred >= as.Date("1900-01-01"))

ufo.us2 <- subset(ufo.us, DateOccurred >= as.Date("1950-01-01"))

ufo.us3 <- subset(ufo.us, DateOccurred >= as.Date("1990-01-01"))


#Creating new, more sophisticated histogram
new.hist <- ggplot(ufo.us3, aes(x = DateOccurred)) +
  geom_histogram(aes(fill='white', color='red')) +
  scale_fill_manual(values=c('white'='white'), guide="none") +
  scale_color_manual(values=c('red'='red'), guide="none") +
  scale_x_date(date_breaks = "2 years", labels = date_format("%Y")) +
  labs(x = "Observation date", y = "Counts")

show(new.hist)

#Saving plot into a PDF file
ggsave(plot = new.hist, filename = file.path("quick_hist.pdf"), height = 6, width = 8)


#As a next step we are going to check the seasonal variances of UFO sightings in USA.
#We are starting from aggregating data by years and months
#Creatign new column containing following months in YYYY-MM format
ufo.us3$YearMonth<-strftime(ufo.us3$DateOccurred, format = "%Y-%m")


#Counting state-year-month combinations using ddply function from plyr package
observations.counts <- ddply(ufo.us3, .(USState, YearMonth), nrow)
head(observations.counts)


#We're missing data in February and April for Alaska so we need to impute them.
#Let's start by creating years and months sequences

date.range <- seq.Date(from = as.Date(min(ufo.us3$DateOccurred)),
                       to = as.Date(max(ufo.us3$DateOccurred)), by = "month")
head(date.range)
summary(date.range)
date.strings <- strftime(date.range, "%Y-%m")
head(date.strings)

states.dates <- lapply(state.abb, function(s) cbind(s, date.strings))
summary(states.dates)

states.dates1 <- states.dates

#Creating new dafa frame 
states.dates <- data.frame(do.call(rbind, states.dates),
                           stringsAsFactors = FALSE)


#Now we're going to merge 2 data frames : states.dates and observations.counts using merge function
#Missing values will be fulfilled with "NA" values 

all.observations <- merge(states.dates, observations.counts, by.x = c("s", "date.strings"),
                          by.y = c("USState", "YearMonth"), all = TRUE)

#Setting column names for the newly created data frame
names(all.observations) <- c("State", "YearMonth", "Observations")


#Replacing NA values with zeros
all.observations$Observations[is.na(all.observations$Observations)] <- 0

#Changing format of "YearMonth" column to date
all.observations$YearMonth <- as.Date(rep(date.range, length(state.abb)))

#Creating factors from the "State" column
all.observations$State <- as.factor(all.observations$State)

#Let's check the seasonality for 2 states: Alaska and California using plots
ufo.ak <- subset(all.observations, State == "AK")
ufo.ca <- subset(all.observations, State == "CA")


plot.ufo.ak <- ggplot(ufo.ak, aes(x = YearMonth, y = Observations)) +
  geom_line(aes(color = "darkblue")) +
  theme_bw() + 
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  scale_x_date(date_breaks = "1 years", labels = date_format('%Y')) +
  xlab("Years") +
  ylab("Counts") +
  ggtitle("Number of UFO sightings in the following months in Alaska (1990-2010)")

show(plot.ufo.ak)

plot.ufo.ca <- ggplot(ufo.ca, aes(x = YearMonth, y = Observations)) +
  geom_line(aes(color = "darkblue")) +
  theme_bw() + 
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  scale_x_date(date_breaks = "1 years", labels = date_format('%Y')) +
  xlab("Years") +
  ylab("Counts") +
  ggtitle("Number of UFO sightings in the following months in California (1990-2010)")

show(plot.ufo.ca)

#Saving seasonality plots into PDF files
ggsave(plot = plot.ufo.ak, filename = file.path("ufo_ak_report.pdf"), width = 14, height = 8.5)
ggsave(plot = plot.ufo.ca, filename = file.path("ufo_ca_report.pdf"), width = 14, height = 8.5)

#Let's try to plot the data for all states on one plot
state.plot <- ggplot(all.observations, aes(x = YearMonth, y = Observations)) +
  geom_line(aes(color = State))

show(state.plot)

#Since our plot is illegible, let's try to refine it by drawing plots for all states as separate panels
#facet_wrap() function will create separate plots for each state and locate them on 10x5 grid
state.plot <- ggplot(all.observations, aes(x = YearMonth, y = Observations)) +
  geom_line(aes(color = "darkblue")) +
  facet_wrap(~State, nrow = 10, ncol = 5) + 
  theme_bw() + 
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  scale_x_date(date_breaks = "4 years", labels = date_format('%Y')) +
  xlab("Years") +
  ylab("Counts") +
  ggtitle("Number of UFO sightings in the following months divided by states (1990-2010)")

show(state.plot)

#Saving plot into a PDF file
ggsave(plot = state.plot, filename = file.path("ufo_observations.pdf"), width = 14, height = 8.5)


#The task was based on the below book
#Drew Conway, John Myles White, "Uczenie maszynowe dla programistów", Helion 2015
