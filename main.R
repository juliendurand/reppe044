library(reshape2)
library(ggplot2)

mydata <-  read.csv("data/quotes.csv")
levels(mydata$CONVERSION) <-  c(levels(mydata$CONVERSION), "ANI")
mydata$CONVERSION[mydata$CONVERSION==""] <- "ANI"
mydata$DATE_EMISS <- as.Date(mydata$DATE_EMISS, "%d/%m/%Y")
mydata <- mydata[mydata$DATE_EMISS >= as.Date("01/03/2015", "%d/%m/%Y") &
                 mydata$DATE_EMISS <= as.Date("01/01/2016", "%d/%m/%Y"), ]

st <- function(df, dept=NA, region=NA){
  if(!is.na(dept)){
    df <- df[df$DEPARTEMENT_PTF == dept, ]
  }
  if(!is.na(region)){
    df <- df[df$REGION == region, ]
  }
  df <- dcast(df, MOIS+ANNEE~CONVERSION, length, value.var="USERID")
  df$CONVERTED <- df$AND + df$ANI
  df$TOTAL <- df$AND + df$ANI + df$DNT
  df$CONVERSION <- df$CONVERTED / df$TOTAL
  df$DATE <- as.Date(ISOdate(df$ANNEE, df$MOIS, 1))
  return(df)
}

data <- st(mydata, dept="01")
ggplot(data=data, aes(DATE, CONVERSION)) +
  geom_line() +
  xlab("Monthly View") +
  scale_x_date(date_labels = "%b-%Y") +
  ylab("Conversion Rate") +
  ylim(0, 1) +
  theme_bw()

