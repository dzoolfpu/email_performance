setwd("D:/")

#'Required libraries
#'---
#'
#'
#'
devtools::install_github("ropensci/plotly")
library(RPostgreSQL)
library(ggplot2)
library(plotly)
library(reshape2)
library(plyr)
library(lubridate)
library(Hmisc)
library(scales)
library(dplyr)
library(sqldf)
library(RColorBrewer)
library(randomForest)
library(d3heatmap)
Sys.setenv("plotly_username"="")
Sys.setenv("plotly_api_key"="")


#' Read data from DB
-----------------------
#' 
#' 
#' 
con = dbConnect(drv = PostgreSQL(), user = "postgres",
                  password = "pass",host="host",port=port,dbname="dbname")
  
email_activity = dbGetQuery( con, 'select * from email_activity' )
email_campaigns = dbGetQuery( con, 'select * from email_campaigns' )
email_lists = dbGetQuery( con, 'select * from email_lists' )
  
  
  
  
#' Data preparation
#' ---
#' 
#' 
#' 
email_lists[!complete.cases(email_lists),]           # list rows of data that have missing values 
email_listsnew = na.omit(email_lists)                # create new dataset without missing data

email_campaigns$hour  = as.numeric(format(as.POSIXct(email_campaigns$create_time), "%H"))   # create a column with the exact hour

  
email_campaigns$day = weekdays(as.Date(email_campaigns[["create_time"]]))       # create a column with the exact day of the email sent
#email_campaigns$day  = factor(email_campaigns$day, levels= c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday") )
  
  
email_campaigns$wday = wday(email_campaigns$create_time) 
email_campaigns$wday  = as.numeric(email_campaigns$wday)
  
email_campaigns$working_day= ifelse(email_campaigns$wday %in% c(1,7), "Weekend", "Working day") # split days to working/non working days for future use of the analysis
  
email_campaigns$month = month(as.Date(email_campaigns[["create_time"]], label = TRUE, abbr = TRUE))  # create a column with the month of the email sent
email_campaigns$month = as.numeric(email_campaigns$month)
  
email_campaigns$season = quarter(as.Date(email_campaigns[["create_time"]]))   # For future use, analysis over seasons
email_campaigns$season[email_campaigns$month %in% c(9,10,11)] = "Aut"
email_campaigns$season[email_campaigns$month %in% c(12,1,2)] = "Win"
email_campaigns$season[email_campaigns$month %in% c(3,4,5)]="Spr"
email_campaigns$season[email_campaigns$month %in% c(6,7,8)]  ="Sum"
  
email_campaigns$year = as.numeric(format(as.POSIXct(email_campaigns$create_time), "%Y"))
  
email_campaigns = upData(email_campaigns, rename=c("recipients_list_id" = 'list_id'))
email_campaigns = upData(email_campaigns,rename = c("id"= "campaign_id"))  
email_activity = upData(email_activity,rename = c('"timestamp"'= "date"))  

  
#plot(as.matrix(table(email_campaigns$day)), type="b", col="darkgreen", pch=20, cex=1.0, xlab=list("Day",cex=0.8,font=2),ylab = list("times of sequence",cex=0.8,font=2),main=list("Day Sequence of email sent per day type ",cex=1.0,font=2))
  
  
  
#' join tables
#' ----
#'
#'
#'
#'
  
detach("package:RPostgreSQL", unload=TRUE)
  
ROR = sqldf("select date,action,campaign_id,email_address,emails_sent,send_time,working_day,report_summary_open_rate,report_summary_click_rate,day,month,hour
              from email_activity join email_campaigns 
              using(campaign_id)")
  

SbscG = sqldf("select campaign_id,date,status,action
                from email_activity join email_lists 
                using(email_address)")


Bounce = sqldf("select campaign_id,type, emails_sent
                 from email_activity join email_campaigns 
                 using(campaign_id)")


sp1 =  sqldf("select campaign_id,action,email_activity.list_id, email_client, email_type,language,location_country_code
               from email_activity join email_lists 
               using(email_address)")         


sp2 = sqldf("select campaign_id,action,report_summary_open_rate,report_summary_click_rate, working_day,day
              from email_activity join email_campaigns 
              using(campaign_id)")  
  
  




#' Some basic plots
#' --
#' 
#' 
MyBarlot = function(indf , stili1, stili2, intitle, inxtitle, inytitle) {
  results = ggplot( data=indf, aes_string( x=stili1, y=stili2 )) +
    geom_bar(stat = "identity",position = "identity")+
    theme_light() +
    scale_color_discrete("")+
    labs(title=intitle, x=inxtitle, y=inytitle)+
    return(results)
}

sxa= MyBarlot(sp2,"day","report_summary_open_rate",intitle = "Open Rate per Day",
              inxtitle = "Day of the week",inytitle = "Open rate value")
sxb= MyBarlot(sp2,"day","report_summary_click_rate",intitle = "Click Rate per Day",
              inxtitle = "Day of the week",inytitle = "Open rate value")
sxc= MyBarlot(sp2,"working_day","report_summary_open_rate",intitle = "Open Rate per Day",
              inxtitle = "Day of the week",inytitle = "Open rate value")
sxd= MyBarlot(sp2,"workin_day","report_summary_click_rate",intitle = "Click Rate per Day",
              inxtitle = "Day of the week",inytitle = "Open rate value")
plot(sxa)
plot(sxb)
plot(sxc)
plot(sxd)




#' Relevant Open Rate & clickthrough Rate for all email campaigns (daily)
#  ------
#'
#'
#'



agr = sqldf("select report_summary_open_rate,report_summary_click_rate,hour
              from email_activity join email_campaigns 
            using(campaign_id)")


mdf <- melt(agr, id.vars="hour", value.name="value", variable.name="variable")
h= ggplot(mdf, aes(hour , value,colour = variable )) + 
   geom_line()+
   geom_smooth(method = "lm")+
   theme_light() +
   xlab("Time of the day") +
   ylab("click/open_rate value") +
   scale_color_discrete("") +
   ggtitle("Time Relevance on Open Rate & Clickthrough Rate for all email campaigns") +
   theme(plot.title=element_text(size=rel(1.2)))

p =h+theme(legend.position="bottom")
gg=ggplotly(p)
print(gg)
htmlwidgets::saveWidget(as.widget(gg), ":/ /html/index1.html")





#' Relevant Open Rate & clickthrough Rate for each email_campaign (daily)
#  ------
#'
#'
#'

agr1 = ddply(ROR, .(campaign_id,hour,month,report_summary_click_rate,report_summary_open_rate),
             plyr::summarize,
             emails_per_campaign = sum(emails_sent)) 
 

for( i in 1:length(agr1$campaign_id))
{ campid = agr1$campaign_id[i]


h2 =  ggplot(agr1, aes(hour,report_summary_click_rate))+
  geom_smooth(method = "loess") +
  geom_smooth(aes(hour,report_summary_open_rate),color="red")+
  xlab("Time of the day") +
  ylab("Click/Open_rate value") +
  scale_color_manual(labels=abbreviate) +
  ggtitle(paste0("Time Relevance on Open Rate & Clickthrough Rate for campaign_id ",campid) ) +
  theme(plot.title=element_text(size=rel (1.2)))

 p2 =h2+theme(legend.position="bottom")
 gg2 = ggplotly(p2)
 print(gg2)
 htmlwidgets::saveWidget(as.widget(gg2), "://html/index2.html")
}





#' Relevant Open Rate & clickthrough Rate (monthly)
#' -----
#' 
#' 
#' 
for( i in 1:length(agr1$campaign_id))
{ campid = agr1$campaign_id[i]


h3 =  ggplot(agr1, aes(month,report_summary_click_rate))+
  geom_smooth(method = "loess") +
  geom_smooth(aes(month,report_summary_open_rate),color="red")+
  #theme_light() +
  xlab("Month of the year") +
  ylab("Click/Open_rate value") +
  scale_color_manual(labels=abbreviate) +
  ggtitle(paste0("Time Relevance on Open Rate & Clickthrough Rate for campaign_id ",campid) ) +
  theme(plot.title=element_text(size=rel (1.2)))
h33 = h3 + scale_x_continuous(breaks = c(1:12),
                              labels = c("Jan","Feb","Mar","Apr","May","jun","Jul","Aug","Sep","Oct","Nov","Dec"))
p3 = h33+theme(legend.position="bottom")
gg3 = ggplotly(p3)
print(gg3)
htmlwidgets::saveWidget(as.widget(gg2), "://html/index3.html")
}




#' Timeseries
#' ---
#' 
#' 
#' 

ROR$date = strptime(ROR$date, "%Y-%m-%d %H:%M:%OS")
ROR$send_time = strptime(ROR$send_time, "%Y-%m-%d %H:%M:%OS")

ROR$diff_seconds = as.numeric(ROR$date - ROR$send_time,units='secs')
ROR$diff_minutes  = as.numeric(ROR$date - ROR$send_time,units='mins')
ROR$diff_hours  = as.numeric(ROR$date - ROR$send_time,units='hours')



#'Remove outliers
#'---
#'
ROR = subset(ROR,diff_hours<1000 & diff_hours>0)
s = plot(ROR$diff_hours,ylab=list('Duration(h)',cex=0.8),
     xlab=list('# of emails sent',cex=0.8),
     main=list('Delivery Report'),
     cex=1.0,font=2,col='lightgrey',lwd=1,type='h')
box(lty = '1373', col = 'orange')
abline(h= mean(ROR$diff_hours),col="red",lty='dashed' )





#' Subscriber Retention Rate
#' ----------
#' 
#' 
#' 

agr3 = ddply(SbscG, .(campaign_id),
             plyr::summarize,
             subs = sum(status=="subscribed"),
             unsubs = sum(status=="unsubscribed" ),
             boun = sum(status=="bounce")) 

agr3$retention_rate = as.numeric((agr3$subs - agr3$unsubs - agr3$boun) / (agr3$subs) )


h5= ggplot(agr3, aes(factor(campaign_id),retention_rate,group=1)) +
  geom_line() +
  geom_smooth() +
  theme_light() +
  xlab("campaign_id") +
  ylab("retention_rate") +
  scale_color_discrete("") +
  ggtitle("Retention_rate per campaign_id") +
  theme(plot.title=element_text(size=rel(1.2)))

p5 = h5+theme(legend.position="bottom")
gg5=ggplotly(p5)
print(gg5)
htmlwidgets::saveWidget(as.widget(gg5), "://html/index4.html")



#' Bounce Rate (per campaign_id)
#' ----------
#' 
#' 
#' 

agr4 = ddply(Bounce, .(campaign_id,emails_sent),
             plyr::summarize,
             ok = sum(type=="/N"),
             soft = sum(type=="soft"),
             hard = sum(type=='hard')) 

agr4$hard_bounce_rate = as.numeric((agr4$hard / agr4$emails_sent)*100)
agr4$soft_bounce_rate = as.numeric((agr4$soft / agr4$emails_sent)*100)
agr4$bounce_rate = as.numeric(agr4$hard_bounce_rate + agr4$soft_bounce_rate)

h6 = ggplot(agr4, aes(factor(campaign_id),bounce_rate,group=1)) +
  geom_point() +
  geom_smooth() +
  theme_light() +
  xlab("campaign_id") +
  ylab("bounce_rate") +
  scale_color_discrete("") +
  ggtitle("Bounce Rate per campaign_id") +
  theme(plot.title=element_text(size=rel(1.2)))

p6 = h6+theme(legend.position="bottom")
gg6 = ggplotly(p6)
print(gg6)
htmlwidgets::saveWidget(as.widget(gg6), "://html/index5.html")





#' Location analysis
#' ---------
#' 
#' 
#' 

spout = sp1[-which(is.na(sp1$location_country_code)), ]



agr5 = ddply(spout,.(location_country_code),
             plyr:: summarize,
             summary_open_emails = sum(action=="click"),
             sumary_clicked_emails = sum(action=="open"))
             


agr5.melt <- melt(agr5, id = "location_country_code")


h7 = ggplot(agr5.melt, aes(location_country_code , value,fill = variable )) + 
  geom_bar(stat = "identity", position = "identity")+
  coord_flip()+
  theme_light() +
  xlab("location_country_code") +
  ylab("value") +
  scale_color_discrete("") +
  ggtitle("Email activity per country") +
  theme(plot.title=element_text(size=rel(1.2)))

p7 = h7+theme(legend.position="bottom")
gg7=ggplotly(p7)
print(gg7)
htmlwidgets::saveWidget(as.widget(gg7), "://html/index6.html")









#' RANDOM FOREST 
#' --------
#' 
#' 
#' 
#' 

detach("package:RPostgreSQL", unload=TRUE)

tes = sqldf("select action,email_address,emails_sent,email_id,hour,day,month,season,year,working_day
            from email_activity join email_campaigns 
            using(campaign_id)")

model_data  = sqldf("select * from tes left join email_lists using (email_address) ")

aggr = ddply(model_data,.(email_address,hour,day,month,status),
             plyr:: summarize,
             summary_open_emails = sum(action=="click"),
             sumary_clicked_emails = sum(action=="open"),
             sumary_bounced_emails = sum(action=="bounce"))

aggr[!complete.cases(aggr),]  
aggr = na.omit(aggr)  

names = c("email_address", "day", "status")
aggr[,names]  =  lapply(aggr[,names], factor)


smp_size = floor(0.70 * nrow(aggr))             # splitting into training and test:
                                                # 70% of the sample size

set.seed(400)                                   # set the seed to make your partition reproductible
train_ind = sample(seq_len(nrow(aggr)), size = smp_size)


train_data = aggr[train_ind, ]                  # split the data
test_data = aggr[-train_ind, ]



predictors = train_data[,which(!names(train_data) %in% c("email_address", "day", "status"))]           # what to use in the model
response = as.factor(train_data[,"status"])


rf = randomForest(x = predictors, y = response)



predictor_test = test_data[,which(!names(test_data) %in% c("email_address", "day", "status"))]        #run it using test data and compare results to reality
response_test = as.factor(test_data[,"status"])


prediction = predict(rf, predictor_test)                      # check result on test set
predictor_test$correct = as.character(prediction) == as.character(response_test)


table(as.character(prediction) == as.character(response_test)) # How many were correct?
accuracy = sum(predictor_test$correct) / nrow(predictor_test)




getCompareTable = function (test_data, prediction) {        # function to get plot data format
library(dplyr)
  

 actual_freq = table(aggr$status)                             # plot real vs model status
 predicted_freq = table(prediction)
  
 actual_freq = actual_freq[order(actual_freq)]
 predicted_freq = predicted_freq[order(predicted_freq)]
  
 actual_freq_s = data.frame(status = names(actual_freq), 
                              actual = as.vector(actual_freq), 
                              stringsAsFactors = F)
  
 predicted_freq_s = data.frame(status = names(predicted_freq),
                                 predict = as.vector(predicted_freq),
                                 stringsAsFactors = F)
  
 actual_freq_s$actual = unname(actual_freq_s$actual)
 predicted_freq_s$predict = unname(predicted_freq_s$predict)
  
  compare = dplyr::left_join(actual_freq_s, predicted_freq_s, by = "status")
  compare
}


library(reshape2)                                     # use function to get plot data
compare = getCompareTable(test, prediction)


compare_long = melt(compare)                          # plot the predicted vs actual in test set
g = ggplot(data = compare_long, aes(x=status, y = value, colour = variable, group = variable)) + 
  theme_bw()
g1 =  g + geom_bar(stat = "identity", position = "dodge", aes(fill=variable))
f = ggplotly(g1)
print(f)  
htmlwidgets::saveWidget(as.widget(f), "://html/index7.html")  
  
 




#' Principal Component Analysis (PCA)
#' ---
#' 
#' 
#' 


ag = ddply(model_data,.(email_address,stats_avg_click_rate,stats_avg_open_rate),
             plyr:: summarize,
             summary_open_emails = sum(action=="click"),
             sumary_clicked_emails = sum(action=="open"),
            sumary_bounced_emails = sum(action=="bounce"),
           emsent = sum(emails_sent),
           gmail = sum(email_client=="Gmail"),
           amail = sum(email_client=="Apple mail"))

ag$email_address <- NULL
ag[!complete.cases(ag),]  
ag = na.omit(ag)  

pc = princomp(~ ., data = ag) #cor = TRUE)                   # Finding number of components
plot(pc, type="l")


summary(pc)                                                 # look for dimension that is ~ 85% variance
loadings(pc)
pc = prcomp(ag)                                             # run more convenient pca needed for k-means

comp = data.frame(pc$x[,1:8])                               # Chose top 8 dimensions for showing an example
                                                            # limit data to first 8 columns

wss = nrow(comp)-1 * sum(apply(comp,2,var))                # Determine number of clusters & run kmeans for varying number of clusters 1 to 15

for (i in 2:15) 
  wss[i] = sum(kmeans(comp,centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")


k = kmeans(comp, 4, nstart=25, iter.max=1000)              # chose 4 clusters
kResults = data.frame(ag, cluster = k$cluster)


rl = as.data.frame(lapply(1:4, function(x){                # Transform data for columns of cluster
  r3 = kResults[kResults$cluster == x, setdiff(names(kResults), 'cluster')] 
r4 = colSums(r3) / nrow(r3)
r4
}))

names(rl) = paste("cluster",1:4)


q = d3heatmap(rl, theme="dark", scale = 'row')              # plot using d3heatmap library
htmlwidgets::saveWidget(as.widget(q), "://html/index8.html")  
