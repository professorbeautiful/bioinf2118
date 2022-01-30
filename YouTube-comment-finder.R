### YouTube-comment-finder.R

install.packages('vosonSML')
require('vosonSML')
youtube_key <- 'AIzaSyARyZoMghXUtqJGG6iGr7NG3EpJgnVNYs0'

credential = Authenticate(socialmedia = 'youtube', 
                          apiKey = 'AIzaSyARyZoMghXUtqJGG6iGr7NG3EpJgnVNYs0')
## AIzaSyARyZoMghXUtqJGG6iGr7NG3EpJgnVNYs0
## boreal-augury-270315
## dKklcL1WNec   Hughes and Tyson
# HQrfbPAoHW8   -- Banuelos comment

comments = Collect(credential, 
        videoIDs='pWlk1gLkF2Y', 
        verbose = TRUE,
        writeToFile = FALSE, 
        maxComments = 1000)
sum(comments$AuthorDisplayName == 'ProfessorBeautiful')
## 5
mine = which(comments$AuthorDisplayName == 'ProfessorBeautiful')
comments$ReplyCount[mine]
comments$LikeCount[mine]
table(as.numeric(comments$ReplyCount))
table(as.numeric(comments$LikeCount))
names(comments)
comments$Comment[comments$LikeCount==320]
igraph::plot.igraph(Graph(Create(comments, type='actor')) )
###   igraph_demo('centrality')

#  viascience hydrogen  fSoV4s_DLr8  ylh4CLS-gxs
via = Collect(credential, 
        videoIDs='fSoV4s_DLr8', 
        verbose = TRUE,
        writeToFile = FALSE, 
        maxComments = 1000)
grep('Scilab', unlist(via$Comment), ignore.case = T )
via$Comment[grep('Scilab', unlist(via$Comment), ignore.case = T )]

# G Suite trial, starts charging $12/month on july 18.
#    q2tqAHK#toTz
#  me@professorbeautiful.org
#  I added a DNS to my dreamhost account, as provided by G Suite:  google-site-verification=RchpHhpw4EtVwugQx-CoHaXy99QoAr7lm0uaEcCHIKk
# Refreshed the DNS server for professorbeautiful.org
#  (could be 30 minutes)
# Then G Suite will finish verifying my domain.
# https://console.developers.google.com/tos?id=safebrowsing
# https://cloud.google.com/terms/free-trial/?_ga=2.22111675.-1029412731.1591374474
##  Your free trial includes $300 in credit to spend over the next 12 months. If you run out of credit, don’t worry — you won’t be billed unless you turn on automatic billing.
##  OK, all set!


library(httr)
req <- httr::GET("https://www.googleapis.com/youtube/v3/search", 
                 query = list(part = "snippet",
                              q = "Neil DeGrasse Tyson",
                              maxResults = 25,
                              key = youtube_key))

httr::content(req)
