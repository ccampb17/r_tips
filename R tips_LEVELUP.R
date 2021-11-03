# R tips


#translate using google (gl_translate)

for(i in 1:nrow(update.table)){
  update.table$act.title.en[i]=gl_translate(as.character(update.table$act.title.ll[i]))$translatedText
  update.table$act.description.en[i] = gl_translate(as.character(update.table$act.description.ll[i]))$translatedText
}

#get the running processes as a df on the server
devtools::source_url("https://github.com/mathosi/cluster_check/blob/master/ps_to_df.R?raw=TRUE")
ps.to.df() #List All processes sorted by %CPU usage
ps.to.df(bylist.selection = "C rsession") #List All processes named 'rsession' sorted by %CPU usage


#pesky factors
library(dplyr)
inventory %>% mutate_if(is.factor, as.character) -> inventory

#use extract_tables to get table from PDF - must have consistent rows
plop = extract_tables("https://www.jbic.go.jp/ja/information/news/news-2018/pdf/0702-011178_5.pdf")

for (i in 1:length(plop)){
  
  if(i==1){
    r = as.data.frame(plop[i], stringsAsFactors = F)
  }else{
    r = mapply(c, r, data.frame(plop[i], stringsAsFactors = F)) %>% 
      as.data.frame(stringsAsFactors=F)
  }
  
}


#play a system sound in console
#simple, only plays default 'beep'
system("rundll32 user32.dll,MessageBeep 1")

#more complex, can play most sounds
system("PowerShell -C (New-Object System.Media.SoundPlayer 'C:\\Windows\\Media\\chimes.wav').PlaySync()")

system("PowerShell -C (New-Object System.Media.SoundPlayer 'C:\\Windows\\Media\\tada.wav').PlaySync()")


##timing logic for unstable scrapers

library(R.utils)


# adding article info loop

  for (i in 1:nrow(update.table)){
    cat(sprintf("Extracting article info %i/%i...", i, nrow(update.table)))
    tryCatch(
      withTimeout( {
        update.table$act.description.en[i] = extractInfo(remDr, update.table$act.url[i]);
        cat("[Done]\n") },
        timeout = 20),
      TimeoutException = function(ex){ cat("[Skipped due to timeout]\n")}, 
      error=function(cond){
        message("[caught error] timeout, skipping")
      },
      warning=function(cond){
        message("[caught warning] timeout, skipping")
      }
    )
    
    
  }
  



#direct GET to xpath-able html doc
html <- httr::GET(url = tgt.url) %>% 
  httr::content() %>%
  htmlParse(asText=T, encoding = "UTF-8")

#remove rows of a df that have NAs
squad.predictions = training.data[rowSums(is.na(training.data)) == 0,]

#method to remove NAs from a vector, useful when working with %>% 
v %>% purrr::discard(is.na)
v %>% `[`(!is.na(.))


#FTFY
reticulate::py_install("ftfy") #first time only
ftfy = reticulate::import("ftfy")
ftfy$fix_encoding("Ð¶â€“â€¡ÐµÂ­â€”ÐµÐŠâ€“Ð³Ðƒâ€˜") 
ftfy$fix_and_explain("è­ãƒ»ï½­æ€œå–§ç¸ºãƒ»")


#deliberately masking the function with this one to make the script work without having to edit it
read_html = function(some.url){
  return(httr::GET(url = some.url, httr::config(ssl_verifypeer = FALSE)) %>% 
           httr::content())
}

#including headers in the request
headers = c(
  "Host" = "pib.gov.in",
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:87.0) Gecko/20100101 Firefox/87.0",
  "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
  "Accept-Language" = "en-GB,en;q=0.5",
  "Accept-Encoding" = "gzip, deflate, br",
  "DNT" = "1",
  "Connection" = "keep-alive",
  "Referer" = "https://pib.gov.in/PressReleseDetail.aspx?PRID=1721269",
  "Cookie" = "PIB_Accessibility=Lang=1&Region=3; style=null; ASP.NET_SessionId=kg3awlkwcbpwkq10a5h2lmsj; SameSite=Lax",
  "Upgrade-Insecure-Requests" = "1",
  "Cache-Control" = "max-age=0" 
)


html <- read_html(httr::GET(url = main.url,
                            httr::add_headers(.headers=headers)))%>%
  html_text() %>%
  htmlParse(asText=T, encoding = "UTF-8")



#basic version
page <- httr::GET(url = "www.website.com/data",
                  httr::add_headers(.headers = headers)) %>%
  httr::content()

#use cmd.exe to find a string in a directory and subdirs
# win+r cmd.exe enter
#cd C:\Users\c-cam\GTA data team Dropbox\Bastiat
#findstr /s /i fw_user *.*
#findstr /s /i fw_user *.R


#regex for CJK characters, must have perl enabled!
grepl(pattern = "[\\p{Han}\\p{Hangul}]", 
      x = "ì¤‘êµ­ í…ì„¼íŠ¸, ìŒì•…ì €ìž‘ê¶Œ ë…ì ì‚¬ìš©ê¶Œ í¬ê¸°ì— ê´€í•œ ì„±ëª…ì„œ ë°œí‘œ", 
      perl=T)

# great resource for explaining... well, advanced R!
# e.g. subsetting types, etc
"http://adv-r.had.co.nz/"

# big repo of themes (these should be compatible with rstudio)
# https://tmtheme-editor.herokuapp.com/#!/editor/theme/Lucario

# DO NOT NAME VARS THINGS LIKE 'table' WITHOUT A FURHER QUALIFIER
# for find and replace purposes

# DATA FRAMES AND RBIND

# ALWAYS include
stringsAsFactors=F
# or face the consequences

# To initialise a blank one
df = data.frame...
df = data.frame(matrix(nrow=0, ncol=numberOfColumns), stringsAsFactors = F)
#REMEMBER: when you do this, you can't define the colnames until the df has been populated with at least one row of data

# remember when rbinding, better to rbind with another data.frame instead of c()ing a bunch of variables

#this can lead to problems like dates being coerced back into numbers
# which is bad

#change system locale. Second param should be the name in english, eg "Russian", "Arabic", "Spanish" etc
# only works for WINDOWS
Sys.setlocale("LC_ALL","Japanese")

# to remove rows based on duplicates in one column
table=subset(table, !duplicated(act.title.en))

# on more than one column
term.freq.rlv = term.freq.rlv %>%
  filter(!duplicated(cbind(word, doc)))


# to get the current year
format(Sys.Date(), "%Y")

#keep rows of df that contain date > something based on one col
table.new = subset(table.main, table.main$date.from > "2020-01-01")


# to keep entries in a dataframe containing a certain word (add ! to negate obvs)
table.main = table.main[grepl("ARANCEL", table.main$act.title.ll),]


#'zip'/concatenate/combine each row of df into string, then these into string vector with length nrow(df)
months.master = apply(month.names, 1, paste, collapse=")|(")

#remove backslashes
gsub("\\\\", "", string)


#summmarise cats by features
train.full %>% group_by(relevant) %>% summarise_all(mean)



#fix encoding of a column in a dataframe
Encoding(articles$act.title.en) <- "UTF-8"




#Encoding fix from JF
## more encoding worries
table.main$act.title.en=gsub("<.*?>","",iconv(table.main$act.title.en, "", "ASCII", "byte"))
table.main$act.description.en=gsub("<.*?>","",iconv(table.main$act.description.en, "", "ASCII", "byte"))




###WEBDRIVER###
#init
url <- "https://www.zawya.com/saudi-arabia/en/the-vault/latest/policy/"
pjs <- run_phantomjs()
remDr=Session$new(port=pjs$port)
remDr$go(url)

#get html standard
html <- htmlParse(remDr$getSource()[[1]], asText=T)

#coercing dates which have saved as numeric strings back into dates
articles$act.date = as.Date(as.numeric(articles$act.date), origin = "1970-01-01")

# cool progress bar
pb = txtProgressBar(min = 0, max = 100, char = "~", style = 3)
for (i in 1:100){
  Sys.sleep(0.01)
  setTxtProgressBar(pb, i)
}
close(pb)


#iterative binding
cc_iter_bind = function(iter.var = i, FUN, df, iter.var.min = 1){
  
  library(dplyr)
  
  if(iter.var == iter.var.min){
    return(FUN(df))
  } else {
    return(bind_rows(df, FUN(df)))
  }
  
  
}

###XPATH TIPS###


#xpath for node value contains
Xpath="//*[contains(text(),'here')]"

#attr contains
xpath="//a[contains(@prop,'Foo')]"

#xpath intersection of to nodesets
#The kayessian|kayesian intersection of two node-sets $ns1 and $ns2 is evaluated by the following XPath expression:

xpath="$ns1[count(.| $ns2)=count($ns2)]"

bt_xpath_nodes_intersect = function(ns1, ns2){
  return(paste0(
    ns1, "[count(.| ", ns2, ")=count(", ns2, ")]"
  ))
}


  
  
###USING RSELENIUM INSTEAD OF WEBDRIVER - DO NOT FORGET TO CLEAN UP AS WELL!!!!!!###
library(wdman)
library(RSelenium)

available.versions<-binman::list_versions("chromedriver")
#for your computer
latest.version = available.versions$win32[length(available.versions$win32)-1]
#for the server
latest.version = available.versions$linux64[length(available.versions$linux64)-1]


eCaps <- list(chromeOptions = list(
  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
))
rD <- rsDriver(browser=c("chrome"), chromever=latest.version, extraCapabilities = eCaps)

remDr <- rD$client
remDr$open()
remDr$navigate("http://www.google.com")
remDr$screenshot(display = TRUE)

#equivs

html <- htmlParse(remDr$getPageSource()[[1]], asText=T)

e=remDr$findElement(using = "xpath", "//xpath goes here")
  
#RSelenium clean up!
rD$server$stop()
remDr$close()
rm(rD)
gc()
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

