# R tips

# great resource for explaining... well, advanced R!
# e.g. subsetting types, etc
"http://adv-r.had.co.nz/"

# big repo of themes (these should be compatible with rstudio)
# https://tmtheme-editor.herokuapp.com/#!/editor/theme/Lucario

# Data frames are great.

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

#finding and extract a country name in a string
# eg convert 'Due to the spread of COVID-19 disease, the Government of the Republic of Kosovo upon decision no. 01/09, dated 13 March 2020, as amended by...'
# to just 'Kosovo'
for(i in 1:nrow(update.table)){
  update.table$country.lead[i] = str_extract(update.table$act.title.en[i],
                                             paste(gtalibrary::country.names$name, collapse = "|"))
}


#alternate version
#basic script to try and get the country, works..... sometimes.
for(i in 1:nrow(update.table)){
  update.table$country[i] = str_extract(update.table$act.title.en[i],
                                        paste(gtalibrary::country.names$name, collapse = "|"))
  
  if(is.na(update.table$country[i])){update.table$country[i] = "Unknown"}
  
  update.table$country.lead[i] = update.table$country[i]
}


##advanced version
#tried to do a vectorised version but to no avail (grepl doesn't take vector as pattern)
#expensive and slow but works pretty well
countries.matcher = data.frame(gta.name = c("Afghanistan","Albania","Algeria","American Samoa","Andorra","Angola","Antigua & Barbuda","Azerbaijan","Argentina","Australia","Austria","Bahamas","Bahrain","Bangladesh","Armenia","Barbados","Belgium","Bermuda","Bhutan","Bolivia","Bosnia & Herzegovina","Botswana","Brazil","Belize","Solomon Islands","British Virgin Islands","Brunei Darussalam","Bulgaria","Myanmar","Burundi","Belarus","Cambodia","Cameroon","Canada","Cape Verde","Cayman Islands","Central African Republic","Sri Lanka","Chad","Chile","China","Chinese Taipei","Colombia","Comoros","Mayotte","Congo","DR Congo","Cook Islands","Costa Rica","Croatia","Cuba","Cyprus","Czechia","Benin","Denmark","Dominica","Dominican Republic","Ecuador","El Salvador","Equatorial Guinea","Ethiopia","Eritrea","Estonia","Faeroe Islands","Falkland Islands","Fiji","Finland","France","French Guiana","French Polynesia","Djibouti","Gabon","Georgia","Gambia","State of Palestine","Germany","Ghana","Kiribati","Greece","Greenland","Grenada","Guadeloupe","Guam","Guatemala","Guinea","Guyana","Haiti","Vatican","Honduras","Hong Kong","Hungary","Iceland","Indonesia","Iran","Iraq","Ireland","Israel","Italy","Ivory Coast","Jamaica","Japan","Kazakhstan","Jordan","Kenya","DPR Korea","Republic of Korea","Kuwait","Kyrgyzstan","Lao","Lebanon","Lesotho","Latvia","Liberia","Libya","Liechtenstein","Lithuania","Luxembourg","Macao","Madagascar","Malawi","Malaysia","Maldives","Mali","Malta","Martinique","Mauritania","Mauritius","Mexico","Monaco","Mongolia","Republic of Moldova","Montenegro","Montserrat","Morocco","Mozambique","Oman","Namibia","Nauru","Nepal","Netherlands","Netherlands Antilles","Aruba","New Caledonia","Vanuatu","New Zealand","Nicaragua","Niger","Nigeria","Niue","Norfolk Island","Norway","Northern Mariana Islands","Micronesia","Marshall Islands","Palau","Pakistan","Panama","Papua New Guinea","Paraguay","Peru","Philippines","Pitcairn","Poland","Portugal","Guinea-Bissau","Timor-Leste","Puerto Rico","Qatar","Reunion","Romania","Russia","Rwanda","Saint-Barthelemy","Saint Helena","Saint Kitts & Nevis","Anguilla","Saint Lucia","Saint-Martin","Saint Pierre & Miquelon","Saint Vincent & the Grenadines","San Marino","Sao Tome & Principe","Saudi Arabia","Senegal","Serbia","Seychelles","Sierra Leone","India","Singapore","Slovakia","Vietnam","Slovenia","Somalia","South Africa","Zimbabwe","Spain","South Sudan","Republic of the Sudan","Western Sahara","Suriname","Svalbard & Jan Mayen Islands","Swaziland","Sweden","Switzerland","Syria","Tajikistan","Thailand","Togo","Tokelau","Tonga","Trinidad & Tobago","United Arab Emirates","Tunisia","Turkey","Turkmenistan","Turks & Caicos Islands","Tuvalu","Uganda","Ukraine","Macedonia","Egypt","United Kingdom","Tanzania","United States of America","US Virgin Islands","Burkina Faso","Uruguay","Uzbekistan","Venezuela","Wallis & Futuna Islands","Samoa","Yemen","Zambia"),
                               name.part = c("afgha","albania","algeria","american samoa","andorr","angola","antigua","azerbaija","argentin","australia","austria","bahama","bahrai","banglades","armeni","barbado","belgiu","bermud","bhuta","bolivi","bosnia","botswan","brazi","beliz","solomon island","british virgin","brunei","bulgari","(myanma)|(burma)","burund","belaru","cambodi","cameroo","canada","(cape verde)|(cabo verde)","cayman island","central african rep","sri lank","chad\\W","chile\\W","china\\W","(taipei)|(taiwan)","colombia","comoro","mayotte","congo","(dr congo)|(democratic republic of congo)","cook island","costa rica","croati","cuba\\W","cyprus","czech","benin","denmark","dominica(?!n)","dominican republi","ecuador","el salvador","equatorial guine","ethiopi","eritrea","estonia","faeroe","falkland island","fiji","finland","france","french gu","french polynesi","djibout","gabon","georgia","gambia","palestin","germany","ghana","kiribati","greece","greenland","grenada","guadeloupe","guam","guatemala","guinea(?!(-b)|( bis))","guyan","hait","vatican","hondura","hong kon","hungary","iceland","indonesia","iran","iraq","ireland","israel","italy","(ivory coas)|(c.{1,3}te d'ivoire)","jamaica","japan","kazakhstan","jordan","kenya","(dprk)|(north korea)|(dpr korea)","south korea","kuwait","kyrgyz","lao","lebanon","lesotho","latvia","liberia","libya","liechtenstein","lithuania","(luxembourg)|(europe)|(\\Weu\\W)","maca[uo]","madagascar","malawi","malaysia","maldive","mali\\W","malta","martinique","mauritani","mauritius","mexico","monaco","mongoli","moldova","montenegr","montserra","morocco","mozambiqu","oman\\W","namibia","nauru","nepal","netherland","netherlands antille","aruba\\W","new caledonia","vanuatu","new zealand","nicaragua","niger\\W","nigeria","niue\\W","norfolk islan","norway","mariana","micronesia","marshall island","palau","pakistan","panama","(\\Wpng\\W)|(papua)","paraguay","peru\\W","philippine","pitcairn","poland","portugal","bissau","(east timor)|(timor-lest)","puerto ric","qatar","reunion","romani","russia","rwanda","saint-barthelem","saint helen","kitts","anguill","\\Wlucia","saint-marti","miquelon","vincent","san marino","principe\\W","saudi arabi","senegal","serbia\\W","seychelle","sierra leon","india","singapor","slovaki","viet ?nam","sloveni","somali","south afric","zimbabw","spain\\W","south suda","republic of the suda","western sahar","surinam","svalbard","swazilan","sweden","switzerlan","syria\\W","tajikista","thailan","togo\\W","tokela","tonga\\W","trinidad","(uae\\W)|(emirates)","tunisia","turkey","turkmenistan","caicos","tuvalu","uganda","ukrain","macedonia","egypt","(uk\\W)|(britain)|(united kingdo)","tanzani","(usa\\W)|(us\\W)|(united states)","us virgin is","burkina fas","uruguay","uzbekistan","venezuela","futuna","samoa\\W","yemen","zambia"),
                               stringsAsFactors = F)

save(countries.matcher, file = "R help files/countries_matcher.Rdata")

for (i in 1:nrow(update.table)){
  for(j in 1:nrow(countries.matcher)){
    if(grepl(countries.matcher$name.part[j], update.table$country.lead[i], ignore.case = T, perl = T)){
      update.table$country[i] = countries.matcher$gta.name[j]
    }
  }
}






#fix encoding of a column in a dataframe
Encoding(articles$act.title.en) <- "UTF-8"


# RUSSIAN MONTHS CONVERTER

cc_date_ru = function(ru.date){
  
  day = str_extract(ru.date, "\\d{1,2}")
  year = str_extract(ru.date, "\\d{4}")
  ru.month = str_extract(ru.date, "(?<=\\d{1,2} ).+(?= \\d{4})")
  
  #remember these are in the genitive
  russian.months = c("ÑÐ½Ð²Ð°Ñ€Ñ", "Ñ„ÐµÐ²Ñ€Ð°Ð»Ñ", "Ð¼Ð°Ñ€Ñ‚Ð°", "Ð°Ð¿Ñ€ÐµÐ»Ñ", "Ð¼Ð°Ñ", "Ð¸ÑŽÐ½Ñ", "Ð¸ÑŽÐ»Ñ", "Ð°Ð²Ð³ÑƒÑÑ‚Ð°", "ÑÐµÐ½Ñ‚ÑÐ±Ñ€Ñ", "Ð¾ÐºÑ‚ÑÐ±Ñ€Ñ", "Ð½Ð¾ÑÐ±Ñ€Ñ", "Ð´ÐµÐºÐ°Ð±Ñ€Ñ")
  
  #well, it took me an hour to find that this one line fixes the stupid encoding problem (this time) after some insane attempts to solve it.
  #why why why can't we all just get along and use UTF8
  Encoding(russian.months) <- "UTF-8"
  
  #russian.months.utf = c("<U+044F><U+043D><U+0432><U+0430><U+0440><U+044F>","<U+0444><U+0435><U+0432><U+0440><U+0430><U+043B><U+044F>","<U+043C><U+0430><U+0440><U+0442><U+0430>","<U+0430><U+043F><U+0440><U+0435><U+043B><U+044F>","<U+043C><U+0430><U+044F>","<U+0438><U+044E><U+043D><U+044F>","<U+0438><U+044E><U+043B><U+044F>","<U+0430><U+0432><U+0433><U+0443><U+0441><U+0442><U+0430>","<U+0441><U+0435><U+043D><U+0442><U+044F><U+0431><U+0440><U+044F>","<U+043E><U+043A><U+0442><U+044F><U+0431><U+0440><U+044F>","<U+043D><U+043E><U+044F><U+0431><U+0440><U+044F>","<U+0434><U+0435><U+043A><U+0430><U+0431><U+0440><U+044F>")
  month = match(ru.month, russian.months)
  
  int.date = paste(year, month, day, sep = "-")
  return(as.Date(int.date))
  
}


#Encoding fix from JF
## more encoding worries
table.main$act.title.en=gsub("<.*?>","",iconv(table.main$act.title.en, "", "ASCII", "byte"))
table.main$act.description.en=gsub("<.*?>","",iconv(table.main$act.description.en, "", "ASCII", "byte"))


#translate using google (gl_translate)

for(i in 1:nrow(update.table)){
  update.table$act.title.en[i]=gl_translate(as.character(update.table$act.title.ll[i]))$translatedText
  update.table$act.description.en[i] = gl_translate(as.character(update.table$act.description.ll[i]))$translatedText
}



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
for (i in 1:100){
  Sys.sleep(0.1)
  pb = txtProgressBar(min = 0, max = 100, char = "â–º", style = 3)
  setTxtProgressBar(pb, i)
}
close(pb)


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


#summmarise cats by features
train.full %>% group_by(relevant) %>% summarise_all(mean)