# load package
require(RCurl)
# read script lines from website
SEM1 <- "https://raw.github.com/joaovissoci/Data/master/ScriptDatabaseSEM.R"
script <- getURL(SEM1, ssl.verifypeer = FALSE)
eval(parse(text = script))
# clean-up
rm("script", "SEM1")
# check
ls()
#[1] "bingSearchXScraper"
#Type the name of the database checked

Teste <- "http://datadryad.org/bitstream/handle/10255/dryad.1889/NicSolstatus.csv?sequence=1"
script <- getURL(Teste, ssl.verifypeer = FALSE)
eval(parse(text = script))
# clean-up
rm("script", "SEM1")
# check
ls()
#[1] "bingSearchXScraper"
#Type the name of the database checked
script
SEM1