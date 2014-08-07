# The Entrez Programming Utilities
# (E-utilities) のラッパー関数
# http://www.ncbi.nlm.nih.gov/books/NBK25497/

# PubReaderのサポート
# http://www.ncbi.nlm.nih.gov/pmc/about/pr-browsers/

#依存パッケージのの読み込み

####################### パッケージング ########################

# # PubMedID取得関数
# get.PubMedID <- function(author, year, keyword, MeSH){
# 	url <- paste0("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&id=", ID_list, "&usehistory=y")
# }

# # PMCID取得関数
# get.PMCID <- function(author, year, keyword, MeSH){
# 	url <- paste0("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&id=", ID_list, "&usehistory=y")
# }


http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?
db=pubmed&
id=25066236
&query_key=25066236
&usehistory=y


# Markdownによるabst表示
# knitrを利用した美しい見た目のhtml
# PDF取得以外は、PubMed, PMC両対応
# html, pdf, csv, tsv, text, excel

# PubMed文献取得関数
get.PubMed <- function(ID, destdir="./"){
	# Check data type
	if(!is.vector(ID)){
		warning("ID must be vector!")
	}

	if(length(grep("^PMC", ID)) == 0){
		URL_list <- paste0("http://www.ncbi.nlm.nih.gov/pubmed/", ID)
		Output_list <- paste0(destdir, ID, ".html")
		sapply(1:length(ID), function(x, url_list, output_list){
			download.file(url=url_list[x], destfile=output_list[x])
			}, url_list=URL_list, output_list=Output_list)
	}else{
		warning("Please use only PubMed ID (e.g., 25066236), otherwise use get.PMC function")
	}
}

# PMC文献取得関数
get.PMC <- function(ID, format=c("html", "ePub", "pdf"), destdir="./"){
	# Check data type
	if(!is.vector(ID)){
		warning("ID must be vector!")
	}
	if(length(grep("^[0123456789]", ID)) == 0){
		############# html #############
		if(format == "html"){
			URL_list <- paste0("http://www.ncbi.nlm.nih.gov/pmc/articles/", ID, "/?report=classic")
			Output_list <- paste0(destdir, ID, ".html")
			sapply(1:length(ID), function(x, url_list, output_list){
				download.file(url=url_list[x], destfile=output_list[x])
				}, url_list=URL_list, output_list=Output_list)
		############# ePub #############
		}else if(format == "ePub"){			
			URL_list <- paste0("http://www.ncbi.nlm.nih.gov/pmc/articles/", ID, "/epub")
			Output_list <- paste0(destdir, ID, ".epub")
			sapply(1:length(ID), function(x, url_list, output_list){
				download.file(url=url_list[x], destfile=output_list[x])
				}, url_list=URL_list, output_list=Output_list)

		############# pdf #############
		}else if(format == "pdf"){
			URL_list <- paste0("http://www.ncbi.nlm.nih.gov/pmc/articles/", ID, "/pdf")
			Output_list <- paste0(destdir, ID, ".pdf")
			sapply(1:length(ID), function(x, url_list, output_list){
				download.file(url=url_list[x], destfile=output_list[x])
				}, url_list=URL_list, output_list=Output_list)
		}else{
			warning('Please specify "type" as reader, html, ePub, or pdf')
		}
	}else{
			warning("Please use only PMCID (e.g., PMC4071818), otherwise use get.PubMed function")
	}
}


# 連結させて表示する場合
#### Pubmed
# http://www.ncbi.nlm.nih.gov/pubmed/?term=25066236%2C25041996

#### PMC
# http://www.ncbi.nlm.nih.gov/pmc/?term=((PMC4071818)+OR+PMC3591984)+OR+PMC3562110

###################### Test #####################
# 1. Pubmed
ID <- c(25066236, 25041996)
get.PubMed(ID)

# 2. PMC
ID <- c("PMC4071818", "PMC3591984", "PMC3562110")
get.PMC(ID, format="html")

get.PMC(ID, format="ePub")

get.PMC(ID, format="pdf")

# 3. PubMed / PMC <- Must be Error !
ID <- c("PMC4071818", 25018766, "PMC3591984", 23471618)
get.PubMed(ID)
get.PMC(ID)


# Objects list
objectslist <- c(
	"get.PubMedID",
	"get.PMCID",
	"get.PubMed",
	"get.PMC",
)

# Packaging
package.skeleton(name = "PubMedQuery", objectslist, path = ".")





