apistatus <- function() {
	require(RCurl); require(RJSONIO); require(ggplot2); require(XML)
	
	# PLoS search API
	if(identical(
		RJSONIO::fromJSON(RCurl::getURL(
			"http://api.plos.org/search?q=id:10.1371/journal.pbio.0000012&wt=json"))$response$docs[[1]]$eissn, 
		"1545-7885") == TRUE){plos_search_api <- 1} else
		{plos_search_api <- 0}
	
	# PLoS ALM API
	if(identical(
		RJSONIO::fromJSON(RCurl::getURL(
			"http://alm.plos.org/articles/10.1371/journal.pbio.0000012.json"))$article$pub_med, 
		"14551910") == TRUE){plos_alm_api <- 1} else
		{plos_alm_api <- 0}
	
	# OpenSNP API
	if(identical(
		RJSONIO::fromJSON(RCurl::getURL(
			"http://opensnp.org/snps/json/rs9939609/1.json"))$user$name, "Bastian Greshake") == TRUE)
	{opensnp_api <- 1} else
	{opensnp_api <- 0}
	
	# ONe of the VertNet APIs
	if(identical(
		RJSONIO::fromJSON(RCurl::getURL(
			"http://canary.vert-net.appspot.com/api/search?genus=calidris&limit=1"))$records[[1]]$geodeticdatum
		, "WGS84") == TRUE) {vertnet_api <- 1} else
		{vertnet_api <- 0}
	
	# ITIS API
	namespaces <- c(ax23="http://data.itis_service.itis.usgs.org/xsd")
	if(identical(
		sapply(XML::getNodeSet(XML::xmlParse(RCurl::getURL(
			"http://www.itis.gov/ITISWebService/services/ITISService/searchByCommonName?srchKey=ferret-badger")), 
								"//ax23:commonName", namespaces=namespaces), XML::xmlValue)[[1]]
		, "Everett's ferret-badger") == TRUE) {itis_api <- 1} else
		{itis_api <- 0}
	
	# Altmetric.com API
	if(identical(
		RJSONIO::fromJSON(RCurl::getURL(
			"http://api.altmetric.com/v1/doi/10.1038/480426a"))$altmetric_jid,
			"4f6fa62f3cf058f6100082d3") == TRUE) {altmetcom_api <- 1} else
		{altmetcom_api <- 0}
	
	# Springer Open Access API
	if(identical(
		RJSONIO::fromJSON(RCurl::getURL(
			"http://api.springer.com/openaccess/json?q=dna&api_key=4qv484abnnptychrn52y9rb4&p=1"))$query,
				"dna ") == TRUE) {springeroa_api <- 1} else
						{springeroa_api <- 0}
	
	# PubMed Central OAI-PMH service
	if(identical(
		XML::xmlToList(XML::xmlParse(RCurl::getURL(
			"http://www.pubmedcentral.gov/oai/oai.cgi?verb=Identify")))$Identify$repositoryName,
		"PubMed Central") == TRUE) {pmc_api <- 1} else
		{pmc_api <- 0}
	
	# National Phenology Network API
	if(identical(
		RJSONIO::fromJSON(RCurl::getURL(
			"http://www.usanpn.org/npn_portal/observations/getAllObservationsForSpecies.json?&species_id[1]=53&start_date=2008-01-01&end_date=2008-05-10"
			))$station_list[[1]][[1]],
		"1915") == TRUE) {npn_api <- 1} else
		{npn_api <- 0}
	
	# Create data.frame of results from API checks
	dat <- data.frame(
		names = c("PLoS Search API","PLoS ALM API","OpenSNP API","VertNet API",
							"ITIS API","Altmetric.com API","Springer OA API","PubMed Central API",
							"National Phen Net API"), 
		values = c(plos_search_api, plos_alm_api, opensnp_api, vertnet_api, 
							 itis_api, altmetcom_api, springeroa_api, pmc_api, npn_api))
	dat$status <- ifelse(dat$values == 1, "Okay!", "Sad :(")
	
	p <- ggplot2::ggplot(dat, aes(names, values, colour=factor(values), label=status)) +
		theme_bw(base_size=18) +
		geom_point(size=16) +
		scale_colour_manual(values = c("0" = "red","1" = "green")) +
		geom_text(hjust=2) +
		coord_flip() +
		labs(x="",y="") +
		opts(legend.position="none", panel.grid.major=theme_blank(),
				 panel.grid.minor=theme_blank(), axis.text.x=theme_blank(),
				 axis.ticks=theme_blank(), panel.border = theme_blank())
	print(p)
	invisible();
}
# apistatus()