# adsfinalproj
My repo for the final project for Advanced Data Science at JHSPH

Inital Proposal:

Option 6 - Project Petition

Question: Is there a correlation between the safer sex practices employed in gay porn videos viewed on popular pornographic websites in the United States in the last 5 - 10 years and:
  1. PrEP's US timeline (approval of Truvada as for pre-exposure prophylaxis, news cycle and ad campaigns, major health department efforts, cultural mentions, etc.) ,
  2. Rates of gonorrhea, syphilis, and chlamydia among MSM in the USA,
  3. Timeline of gay male social apps (Grindr, Scruff, etc.)?

Background:

PornHub
PornHub is the world's largest pornography site on the internet, operating now for 10 years--currently, it is the 39th most popular site on the website. Operating similarly to YouTube, PornHub claims itself as a "platform" and a "video host:" in such a way, PornHub is capable of reaping the benefits of the massive traffic generated to its site while claiming no responsibility for the videos therein.
In 2010, the start-up was bought out by a large adult entertainment conglomerate Manwin (now known as MindGeek), who owns several other similar websites.
Started in 2013, PornHub Insights (https://www.pornhub.com/insights) is a blogging platform wherein the massive quantities of data reaped from PornHub users would be periodically analyzed. Insights posts were generally themed around a particular large event, monitoring traffic changes around NBA finals, episodes of Game of Thrones, or the path of totality for the 2017 solar eclipse. Occassionally, and at the end of every year, PronHub Insights will break down data more generally, by geography, times of access, popularity of search terms and categories, etc.

PrEP
Pre-exposure prophylaxis, or PrEP, is the preempstive use of a drug to prevent disease in unexposed populations. Currently, the term is used almost exclusively as shorthand for the use of the antiretroval drug Truvada, a two-drug combo manufactured by Gilead Sciences, to prevent the replicaton of HIV. Truvada's regimen as of the time of writing is a single pill taken daily coupled with a follow-up visit with the patient's primary care provider every three months, presumably in perpetuity.
Truvada has had the most adoption success among men who have sex with men (MSM) in the United States after approval by the CDC for use as PrEP in 2014. Moves toward widespred adoption of PrEP have been divisive, politically and within gay culture. Concerns include the likelihood of PrEP being used counter to the prescription (such as in the phenomenon of "disco dosing," an ineffective practice in which gay men take a Truvada pill only around the time they expect sex), the potential for PrEP to undermine existing safer sex policies and social mores, and the incredible cost of the drug (as high as $450 / month). As of 2017, Truvada as PrEP has been approved by 9 countries in addition to the United States as well as the WHO.

Gay Porn
Since the AIDS crisis in the 80s, condoms have been standard parts of gay pornographic films. While condom-less gay porn ("barebacking") still did exist, it was commonly regarded as "kink," fetish" or otherwise deviant, and was often the domain of specialized or heterodox smaller production studios. In recent years, porn studios, both gay and straight, have seen their profits being eaten in to by a combination of three phenomenons:
  1. The rise of the "tube" sites (such as PornHub), which aggregate and disseminate pornography, usually without a membership fee
  2. The piracy of pornography and distribution via peer-to-peer networks and forums
  3. and the burgeoning competition from amateur, DIY pornographers, many of which were able to provide bareback porn, being unrestrained by mainstream conventions.
With the advent of PrEP, however, gay porn studios suddenly had an out: they could provide bareback sex for audiences who preferred it while still bearing the mantle of "safer" sex. Most large gay pornographic studios began producing bareback porn in the years following the CDC's endorsement of Truvada as PrEP.

Proposal:
A descriptive(?) data analysis of gay male pornographic videos available online and their viewing trends over the past 5-10 years.

i. Obtaining the raw data
	i.1. Asking PornHub politely
    	i.1.a. Reached out via contact form on the PornHub Press page
      		-- NO RESPONSE
    	i.1.b. Reached out to @pornhub on twitter
    		-- NO RESPONSE
    	i.1.c. Reached out to a friend who works for MindGeek, the conglomerate that owns PornHub
      		-- AWAITING RESPONSE
  	i.2. Utilizing a scraper
    	i.2.a. porngram: https://github.com/sexualitics/porngram
    	i.2.b. https://github.com/gedankenstuecke/pOrNtology
    	i.2.c. https://www.pornhub.com/rss
		i.2.d. rvest https://github.com/hadley/rvest
		i.2.e. readlines http://www.programmingr.com/content/webscraping-using-readlines-and-rcurl/
	i.3. Contacting reddit user mrborgen86 who claimed on r/datasets the he has a (now defunct) database containing PornHub data
		-- AWAITING RESPONSE
  	i.4. Manually accessing gay porn on PornHub for data
  	i.5. Manually accessing mainstream gay porn sites for changes in their safer sex practices
  
ii. Data processing
  
iii. The data must be available to be made public by end of class
  	This one I am not so sure of, depending on the source. If I end up scraping the data, absolutely.

iv. You must specify your own question you are asking from the data
  	See above.

v. You need to provide reasonable justification you can answer that question with your data.
	v.1. Usage and search query data
    The most crucial data fields necessary for this analysis would be search term popularity over time, category/tags of film (as archived on the site), condom use: yes or no, popularity of films (views, likes, purchases), gender of viewer, age of viewer
  	v.2. Porno data
    The most crucial data fields necessary for this analysis would be date a film was posted, perhaps date a film was produced, condom use: yes or no, sales or viewing info (if available), number of sex performers in scene
  	v.3. STI time series data
    Easy enough.
