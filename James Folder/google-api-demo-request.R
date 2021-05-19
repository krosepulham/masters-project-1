# Goal: Demo how to send an API call to Google's Geocode service
# and receive data back from the call.

# Next steps:
# Make these requests programatically but in a way that we (1) 
# don't get banned by google for making too many requests too 
# quickly and (2) don't over-run our free credit limit

# Libraries (We'll need httr to make and access requests). We'll
# need jsonlite to turn the JSON data into something we can more
# easily access in R. There's also ways to get around the need
# of using jsonlite which I'll show below as well.
library(httr)
library(jsonlite)

# Step 1: Register for a Google Cloud Service API key and sign
# up for their geocoding service. You'll likely need to hand them
# a credit card number to access the API ... which I personally
# feel is not ideal. However, I think there's strategies we can
# employ so that you don't end up with a huge bill by mistake.
# 
# For instance, we can prototype making a single call at a time,
# which is what I do below. We can then scale this up to making
# 3 or 4 requests using location from our data. Finally, we 
# can let our code rip on all 10,000 unique locations (again,
# making sure we 'pause' the script after each request at least
# 0.02 seconds so we don't get Google mad at us.)
#
# Just be careful and keep that API key private!

# Once you have your key, paste it below.
my_api_key <- "<Your API key>"


# In the meantime, I'm  going to provide a Google API request 
# that I made with my API key which you can load below:
load(file = "James Folder/google-api-location-request.Rda")


# Our "address" requests I think have to be formatted in a
# specific way to form a valid URL. So here, I take Google's 
# default example address ...
request <- "1600 Amphitheatre Parkway, Mountain View, CA"

# And replace the spaces "\\s", with plus signs.
formatted_request <- gsub("\\s", "+", request)

# Now that we have an api key and a formatted request, we put
# our request URL together.
request_url <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=",
                      formatted_request,
                      "&key=",
                      my_api_key)

# This should look like a "URL"
request_url

# Next, we use HTTR to send a "GET" request to the Google API.
# Basically, "Visit this URL and 'GET' me what I'm requesting.
# (It's this function call where we're calling the API and where 
# google begins tracking our API usage.)
location_request <- GET(request_url)

# Again, just in case, here's the location_request I obtained
# with my API key (Since some of the things above won't work 
# without an API key of your own.)
load(file = "James Folder/google-api-location-request.Rda")

# We end up getting a LOT of data from our GET request. Too much
# in fact. We just want the content of the call. So, we use HTTR's
# content function to get it and, in this case, deliver it to us
# as "text". If we use "text", then we can convert the text into 
# an R data thing since the "text" is just JSON.
location_content <- content(location_request, as = "text")
location_json <- fromJSON(location_content)

# Notice here that location_json is not a data.frame (yet) since
# it has our data AND the status of the call

# So here's the status:
location_json[[2]]

# And here's the data:
location_json[[1]]

# Our search returned two locations and so Google gave us details
# for both. I'm thinking the first  location is the "best match"
# but that's a hell of an assumption ...
# anywho, the lat/lng of our address is
location_json[[1]]$geometry$location$lng[1]
location_json[[1]]$geometry$location$lat[1]


# Finally, I think we can avoid the need for JSON if we access
# the content using 'as = "parsed"'.
location_content_parsed <- content(location_request, as = "parsed")

# It seems like this is a bit "cleaner" in my opinion.
location_content_parsed

# I can find out how many results were returned with my search:
length(location_content_parsed) # 2 in this case

# I can grab the info from the first result:
location_content_parsed$results[[1]]$geometry$location$lng
location_content_parsed$results[[1]]$geometry$location$lat
