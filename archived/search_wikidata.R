
# List of composers or musicians
names <- c("Gerrit Jan van Eijken")
name <- "Gerrit Jan van Eijken"
lang <- "nl"

# Apply the function to each name
wikipedia_links <- map_chr(names, get_wikipedia_url)

# Output results
tibble(Name = names, Wikipedia_URL = wikipedia_links)
