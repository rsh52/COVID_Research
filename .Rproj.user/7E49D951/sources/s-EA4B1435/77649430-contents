
# The following data comes from OpenDataPhilly.org
# library(httr)
# set_config(config(ssl_verifypeer = 0L))

# Dataset load
philly.gv_URL <- "https://phl.carto.com/api/v2/sql?q=SELECT+*,+ST_Y(the_geom)+AS+lat,+ST_X(the_geom)+AS+lng+FROM+shootings&filename=shootings&format=csv&skipfields=cartodb_id"
# philly.gv <- download.file(url = philly.gv_URL, )
# philly.gv <- getURL(philly.gv_URL)
philly.gv <- read_csv(philly.gv_URL)
philly.gv$date_ <- ymd(philly.gv$date_) # Convert date to proper format

philly.gv$lng <- round(philly.gv$lng,2) # Round longitude and latitude to 2 digits
philly.gv$lat <- round(philly.gv$lat,2)


# Create a continuous palette function for future coloring
pal <- colorNumeric(
  palette = "magma",
  domain = philly.gv$n
)
