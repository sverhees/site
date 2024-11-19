
library(lingtypology)

coord <- read_tsv("botlikhhood.csv")

map.feature("Botlikh",
            color = "mediumvioletred",
            width = 8,
            zoom.level = 7,
            tile = c("Esri.WorldGrayCanvas"))

map.feature(coord$lang,
            latitude = coord$lat,
            longitude = coord$lon,
            features = coord$lang,
            label = coord$label,
            label.hide = FALSE,
            color = "magma",
            width = 8,
            tile = c("Esri.WorldGrayCanvas"))


