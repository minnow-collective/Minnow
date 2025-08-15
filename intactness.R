library(jsonlite)
library(stars)

bdi <- read_rds("/Volumes/OWC Elite Pro Dual/Data + Other/Minnow/intactness/long_data.rds")

geo1 <- fromJSON("/Volumes/OWC Elite Pro Dual/Data + Other/Minnow/intactness/ab956017cb24d56e580524dea97ee6590d46a14b/56b20e96-f0ac-49bc-a43e-d0ce52b13403.json")
geo2 <- fromJSON("/Volumes/OWC Elite Pro Dual/Data + Other/Minnow/intactness/b1abc3897c41647d4a5b5af3a588df31fcad3d09/61080b48-c50f-414c-8068-fb1bf2b68aff.json")

bii_rast <- rast("intactness/8161226fda7a36ca0b94f2becee9afe1b318d0a8/c4c281c4-befa-4e1b-a162-ba2f25e5ae82/bii-2020_v2-1-1.tif")

