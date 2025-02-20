
# row names to look for in the excel input data 

# station information section
.row_name_project  <- "Prosjekt"
.row_name_stn_code <- "Stasjonskode"
.row_name_old_code <- "Gammel kode"
.row_name_stn_name <- "Stasjonsnavn"
.row_name_date     <- "Dato (tid)"
.row_name_observer <- "Observatør"
.row_name_recorder <- "Skriver"
.row_name_coord_type <- "Koordinattype/sone"
.row_name_long     <- "Lengdegrad"
.row_name_lat      <- "Breddegrad"
.row_name_water_level <- "Vannstand over lavvann"
.row_name_time_low <- "Tid for lavvann"

# species recorded section
.row_name_species_header <- "Kode"

# adjustment of point scores
.points_adjust_NO <-  3

# shore description 
.row_name_shore_desc     <- "Beskrivelse av  fjæra (0=nei, 1=ja)"
.row_name_turbid_water   <- "Turbid vann"
.row_name_sand_scouring  <- "Sandskuring"
.row_name_ice_scouring   <- "Isskuring"
.note_turbid_water       <- "(ikke antropogent)"


.points_turbid_water     <- c(2,0) # 2 for FALSE, 0 for TRUE 
.points_sand_scouring    <- c(2,0)
.points_ice_scouring     <- c(2,0)

# dominant shore type
.row_name_dominant_shore <- "Dominerende fjæretype (Habitat)"
.row_name_ravine         <- "Små kløfter/sterkt oppsprukket fjell/overheng/Plattformer"
.row_name_fractured      <- "Oppsprukket fjell"
.row_name_boulders       <- "Små, middels og store kampestein"
.row_name_steep          <- "Bratt/Vertikalt fjell"
.row_name_unspec_hard    <- "Uspesifisert hardt substrat/glatt fjell"
.row_name_rocks          <- "Små og store steiner"
.row_name_shingle        <- "Singel/Grus"

.points_ravine         <- 4
.points_fractured      <- 3
.points_boulders       <- 3
.points_steep          <- 2
.points_unspec_hard    <- 2
.points_rocks          <- 1
.points_shingle        <- 0

# dominant shore types
.row_name_other_shore    <- "Andre fjæretyper (Subhabitat)"
.row_name_shallow_pool   <- "Brede, grunne Fjærepytter"
.row_name_large_pool     <- "Store fjærepytter"
.row_name_deep_pool      <- "Dype fjærepytter"
.row_name_small_pool     <- "Mindre fjærepytter"
.row_name_cave           <- "Store huler"
.row_name_overhang       <- "Større overheng og vertikalt fjell"
.row_name_other_sub      <- "Andre subhabitat-typer"

.note_shallow_pool       <- "(Rockpools) (>3m bred og <50cm dyp)"
.note_large_pool         <- "(>6m long)"
.note_deep_pool          <- "(50% >100cm dyp)"

.points_shallow_pool   <- 4
.points_large_pool     <- 4
.points_deep_pool      <- 4
.points_small_pool     <- 3
.points_cave           <- 3
.points_overhang       <- 2
.points_other_sub      <- 2



