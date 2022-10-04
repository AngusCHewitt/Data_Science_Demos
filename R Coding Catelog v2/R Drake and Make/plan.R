


# setup dev/prod distiiction around operating environemnt
operating_environment='dev'
source('local_operating_environment.R' )  # will set operating environment to 'prod' if necessary and in correct folder


if ( operating_environment=='dev' ) {
  target_email = "dennis.wollersheim@dhhs.vic.gov.au"
} else {
  target_email = c("greg.dalton@dhhs.vic.gov.au", "dennis.wollersheim@dhhs.vic.gov.au")
}

report_date = lubridate::today() - 1
#if(operating_environment == 'dev' ) report_date = as.Date('2020-08-04')
#if(operating_environment == 'dev' ) report_date = as.Date('2020-11-07')

base_dir = '../onedrive/OneDrive - Department of Health and Human Services. Victoria/Paxton WIP/'
output_dir = dplyr::case_when(
                       operating_environment == 'prod' ~   base_dir,
                       operating_environment == 'dev' ~  paste0( base_dir, 'post_paxton/output_drake/'))

#' Setup file locations
#E:\PBIX\NCoronavirus 2020\Stata nCoV reporting\31 Azure Data Model\DART\Data snapshots\sitrep\
data_folder = "E:/PBIX/NCoronavirus 2020/Stata nCoV reporting/31 Azure Data Model/DART/Data snapshots/sitrep/"
#E:\PBIX\NCoronavirus 2020\Stata nCoV reporting\20 Outbreaks\Automated outbreak report with R\linelist
data_folder_ob = "E:/PBIX/NCoronavirus 2020/Stata nCoV reporting/20 Outbreaks/Automated outbreak report with R/linelist/"

data_update_time=''
# Make sure the data is current
status_file_name =  paste0( data_folder, 'status.txt')
if (!file_exists( status_file_name )) {
  warning('Missing status file: status.txt' )
} else {
  read_csv( status_file_name ) %>%
    mutate( currentTime = parse_date_time(currentTime, 'dmy hMS')) %>%
    pull( currentTime ) %>%
    { . } -> data_update_time
}

# Set threshold to exclude low number suburbs from maps
show_threshold = 0
show_threshold_text = ifelse( show_threshold > 0, paste("more than",show_threshold), "")



the_plan =
  drake_plan(
             #do all dan's work because why not?
#             dans_daily_script = target( daily_script( output_dir, operating_environment),
#                                 trigger=trigger( change=data_update_time )
#                                 ),

             ################################################################################
             # Read in daily substantive data
             ################################################################################


             df_clustersites = target(
                                      data.table::fread(paste0(data_folder, "clustersites.txt"))  %>% as_tibble(),
                                      trigger=trigger( change=data_update_time )
                                      ),



             df_outbreaks =
               target( data.table::fread(paste0(data_folder_ob, "outbreak_linelist_long.csv"))  %>%
                      as_tibble() %>%
                      inner_join( df_clustersites , suffix=c('', '.todelete'),by=c('cluster_RecordID' = 'ClusterID')) %>%
                      select( -ends_with('.todelete'))),



             persons    = target(
                                 data.table::fread(paste0(data_folder, "Persons.txt")) %>%
                                   select(RecordID, DateOfBirth, Age, AgeGroup, Sex, LGA, State, MetroRural,
                                          AddressLine1, Suburb, Postcode) %>%
                                   mutate(LGA = standardise_lgas(LGA),
                                          Postcode = as.character( Postcode )) %>%
                                 as_tibble(),
                               trigger=trigger( change=data_update_time )
                               ),

             linelist   = target(
                                 data.table::fread(paste0(data_folder, "LineList.txt")) %>%
                                   mutate(
                                          across(  ends_with('Date'), dmy),
                                          LGA = standardise_lgas(LGA),
                                          Postcode = as.character( Postcode )
                                          ) %>%
                                 select( RecordID, LGA, Sex, Postcode, Classification, ends_with('Date'),
                                        CountryOfBirth, Acquired, AgeAtOnset, MetroRural,
                                        HCWFlag, State) %>%
                                 as_tibble()
                               ,
                               trigger=trigger( change=data_update_time )
                               ),

             confirmed  = get_confirmed( linelist, df_outbreaks, lga_lookup ) ,

             confirmed_l2w = confirmed %>%
               filter( is_last_2_weeks( DiagnosisDate, report_date )),

             # last 2 weeks, ending 1 week ago
             confirmed_l2w.m7 = confirmed %>%
               filter( is_last_n_days( DiagnosisDate, report_date-7, n=14 )),

             confirmed_l2w_local = confirmed_l2w %>%
               filter( Acquired != 'Travel overseas'),

             confirmed_l2w_local.m7 = confirmed_l2w.m7 %>%
               filter( Acquired != 'Travel overseas'),

             ## daily testing
             ################################################################################

             lab_results = target(
                                  data.table::fread(paste0(data_folder, "labresults.txt")) %>%
                                    as_tibble() %>%
                                    select(SignatureDate, TestDate, RecordID, TestTrackerId) %>%
                                    filter( TestDate <= report_date ) %>%
                                    mutate( across(  ends_with('Date'), dmy)),
                                  trigger=trigger( change=data_update_time )
                                  ) ,

             tests = target( get_tests( lab_results, persons ),
                            trigger=trigger( change=data_update_time )
                            ),


             test_tracker = target( get_test_tracker(),
                                   trigger=trigger( change=data_update_time )
                                   ),

             tt_l2w = test_tracker %>%
               filter(is_last_2_weeks( Date , report_date ) ) ,

             ################################################################################
             # Standard Lookups
             ################################################################################

             #' Making a dataframe that has mapping between postcode, LGA, LGA population, LGA name/cat etc.
             region_lookup = read_csv(file_in( "data/Region_lookup.csv")) %>%
                filter( LGA != 'Interstate' & LGA != 'Victoria')  %>%
                mutate(LGA = standardise_lgas(LGA)) ,

             postcode_mapping = read_csv(file_in( "data/Postcode_LGA_mapping_maxarea.csv")) %>%
               set_names( c( 'Postcode', 'LGA_Code', 'LGA')) %>%
               mutate( Postcode = as.character( Postcode )) %>%
                mutate(LGA = standardise_lgas(LGA)) ,

              postcode_population = read_csv(file_in( "data/postcode.csv")) %>%
                clean_names() %>%
                drop_na( postcode) %>%
                select( postcode, starts_with( 'population'))  %>%
                set_names(c("Postcode", "Population")) %>%
                mutate( Postcode = as.character( Postcode ) ) ,


             SEIFA_lookup = read_csv(file_in( "data/SEIFA_postcode.csv")),

             SEIFA_deciles =
               readxl::read_xlsx(file_in( "data/SEIFA_deciles.xlsx")) %>%
               mutate(Prop = Pop/sum(Pop)),

             ABS_agesex =
               read_xlsx(file_in( "data/ABS_agesex.xlsx")),

             postcode_lookup =
               left_join(postcode_mapping, region_lookup, by = 'LGA') %>%
               rename( LGA_Population = Population) %>%
               inner_join( postcode_population, by='Postcode'),

             lga_lookup =
               postcode_lookup  %>%
               distinct( LGA_Code, LGA, Restriction, LGA_Population ) %>%
               rename( Population = LGA_Population ) %>%
               mutate(  MetroRural = ifelse( LGA %in% melb_lga, 'Metro', 'Rural')),

              ################################################################################
             # Testing
              ################################################################################
             # ---- Test results dataset #1: lab results ---------------------------
             # Lab results from the last 2 weeks:
             lr_l2w =  tests  %>%
               filter(is_last_2_weeks( TestDate , report_date ) ),

             lr_l1w =  tests  %>%
              filter( is_last_n_days( event_date=TestDate, report_date =report_date , n=7 )),

             lga_testing_rates_l2w =
               lr_l2w %>%
               count(LGA, name='Count')   %>%
               left_join( lga_lookup , by = "LGA") %>%
               filter( !is.na(Restriction)) %>%
               mutate(PropPer100k = (Count/Population)*100000),

             lga_testing_rates_l1w =
               lr_l1w %>%
               count(LGA, name='Count')   %>%
               left_join( lga_lookup , by = "LGA") %>%
               filter( !is.na(Restriction)) %>%
               mutate(PropPer100k = (Count/Population)*100000),


             ################################################################################
             # Mapping - generic
             ################################################################################

             #base OSM map, for use in tmap
             basemap_state = readRDS(file_in( 'data/VIC_osm_bw.rds')),

             # Load in shapefile
             lga_base_poly =
               st_read(file_in( "data/lga_2020_aust_shp/LGA_2020_AUST.shp")),

             # modify shapefile
             lga_testing_poly =
               lga_base_poly %>%
               st_transform(crs = 4326) %>%
               filter( STE_NAME16 == "Victoria") %>%
               mutate(LGA_NAME20= standardise_lgas(LGA_NAME20)) %>%
               left_join( lga_testing_rates_l2w, by=c("LGA_NAME20"="LGA")),


             ################################################################################
             # Mapping - testing
             ################################################################################

             testing_map =
               get_lga_testing_map( lga_testing_poly, basemap_state)
             ,


             ################################################################################
             # make EpiDeck
             ################################################################################


             epideck =  make_epideck(confirmed,
                                     confirmed_l2w,
                                     tt_l2w,
                                     tests,
                                     lr_l2w,
                                     lga_testing_rates_l1w,
                                     lga_testing_rates_l2w,
                                     region_lookup ,
                                     testing_map,
                                     SEIFA_lookup,
                                     SEIFA_deciles,
                                     ABS_agesex) ,

             epideck_supplementary = make_epideck_supplementary( postcode_lookup,
                                                                region_lookup,
                                                                lga_lookup,
                                                                N_Outb_Postcode_cases,
                                                                Un_Postcode_cases,
                                                                confirmed,
                                                                confirmed_l2w,
                                                                confirmed_l2w_local,
                                                                confirmed_l2w_local.m7,
                                                                lga_cases,
                                                                lga_metro_sf,
                                                                lga_testing_rates_l1w,
                                                                lga_testing_rates_l2w,
                                                                lga_victoria_sf,
                                                                lr_l2w,
                                                                postcode_cases,
                                                                postcode_metro_sf,
                                                                postcode_victoria_sf,
                                                                toner_basemap_metro,
                                                                toner_basemap_victoria,
                                                                tests,
                                                                df_outbreaks,
                                                                show_threshold
                                                                ),

              email_success = send_success_email( to=target_email,
                                                 subject = glue('Epideck build success for {report_date}'),
                                                 attachment=c(epideck, epideck_supplementary),
                                                 body='Dear Colleagues\n\n The epideck has built successfully, and is attached.\n
                                                 Please do not hesitate to get in touch with any feedback or queries via dennis.wollersheim@dh.vic.gov.au\n\nKind regards,\nDennis '),

             ################################################################################
             # Mapping - replicaing Dan's maps
             ################################################################################

# make generic map sf objects
             map_nsw_red_zone = make_map_nsw_red_zone(),

             # Mapping data by LGAs
             lga_victoria_sf = lga2018 %>%
               st_transform(crs = 4326) %>%
               filter(state_name_2016 == "Victoria") %>%
               mutate(lga = standardise_lgas(lga_name_2018)) %>%
               select(lga, cent_long, cent_lat),

             lga_victoria_boundary = lga_victoria_sf %>% summarise(),

             lga_metro_sf = lga_victoria_sf %>%
               filter( lga %in% melb_lga),

             lga_metro_boundary = lga_metro_sf %>% summarise(),


             # Postcodes within the Victoria LGA boundary
             postcode_victoria_sf =
               postcode2016 %>%
               st_transform(crs = 4326) %>%
               st_filter(lga_victoria_boundary) %>%
               rename(postcode = postcode_2016) ,

             # Postcodes within the Melbourne LGA boundary
             postcode_metro_sf =
               postcode2016 %>%
               st_transform(crs = 4326) %>%
               st_filter(lga_metro_boundary) %>%
               rename(postcode = postcode_2016),

             # summarise last two week case data
#------------------------------------------------------------------------
             lga_cases = confirmed_l2w_local %>%
               count(LGA, name='Cases'),

             postcode_cases = confirmed_l2w_local %>%
               count(VIC_Postcode, name='Cases'),

             # summarise last two week Non-Outbreak unknown case data
#------------------------------------------------------------------------
             Un_LGA_cases =  confirmed_l2w_local %>%
               filter(Acquired=="Acquired in Australia, unknown source")%>%
               count(LGA, name='Cases'),

             Un_Postcode_cases =  confirmed_l2w_local %>%
               filter(Acquired=="Acquired in Australia, unknown source")%>%
               count(VIC_Postcode, name='Cases'),

             # summarise last two week Outbreak case data
#------------------------------------------------------------------------
             N_Outb_Postcode_cases =  confirmed_l2w_local %>%
               filter(is.na(outbreak))%>%
               count(VIC_Postcode, name='Cases'),

             N_Outb_LGA_cases =  confirmed_l2w_local %>%
               filter(is.na(outbreak))%>%
               count(LGA, name='Cases'),
#------------------------------------------------------------------------

             # Dan's mapping - prepare base maps
             victoria_bbox = getbb("Victoria, Australia"),
             melb_bbox  =  structure(c(144, -38.36, 145.9, -37.2),
                                    .Dim = c(2L, 2L),
                                    .Dimnames = list( c("x", "y"), c("min", "max"))),


             toner_basemap_victoria = get_map(victoria_bbox, maptype = "toner", color = "bw", zoom = 05)  %>%
               ggmap() +
               geom_sf(data = lga_victoria_boundary,
                       inherit.aes = FALSE,
                       aes(geometry = geometry),
                       fill = NA, colour = "black"),

             toner_basemap_metro = get_map(melb_bbox, maptype = "toner", color = "bw", zoom = 09)  %>%
               ggmap() +
               # Add Melbourne LGA boundary
               geom_sf(data = lga_metro_boundary,
                       inherit.aes = FALSE,
                       aes(geometry = geometry),
                       fill = NA, colour = "black"),

#             maps = make_all_maps ( lga_victoria_sf, postcode_victoria_sf, toner_basemap_victoria,
#                                   lga_metro_sf, postcode_metro_sf, toner_basemap_metro,
#                                   lga_cases, postcode_cases, Un_Postcode_cases, N_Outb_Postcode_cases,
#                                   confirmed_l2w_local, df_conf_outb_linelist, show_threshold ) ,



             0
  )
