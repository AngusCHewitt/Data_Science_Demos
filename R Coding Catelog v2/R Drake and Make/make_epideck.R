if (FALSE) {

  loadd(confirmed)
  loadd(confirmed_l2w)
  loadd( tt_l2w)
  loadd( tests)
  loadd( lr_l2w)
  loadd( lga_testing_rates_l2w)
  loadd( lga_testing_rates_l1w)
  loadd( region_lookup )
  loadd( testing_map)
  loadd( SEIFA_lookup)
  loadd( SEIFA_deciles)
  loadd( ABS_agesex)

}

make_epideck = function(confirmed,
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
                        ABS_agesex ) {
  #

  #
  ppt_template_dir = paste0( base_dir, '/post_paxton/templates/' )
  ppt_template =  paste0( ppt_template_dir, 'epideck_template.pptx')
  ppt_output =  glue::glue( "{output_dir}/DEW_testing_slides-{report_date}_{operating_environment}.pptx")
  epi_curve = plot_epi_curve( confirmed, report_date, ndays=30 )

  ################################################################################
  #


  read_pptx(ppt_template) %>%
    add_title_page( report_date ) %>%
    update_page_cases_14_days( confirmed, confirmed_l2w,  report_date ) %>%
    update_page_testing_map( lga_testing_rates_l2w, testing_map ) %>%
    update_page_testing_rate( confirmed, tests, region_lookup,lga_testing_rates_l1w, report_date) %>%
    update_page_testing_demographics( SEIFA_lookup, SEIFA_deciles, lr_l2w , ABS_agesex) %>%
    update_page_testing_delay( tt_l2w, lr_l2w ) %>%
    add_single_ppt_ggplot( title = "Overall Epidemic Curve - diagnosis date",
                        plot=  epi_curve ) %>%
    print( ppt_output )

  ppt_output
}
