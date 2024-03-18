# get the model and known fish rearing/spawning columns
fpr_db_query(query = fpr_dbq_lscols(schema = "bcfishpass", table = "streams_vw")) %>%
  dplyr::filter(str_detect(column_name, "^model_|^known_")) %>%
  select(column_name) %>%
  write_csv("temp/streams_vw_model_known_cols.csv")


# check this locally
fpr_db_query(query = ldbq_chk_null("whse_wildlife_management.wcp_fish_sensitive_ws_poly", "gazetted_name"),
             db_var = 'newgraph',
             host_var = Sys.getenv("PG_HOST_LOCAL"),
             port_var = Sys.getenv("PG_PORT_SHARE"),
             user_var = Sys.getenv("PG_USER"),
             password_var = Sys.getenv("PG_PASS")
             )
