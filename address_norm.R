### function to normalize address

address_norm = function(addr_vec, upcase=FALSE){
  ### Takes a concatenated list of addresses, returns normalized address columns
  ### Step one - no post codes
  ### do the normalization
  ### TODO Step two - recognize whether the address has a postcode and strip it or leave it
  addr_df_norm = data.frame(addr_vec, stringsAsFactors = FALSE) %>% mutate(addr_vec = enc2utf8(addr_vec), 
                                                                            addr_vec = iconv(addr_vec), 
                                                                            addr_vec = tolower(addr_vec), 
                                                                            addr_vec = trim_all(addr_vec))
  #data.frame(trim_all(tolower(iconv(enc2utf8(addr_vec)))))
  
  ### Remove city
  city_filter = "glasgow|glagsow|glsgow|glasgoq|gladgow|glagow|gasgow|glasagow|glasggow|glasgiw|glasgoe|glasgw|glashow|glasogw|glasow|glsagow|flasgow"
  city_select = city_filter
  addr_df_norm$working_addr = regex_filter_and_extract(addr_df_norm$addr_vec, city_filter, city_select)$replace_col
  
  ### House number & street leftover
  flat_position_filter = "(house [0-9\\/\\w\\.]+|flat [0-9\\/\\w\\.]+|site [0-9\\/\\w\\.]+|(flat[\\d]+))|(gr[\\s\\.]?[\\d+]?$)|(rm[\\s]?\\d+?$|room[\\s]?\\d+?$)|(\\d+\\/\\d+)|(\\w+\\/\\w+)|([0-9]+?[a-z]$)|( [\\w+]?[0-9]+$)"
  flat_position_select = flat_position_filter
  flat_cols = regex_filter_and_extract(addr_df_norm$working_addr, flat_position_filter, flat_position_select, 1)
  addr_df_norm$flat_position = flat_cols$new_col
  addr_df_norm$working_addr = flat_cols$replace_col
  addr_df_norm$working_addr = trim_all(addr_df_norm$working_addr)
  
  ### reapply to address cat
  addr_df_norm$working_addr = regex_filter_and_extract(addr_df_norm$working_addr, flat_position_filter, flat_position_select, 1)$replace_col
  
  ### split house_n from street
  street_n_filter = "^([\\w\\s]*?)([\\d]+[a-zA-Z]{1}|[\\d]+? [a-zA-Z]{1}|[\\d]{1,4}) ([^0-9]*)$"
  street_n_select = street_n_filter
  # street_n_cols = regex_filter_and_extract(llr_live_processed$street_address, street_n_filter, street_n_select, 3)
  street_n_cols = str_match(addr_df_norm$working_addr, street_n_filter)
  
  ### Any in the match that aren't na
  street_match_rows = which(!is.na(street_n_cols[,1]))
  
  ### Add street address and house number & leftover to data frame
  addr_df_norm[,c("street_name", "house_n", "addr_other", "house_alpha")] = NA
  addr_df_norm[street_match_rows,]$street_name = street_n_cols[street_match_rows,4]
  addr_df_norm[street_match_rows,]$house_n = street_n_cols[street_match_rows, 3]
  addr_df_norm[street_match_rows,]$addr_other = street_n_cols[street_match_rows, 2]
  
  ### Split house_n alpha
  house_alpha_filter = "([0-9]+)([\\s]?)([a-z]?)$"
  house_alpha_cols = str_match(addr_df_norm[,]$house_n, house_alpha_filter)
  addr_df_norm[,c("house_n", "house_alpha")] = house_alpha_cols[,c(2,4)]
  
  ### add flat in front of any flat_positions without a word before the slashes
  naked_flat_n_rows = which(!is.na(str_match(addr_df_norm$flat_position, "^[\\w]+?[/][\\w]+?$")))
  addr_df_norm[naked_flat_n_rows,"flat_position"] = paste("flat", addr_df_norm[naked_flat_n_rows, "flat_position"])
  
  ### Put them back to upper case word beginnings
  if(upcase == TRUE){
    addr_df_norm = data.frame(lapply(addr_df_norm[,c("street_name", "addr_other")], function(x) gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x, perl=TRUE)), stringsAsFactors = FALSE)
  }
  
  ### remove NA's
  addr_df_norm[is.na(addr_df_norm)] = ""
  
  ### reorder
  addr_df_norm = addr_df_norm %>% select(addr_vec, flat_position, house_n, house_alpha, street_name, addr_other)
  
  ### Rename columns
  addr_df_norm = addr_df_norm %>% rename(original_address = addr_vec)
  
  return(addr_df_norm)
  
}