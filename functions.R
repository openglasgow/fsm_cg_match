## trims whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
trim_internal = function(x) gsub("\\s+", " ", x)
trim_leading_trailing_punctuation = function(x) gsub("^[,\\.()]+|[,\\.()]+$", "", x)
trim_all = function(x){
  trimmed_punctuation = trim_leading_trailing_punctuation(x)
  trimmed_external = trim(trimmed_punctuation)
  trimmed_internal = trim_internal(trimmed_external)
  return(trimmed_internal)
}

### NA functions
empty_to_na = function(x) gsub("^$|^ $|^[\\s]+$", NA, x)

## currently doesn't work - caching the function on subsequent iterations?
iterative_benchmark = function(iterations, fun){
  total_time = 0
  for(i in 1:iterations){
    t = system.time(fun)
    total_time = total_time + t[[3]]
    print(total_time)
    print(t)
  }
  return(total_time)
}

extract_titles = function(name_strings){
  patterns = "ms|miss|mrs|mr|master|ref|fr|dr|atty|prof|hon|pres|gov|coach|ofc|unknown|ms\\.|miss\\.|mrs\\.|mr\\.|master\\.|ref\\.|fr\\.|dr\\.|atty\\.|prof\\.|hon\\.|pres\\.|gov\\.|coach\\.|ofc\\."
  titles = str_match(name_strings, patterns)
  return(titles)
}

strip_titles = function(name_strings){
  patterns = "ms|miss|mrs|mr|master|ref|fr|dr|atty|prof|hon|pres|gov|coach|ofc|ms\\.|miss\\.|mrs\\.|mr\\.|master\\.|ref\\.|fr\\.|dr\\.|atty\\.|prof\\.|hon\\.|pres\\.|gov\\.|coach\\.|ofc\\."
  scrubbed_names = trim_all(gsub(patterns, "", name_strings, ignore.case=TRUE))
  return(scrubbed_names)
}

top_tail_names = function(name_strings){
  matches = str_match(name_strings, "(^['-[\\w]]+)(.+?)(['-[\\w]]+$)")
  matches[,3] = trim_all(matches[,3])
  return(matches[,2:4])
}

### Convert excel dates back to flat numbers
excel_date_convert = function(strings){
  ### setup
  conversion = list(jan = 1, feb=2, mar=3, apr=4, may=5, jun=6,jul=7, aug=8,sept=9,oct=10,nov=11,dec=12)
  indexes = grep("[0-9]{2}-[a-zA-Z]{3}", strings)
  working_strings = strings[indexes]
  
  ### Iterate through the slices with dates in them 
  converted_strings = unname(sapply(working_strings, function(x){
    excel_date = str_match(x, "[0-9]{2}-[a-zA-Z]{3}")[1]
    converted = paste(gsub("0","", as.vector(str_match(excel_date, "[0-9]{2}"))), "/", conversion[as.vector(str_match(excel_date, "[a-z]+"))], sep="")
    gsub(excel_date, converted, x)
  }))
  
  ### Replace in the original list
  strings[indexes] = converted_strings
  
  ### Return 
  return(strings)
}

### Filters and extracts patterns in a dataset
regex_filter_and_extract = function(char_vector=c("a", "b", "c"), filter=".*", select=".*", select_groups = FALSE){
  ### Takes a character vector, a regex to filter the vector for what you are looking for - only those rows are operated on
  ### A select regex is then used to choose what string you want to select
  ### If you have multiple groups in the select regex, you can choose which ones you want from the groups with 'select_groups' - a numeric vector
  ### The matching rows are returned with the full original length of the input, with NA's filling the positions the filter regex did not match. 
  
  match_rows = which(!is.na(str_match(char_vector, filter)[,1]))
  if(select_groups == FALSE){
    selection = str_match(char_vector[match_rows], select)
  } else {
    selection = str_match(char_vector[match_rows], select)[,select_groups]
  }
  new_col = rep(NA, length(char_vector))
  new_col[match_rows] = selection
  char_vector[match_rows] = apply(data.frame(char_vector, new_col), 1, function(x) {regex_remove_pattern(x[1], x[2])})[match_rows]
  
  
  return(list(replace_col=char_vector, new_col=new_col))
}

### remove pattern from string
regex_remove_pattern = function(string, pattern){
  return(trimws(gsub(pattern, "", string)))
}


## Rounds up (badly -fix this)
roundUp <- function(x) 10^ceiling(log10(x))

### Rescales a numeric vector linearly into categories for charting purposes
linMap = function(x, from, to) {
  (x - min(x)) / max(x - min(x)) * (to - from) + from
}
  
### string normalize datasets as they're imported - requires stringsAsFactors = FALSE
import_string_normalize = function(df){
  
  ### discover character cols
  str_cols = unname(which(lapply(df, function(x) is.character(x)) == TRUE))
  
  if(length(str_cols) == 0){
    return(df)
  } else {
    ### Trim and tolower them
    df[,str_cols] = data.frame(lapply(df[,str_cols], function(x) trim_all(tolower(iconv(enc2utf8(x))))), stringsAsFactors = FALSE)
    
  }
  ###return the df
  return(df)
}

## Function that populates a new column with a standard score
## based the frequency or sum of a certain attribute against a category.
category_to_value = function(df, cat_col, var_col=NULL, new_col_name, scale_data=TRUE, type="sum"){
  
  if(type == "sum") {
    
    cat_table = aggregate(df[,var_col], by=list(cat_col = df[,cat_col]), FUN=eval(type))
    
  } else if (type == "freq") {
    
    cat_table = data.frame(table(df[,cat_col]))
    colnames(cat_table) = c("cat_col", "x")
  }
  if(scale_data == TRUE)   {
    cat_table$x = scale(cat_table$x)
  }
  
  df[,new_col_name] = cat_table$x[match(df[,cat_col], cat_table$cat_col)]
  
  return(df)
}

## create a matrix of aggregated columns by category, choose your columns, and what function to aggregate with
create_aggregate_matrix = function(dataframe, comparison_col, aggregate_columns=c(), func="sum") {
  
  aggregate_matrix = data.frame(table(dataframe[,comparison_col]))
  
  for (i in aggregate_columns) {
    aggregate_matrix = merge(aggregate_matrix, setNames(aggregate(dataframe[,i], by=list(Var1 = dataframe[,comparison_col]), FUN=eval(func)), c("Var1", i)), by="Var1")
  }
  
  #Set column names
  colnames(aggregate_matrix) = c(c(comparison_col,"count"), aggregate_columns )
  return(aggregate_matrix)
}

## Compare two factors in a df
table_comparison = function(df, col1, col2) {
  tab = data.frame(prop.table(table(df[,col1], df[, col2])))
  colnames(tab) = c(col1, col2, "Freq")
  
  return(tab)
}

## Generic function to match an existing table of values and fill a data frame with its contents - NOTE - both df's have to have the same match colname"
match_and_populate= function(df, match_df, match_col, fill_col, new_col_name){
  df[,new_col_name] = match_df[,fill_col][match(df[,match_col], match_df[,match_col])]
  return(df)
}

## Add custom metadata to a new table
custom_table = function(df, table_col, new_col_name, new_col_dat, scale_dat=FALSE){
  df_table = data.frame(table(df[,table_col]))
  if(scale_dat==TRUE){
    new_col_dat = scale(new_col_dat)
  }
  df_table[,new_col_name] = new_col_dat
  colnames(df_table) = c(table_col, "count", new_col_name)
  
  return(df_table)
}


## Partition dataset into train and test
partition_df = function(df, set="train", seed = 123, train_size = 0.75){
  
  ## Set train size
  smp_size = floor(train_size * nrow(df))
  
  ## Set seed to make it reproduceable
  set.seed(seed)
  train_ind = sample(seq_len(nrow(df)), size = smp_size)
  
  train=df[train_ind,]
  test =df[-train_ind,]
  
  if (set == "train"){
    return(train)
  } else if (set == "test") {
    return(test)
  }
  
}

### Data integrity check
### Returns a table that shows the number of completed records for each column
data_integrity_check = function(dataframe, by_col=FALSE){
  if(by_col==FALSE){
    di_check = data.frame(colname = colnames(dataframe), records = NA, percent=NA)
    for (col in colnames(dataframe)){
      di_check[di_check$colname == col, ]$records = nrow(dataframe[!is.na(dataframe[,col]),])
      di_check[di_check$colname == col, ]$percent = round(nrow(dataframe[!is.na(dataframe[,col]),])/nrow(dataframe)*100, 2)
      
    }
    return(di_check[rev(order(di_check$percent)),])
  } else {
    ## Split factor records into columns with one to list the original column names
    ## and place the column names in the df
    di_cols = unique(dataframe[,by_col])
    di_cols = c("column_name", di_cols)
    di_check = data.frame(matrix(ncol=length(di_cols), nrow=length(colnames(dataframe))))
    colnames(di_check) = as.list(di_cols)
    df_cols = colnames(dataframe)
    di_check$column_name = df_cols
    
    ## Start this mad iteration
    for (di_col in colnames(di_check[,2:length(colnames(di_check))])) {
      ## di_check iteration
      ## slice by current col and then iterate over every column in original df
      slice = dataframe[dataframe[,by_col] == di_col,]
      
      ## Dream within a dream - count NA in each DF col and place it in the DI place
      for (df_col in df_cols) {
        
        has_data = length(na.omit(slice[,df_col]))
        if(has_data != 0){
          no_data = has_data / nrow(slice) 
        } else {
          no_data = 0
        }
        di_check[di_check[,"column_name"] == df_col,di_col] = round(no_data, 2)
      }
    }
    return(di_check)
  }
  
}

### store normalization scale values
store_scale_values = function(df, cols = c()){
  
  df_attrs = data.frame(col_scaled = cols, scale=NA, center=NA)
  for(col in cols){
    scale.center = c(attr(scale(df[,col]), "scaled:scale"), attr(scale(df[,col]), "scaled:center"))
    df_attrs[df_attrs$col_scaled == col,]$scale = scale.center[1]
    df_attrs[df_attrs$col_scaled == col,]$center = scale.center[2]
  }
  return(df_attrs)
}

### Cast many rows into single row with group members
cast_long_df = function(df, uid, cast_col, cast_prefix){
  
  ## Trim to only include needed columns  
  df_trimmed = df[, c(uid, cast_col)]
  ## Remove empty rows
  df_trimmed = na.omit(df_trimmed)
  ## Convert to data table
  setDT(df_trimmed)
  ## New column with unique group count
  df_trimmed[, N:= 1:.N, by = uid]
  ## Cast the data to rows
  df_trimmed = dcast(df_trimmed, get(uid)~paste(cast_prefix, N, sep=""), value.var=cast_col, sep="")
  ## Rename uid to original name
  colnames(df_trimmed)[1] = uid
  ## Return these to original df
  df = merge(df, df_trimmed, by=uid)
  ## Remove original single column
  df[,c(cast_col)] = NULL
  ## Remove duplicate entries
  df = df[!duplicated(df[,uid]),]
  ## Return df
  return(df)
}

melt_wide_df = function(df, uid, melt_cols = c(), variable_name = "variable", value_name="values", remove_na = "TRUE"){
  df_melted = melt(dwp_melted, uid, melt_cols, variable.name = variable_name, value.name=value_name, na.rm=remove_na)
}


### Address parsing

### Remove date auto parse from excel
excel_date_to_flat = function(vec){
  ### Set up the parse for months to number
  month_parse = list(Jan=1, Feb=2, Mar=3, Apr=4, May=5, Jun=6, Jul=7, Aug=8, Sep=9, Oct=10, Nov=11, Dec=12)
  ### Split the components
  matches = str_match(vec, "(\\d{2})(-)(\\w{3})")
  ### Rebuild the flat string
  flat_string = paste(as.numeric(matches[,2]), "/", month_parse[matches[,4]], sep="")
  ### Return
  return(flat_string)
}

### Split into house_number, street and town
add_parsed_address_cols = function(df, address_cols = c("addr1", "addr2"), house_regex = list(filter1="filter1_regex", filter2="filter2_regex", select1="select1_regex", select2="select2_regex"), street_regex=list(filter="filter_regex", select1="select1_regex", select2="select2_regex"), postcode_regex="") {
  address_string = paste(df[,address_cols])
  address_string = trim_all(tolower(address_string))
  
  house_number = unname(sapply(address_string, function(x) {
    ## if it has a flat, site or house on it
    if (any(!is.na(str_match(x,  house_regex["filter1"])))){
      str_match(x,  house_regex["select1"])[1]
      ## only the strings that match for example 19/9 or 18/a if it doesn't have flat site or house in it  
    } else if (any(!is.na(str_match(x, house_regex["filter2"])))){
      str_match(x, house_regex["select2"])
      ## Empty otherwise
    } else {
      ""
    }
  }))
  
  street_address = unname(sapply(address_string, function(x) {
    ## filter 1
    if (any(!is.na(str_match(x,  street_regex["filter1"])))){
      str_match(x,  street_regex["select1"])[1]
      ## filter 2
    } else if (any(!is.na(str_match(x, street_regex["filter2"])))){
      str_match(x, street_regex["select2"])
      ## Empty otherwise
    } else {
      ""
    }
  }))
  
  postcode = unname(sapply(address_string, function(x) {
    ## if it has a flat, site or house on it
    if (any(!is.na(str_match(x,  postcode_regex)))){
      str_match(x,  postcode_regex)[1]
    }
  }))
  
  df$address_full = address_string
  df$house_number = house_number
  df$street_address = street_address
  df$postcode = postcode
  
  return(df)
}