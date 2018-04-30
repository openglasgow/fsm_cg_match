### data match

generate_pairs = function(df1, df2, excluded_cols){
  
  ### Generate pairs and weight them
  pairs = RLBigDataLinkage(df1, df2, exclude = excluded_cols, strcmp=TRUE, strcmpfun="levenshtein")
  pairs_weighted = epiWeights(pairs)
  
  ### hist inspection
  pairs_weighted_hist = hist(pairs_weighted@Wdata, breaks=100, plot=FALSE)
  
  return(list(pairs=pairs_weighted, hist=pairs_weighted_hist))
  
}

classify_pairs = function(pairs, lower_threshold=0.60, upper_threshold=.70){
  
  ### Classify the links
  pairs_classified=epiClassify(pairs, threshold.upper=upper_threshold, threshold.lower=lower_threshold)
  
  
  ### Get pairs
  links = getPairs(pairs_classified, filter.link="link", single.rows=TRUE)
  possible = getPairs(pairs_classified, filter.link="possible", single.rows=TRUE)
  links_review = getPairs(pairs_classified, filter.link="link")
  possible_review = getPairs(pairs_classified, filter.link="possible")
  
  ### Bind them
  pairs = rbind(links, possible)
  pairs_review = rbind(links_review, possible_review)
  
  ### Return
  return(list(pairs=pairs, review=pairs_review))
}

save_rl_file = function(rl_object, location){
  saveRLObject(rl_object, location)
}