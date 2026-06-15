filter_rows_by_reference_rownames <- function(df, ref_df) {
  # get rownames of reference dataframe
  ref_rows <- rownames(ref_df)
  # filter rows of original dataframe that match with reference dataframe
  new_df <- df[rownames(df) %in% ref_rows, ]
  return(new_df)
}