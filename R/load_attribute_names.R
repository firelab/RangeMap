#' Load an attributes file (.csv or .dbf) and get attribute names / indices
#'
#' @param attributes_path Path to attributes file. Preferable to use the RangeMap_Attributes.csv so full field names are preserved, but the .tif.vat.dbf associated with one year's raster may also be used
#' @return The column index and the name of all numeric attributes that can be generated
#' @export
#'
#' @examples
#'
#' # Load attribute names
#' attribute_names<- load_attribute_names(attributes_path)
#'
#' # Make vector of the desired attribute names (or the column indices of the desired attributes returned by load_attribute_names) needed for the generate_attribute_layers function
#' attribute_ids<- attribute_names$attribute_name[c(1:2, 77, 120)]   # Vector of attribute names
#' attribute_ids<- c(1:2, 77, 120) # Vector of numeric column indices returned from load_attribute_names
#'
#'
load_attribute_names<- function(attributes_path){

  # Check if the attributes path is csv or dbf, then load it
  if(endsWith(attributes_path, ".dbf")){
    atts<- foreign::read.dbf(attributes_path)
    message("Attributes are from dbf file. Recommend using RangeMap Attributes csv for full attribute names")
  } else if(endsWith(attributes_path, ".csv")){
    atts<- utils::read.csv(attributes_path, check.names = F)
  } else{
    message("Expected .csv or .dbf file, something else provided")
  }

  # Get attribute names, and remove extraneous data
  attribute_names<- names(atts)
  attribute_names<- attribute_names[sapply(atts, is.numeric)]
  attribute_names<- attribute_names[-which(attribute_names %in% c("Value", "RM_ID", "Count", "PrimaryKey", "DataSource"))]

  # Arrange into dataframe for easy reading
  attributes<- data.frame("index" = seq(1,length(attribute_names), 1),
                          "attribute_name" = attribute_names)
  #
  return(attributes)
}
