#' @title Merge function for RLum.Results S4-class objects
#'
#' @description Function merges objects of class [RLum.Results-class]. The slots in the objects
#' are combined depending on the object type, e.g., for [data.frame] and [matrix]
#' rows are appended.
#'
#' @details Elements are appended where possible and attributes are preserved if
#' not of similar name as the default attributes of, e.g., a [data.frame]
#'
#' @note The `originator` is taken from the first element and not reset to `merge_RLum`
#'
#' @param objects [list] (**required**):
#' a list of [RLum.Results-class] objects
#'
#' @section Function version: 0.2.1
#'
#' @keywords internal
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @export
merge_RLum.Results <- function(
  objects
) {
  .set_function_name("merge_RLum.Results")
  on.exit(.unset_function_name(), add = TRUE)

  ## Integrity checks -------------------------------------------------------

  .validate_class(objects, "list")

  ## check if objects in the list are of type RLum.Results
  temp.originator <- sapply(objects, function(x) {
    .validate_class(x, "RLum.Results", name = "All elements of 'object'")
    x@originator
  })
  if (length(objects) == 0) {
    .throw_message("'objects' contains no data, NULL returned")
    return(NULL)
  }

            ##check if originator is different
            if(length(unique(temp.originator))>1){
              .throw_error("Objects cannot be merged, different 'RLum.Results' originators found")
            }

            ##-------------------------------------------------------------
            ##merge objects depending on the data structure
            for (i in seq_along(objects[[1]]@data)) {

              ## shelf list of attributes
              attr_list <- unlist(
                lapply(objects, function(x) attributes(x@data[[i]])),
                recursive = FALSE)

              ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++
              ##numeric vector or data.frame or matrix
              if(is(objects[[1]]@data[[i]], "data.frame")||
                 is(objects[[1]]@data[[i]], "numeric") ||
                 is(objects[[1]]@data[[i]], "matrix")){

                ##grep elements and combine them into a list
                temp.list <- lapply(objects, function(x) x@data[[i]])

                ##check whether the objects can be combined by rbind
                if(length(unique(unlist(lapply(temp.list, FUN = ncol)))) > 1)
                  .throw_error("Objects cannot be merged, different number of columns")

                ##combine them using rbind or data.table::rbindList (depends on the data type)
                if(is(objects[[1]]@data[[i]], "numeric")){
                  objects[[1]]@data[[i]] <- unlist(temp.list)

                }else if(is(objects[[1]]@data[[i]], "matrix")){
                  objects[[1]]@data[[i]] <- do.call("rbind", temp.list)

                }else{
                  objects[[1]]@data[[i]] <- as.data.frame(data.table::rbindlist(temp.list))
                }

                ## continue attribute preservation
                ## remove attributes that stem from the object itself
                attr_list[names(attr_list) %in% names(attributes(objects[[1]]@data[[i]]))] <- NULL

                ## just to avoid working the code if not needed
                if(length(attr_list) > 0) {
                  ## merge attributes with similar name
                  attrs <- lapply(unique(names(attr_list)), function(x){
                    tmp <- unlist(attr_list[names(attr_list)%in%x], recursive = FALSE)
                    names(tmp) <- NULL
                    tmp
                  })
                  names(attrs) <- unique(names(attr_list))

                  # set attributes ... we try because some attributes
                  for(n in names(attrs))
                    attr(objects[[1]]@data[[i]], n) <- attrs[[n]]
                }

              }else{
                ##all other elements
                ##grep elements and write them into a list
                objects[[1]]@data[[i]] <- lapply(objects, function(x) x@data[[i]])

                ##unlist to flatten list if necessary for the elements
                if(is(objects[[1]]@data[[i]][[1]])[1] == "list"){
                  objects[[1]]@data[[i]] <- unlist(objects[[1]]@data[[i]],
                                                       recursive = FALSE)
                }
              }

            }##end loop

            #return by setting a new RLum.Results (for the .uid)
            #the originator is not reset
            objects_merged <- set_RLum(
              class = "RLum.Results",
              originator = objects[[1]]@originator,
              data = objects[[1]]@data,
              info = unlist(lapply(objects, function(x) {
                x@info
              }), recursive = FALSE),
              .pid = unlist(lapply(objects, function(x) {
                x@.uid
              })))

            return(objects_merged)
}
