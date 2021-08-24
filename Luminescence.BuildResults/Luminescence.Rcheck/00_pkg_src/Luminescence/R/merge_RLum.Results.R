#' Merge function for RLum.Results S4-class objects
#'
#' Function merges objects of class [RLum.Results-class]. The slots in the objects
#' are combined depending on the object type, e.g., for [data.frame] and [matrix]
#' rows are appended.
#'
#' @note The originator is taken from the first element and not reset to `merge_RLum`
#'
#' @param objects [list] (**required**):
#' a list of [RLum.Results-class] objects
#'
#' @section Function version: 0.2.0
#'
#' @keywords internal
#'
#' @author
#' Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#' @md
#' @export
merge_RLum.Results <- function(
    objects){

            ##-------------------------------------------------------------
            ##Some integrity checks

            ##check if input object is a list
            if(!is(objects, "list")){

              stop("[merge_RLum.Results()] 'objects' has to of type 'list'!")

            }else{

              ##check if objects in the list are of type RLum.Results
              temp.originator <- sapply(1:length(objects), function(x){

                if(is(objects[[x]], "RLum.Results") == FALSE){

                  stop("[merge_RLum.Results()] Objects to merge have
                       to be of type 'RLum.Results'!")

                }

                objects[[x]]@originator

              })
              }

            ##check if originator is different
            if(length(unique(temp.originator))>1){

              stop("[merge_RLum.Results()] 'RLum.Results' object originator
                   differs!")
            }

            ##-------------------------------------------------------------
            ##merge objects depending on the data structure

            for(i in 1:length(objects[[1]]@data)){

              ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++
              ##numeric vector or data.frame or matrix
              if(is(objects[[1]]@data[[i]], "data.frame")||
                 is(objects[[1]]@data[[i]], "numeric") ||
                 is(objects[[1]]@data[[i]], "matrix")){

                ##grep elements and combine them into a list
                temp.list <-
                  lapply(1:length(objects), function(x) {
                    objects[[x]]@data[[i]]

                  })

                ##check whetger the objects can be combined by rbind
                if(length(unique(unlist(lapply(temp.list, FUN = ncol)))) > 1){

                  stop("[merge_RLum.Results()] Objects cannot be combined, number of columns differs.")

                }

                ##combine them using rbind or data.table::rbindList (depends on the data type)
                if(is(objects[[1]]@data[[i]], "numeric")){
                  objects[[1]]@data[[i]] <- unlist(temp.list)

                }else if(is(objects[[1]]@data[[i]], "matrix")){
                  objects[[1]]@data[[i]] <- do.call("rbind", temp.list)

                }else{
                  objects[[1]]@data[[i]] <- as.data.frame(data.table::rbindlist(temp.list))

                }


              }else{

                ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                ##all other elements

                ##grep elements and write them into a list
                objects[[1]]@data[[i]] <- lapply(1:length(objects),
                                                     function(x){

                                                       objects[[x]]@data[[i]]

                                                     })


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
              }))

            )

            return(objects_merged)

}
