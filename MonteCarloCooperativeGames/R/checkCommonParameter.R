#' @name checkCommonParameter
#' @title Checks the common parameters for their validity
#' @description
#' Checks the common parameters for their validity.
#' @template author/EW
#' @template param/v
#' @template param/n
#' @template param/i
#' @template param/m
#' @template param/P
#' @template param/w
#' @return Returns
#' @export
checkCommonParameter <- function(v, n=NULL, P=NULL, i=NULL, m=NULL, w=NULL) {

  resultTable = list()

  if(!is.null(n) && !is.null(P))
    resultTable = append(resultTable, createMessage(2, "Define only n or P (not both)"))

  # Check if characteristic function is NULL
  if(!is.null(v)) {

    if(!is.function(v))
      resultTable = append(resultTable, createMessage(3, "Parameter v isn't a function"))

    # There must be one parameter (The quantity)
  }
  else {
    resultTable = append(resultTable, createMessage(3, "Function is NULL"))
  }



  # if n is set
  if(!is.null(n)) {

    if(!is.numeric(n)) {
      resultTable = append(resultTable, createMessage(3, "n is not a number"))
    }

    if(n <= 0) {
      resultTable = append(resultTable, createMessage(3, "n must be greater than 0"))
    }

  }



  # if P is set
  if(!is.null(P)) {

    # Step 1: check if it is a list
    if(!is.list(P)) {
      resultTable = append(resultTable, createMessage(3, "Priori unions must be a list"))
    }
    else {
      #Step 2: Here the respective unions are checked. if one isn't a vector, the next step will be skipped
      #if(all(sapply(unlist(P), is.vector)) == FALSE) {
      #  resultTable = append(resultTable, createMessage(3, "There is a priori union in P that isn't a vector or alist"))
      #}

      index <- 1
      mistake <- FALSE
      for(union in P) {
         if(!is.vector(union)) {
           resultTable = append(resultTable, createMessage(3, paste("Priori union with index", index, "is not a vector or a list")))
           mistake <- TRUE
         }
         index <- index + 1
      }

      if(TRUE) {

        #Step 3: Check if the values are numeric
        if(all(sapply(unlist(P), is.numeric)) == FALSE) {
          resultTable = append(resultTable, createMessage(3, "There is a priori union that contains a element that isn't numeric"))
        }
        else {
          #Step 4: Check if the prior unions are disjunct
          mistake <- FALSE

          index_1 <- 1
          laenge <- length(P)
          while(index_1 < laenge) {
            A <- P[index_1]

            index_2 <- index_1+1
            while(index_2 <= laenge) {
              B <- P[index_2]

              if(length(intersect(A,B)) > 0) {
                mistake <- TRUE
                break
              }
              index_2 <- index_2 + 1
            }

            if(mistake == TRUE) break

            index_1 = index_1 + 1
          }

          if(mistake == TRUE) {
            resultTable = append(resultTable, createMessage(3, "Priori unions aren't disjunct"))
          }

        }

      }



    }
  }



  # if i is set
  if(!is.null(i)) {

    if(!is.numeric(i))
      resultTable = append(resultTable, createMessage(3, "i must be a number"))
    else {
      if(i < 0)
        resultTable = append(resultTable, createMessage(3, "i must be greater or equal 0"))
      else {
        # If P is set
        if(!is.null(P) && i %in% unlist(P) == FALSE)
          resultTable = append(resultTable, createMessage(3, "i isn't in a priori unions"))

        # if n is set
        else if(!is.null(n) && i >= n)
          resultTable = append(resultTable, createMessage(3, "i isn't in n"))
      }
    }

  }



  #if m is set
  if(!is.null(m)) {

  }



  # if w is set
  if(!is.null(w)) {

  }

  processParamCheckResultTable(resultTable)
}






#' @name createMessage
#' @title Creates a named list to store the info, warning and error messages used by checkCommonParams
#' @description Returns a named list which includes the typeCode an the message
#' There are three message types:
#'  - info: 1
#'  - warning: 2
#'  - error: 3
#' @export
#' @param tc typeCode
#' @param msg message
#' @template author/EW
#' @return list with 2 elements. First one is called typeCode which contains an integer
#' representing the type of the message. Second one is the message
#' @examples
#' #Fehlt noch
createMessage <- function(tc, msg) {

  paramCheckResult = list(typeCode=tc,message=msg)

  return(paramCheckResult)
}





#' @name processParamCheckResultTable
#' @title processParamCheckResultTable - creates logging on  stdout
#' @description prints the messages
#' @export
#' @template author/EW
#' @param paramCheckResultTable the table with the messages
#' @examples
#' #Fehlt noch
processParamCheckResultTable=function(paramCheckResultTable){

  for(paramCheckResult in paramCheckResultTable) {
    print(paramCheckResult)
    #print(paste("TypeCode " ,getElement(paramCheckResult,"typeCode"),": ",getElement(paramCheckResult,"message"), sep=""))
  }

}

