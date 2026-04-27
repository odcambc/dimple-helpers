library(stringr)

parse_hmm_profile <- function(hmm_profile_file) {
  # Read the file
  con <- file(hmm_profile_file, "r")
  #lines <- readLines(hmm_profile_file)
  
  # Initialize the variables
  # Mandatory fields
  hmm_profile <- list()
  hmm_profile <- list()
  
  hmm_profile$format <- ""
  hmm_profile$name <- ""
  hmm_profile$leng <- 0
  hmm_profile$alph <- ""
  hmm_profile$cons <- FALSE
  hmm_profile$hmm <- data.frame()
  
  hmm_profile$consensus_sequence <- c()
  
  # Optional fields
  hmm_profile$acc <- ""
  hmm_profile$desc <- ""
  hmm_profile$maxl <- 0
  hmm_profile$rf <- FALSE
  hmm_profile$mm <- FALSE
  hmm_profile$cs <- FALSE
  hmm_profile$map <- FALSE
  hmm_profile$date <- ""
  hmm_profile$com <- c()
  hmm_profile$nseq <- 0
  hmm_profile$effn <- 0.0
  hmm_profile$cksum <- 0
  hmm_profile$ga <- c(0.0, 0.0)
  hmm_profile$tc <- c(0.0, 0.0)
  hmm_profile$nc <- c(0.0, 0.0)
  hmm_profile$stats <- c(list("", "", 0.0, 0.0))
  
  hmm_profile$model <- list()
  hmm_profile$model$characters <- c()
  hmm_profile$model$transitions <- c()
  hmm_profile$model$nodes <- list()
  hmm_profile$model$compo <- c()
  
  # Initialize the state
  state <- "header"
  
  # Iterate through lines
  while ( TRUE ) {
    line <- readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    # Now parse the line
    
    # Split the line
    fields <- strsplit(line, "\\s+")[[1]]
    
    
    # Parse the line
    # Update state
    if (fields[1] == "HMM") {
      state <- "model"
    }
    
    if (state == "header") {
      if (fields[1] == "HMMER3/f") {
        hmm_profile$format <- str_flatten(fields[-1])
      } else if (fields[1] == "NAME") {
        hmm_profile$name <- fields[2]
      } else if (fields[1] == "ACC") {
        hmm_profile$acc <- fields[2]
      } else if (fields[1] == "DESC") {
        hmm_profile$desc <- str_flatten(fields[-1], collapse = " ")
      } else if (fields[1] == "LENG") {
        hmm_profile$leng <- as.integer(fields[2])
      } else if (fields[1] == "MAXL") {
        hmm_profile$maxl <- as.integer(fields[2])
      } else if (fields[1] == "ALPH") {
        hmm_profile$alph <- fields[2]
      } else if (fields[1] == "RF") {
        if (tolower(fields[2]) == "yes") {
          hmm_profile$rf <- TRUE
        } else {
          hmm_profile$rf <- FALSE
        }
      } else if (fields[1] == "MM") {
        if (tolower(fields[2]) == "yes") {
          hmm_profile$mm <- TRUE
        } else {
          hmm_profile$mm <- FALSE
        }
      } else if (fields[1] == "CONS") {
        if (tolower(fields[2]) == "yes") {
          hmm_profile$cons <- TRUE
        } else {
          hmm_profile$cons <- FALSE
        }
      } else if (fields[1] == "CS") {
        if (tolower(fields[2]) == "yes") {
          hmm_profile$cs <- TRUE
        } else {
          hmm_profile$cs <- FALSE
        }
      } else if (fields[1] == "MAP") {
        if (tolower(fields[2]) == "yes") {
          hmm_profile$map <- TRUE
        } else {
          hmm_profile$map <- FALSE
        }
      } else if (fields[1] == "DATE") {
        hmm_profile$date <- str_flatten(fields[-1], collapse = " ")
      } else if (fields[1] == "COM") {
        # Compile the multiple lines
        # TODO
        hmm_profile$com <- fields[-1]
      } else if (fields[1] == "NSEQ") {
        hmm_profile$nseq <- as.integer(fields[2])
      } else if (fields[1] == "EFFN") {
        hmm_profile$effn <- as.numeric(fields[2])
      } else if (fields[1] == "CKSUM") {
        hmm_profile$cksum <- as.integer(fields[2])
      } else if (fields[1] == "GA") {
        hmm_profile$ga <- as.numeric(str_replace_all(fields[-1], ";", ""))
      } else if (fields[1] == "TC") {
        hmm_profile$tc <- as.numeric(str_replace_all(fields[-1], ";", ""))
      } else if (fields[1] == "NC") {
        hmm_profile$nc <- as.numeric(str_replace_all(fields[-1], ";", ""))
      } else if (fields[1] == "STATS") {
        hmm_profile$stats <- append(hmm_profile$stats,
                                    list(fields[2], fields[3], as.numeric(fields[4]), as.numeric(fields[5])))
      }
      else {
        print(paste("Unknown field:", fields[1]))
      }
    } else if (state == "model") {
      if (fields[1] == "HMM") {
        # Read two lines: first is match characters, second is transitions
        hmm_profile$characters <- fields[-1]
        transitions_string <- readLines(con, n = 1)
        hmm_profile$transitions <- strsplit(transitions_string, "\\s+")[[1]][-1]
        
        # Next line is either COMPO or the beginning node
        next_line <- readLines(con, n = 1)
        fields <- strsplit(next_line, "\\s+")[[1]]
        
        if (fields[2] == "COMPO") {
          # Read the line and move on to the beginning node info
          hmm_profile$compo <- as.numeric(fields[-1:-2])
          next_line = readLines(con, n = 1)
          fields <- strsplit(next_line, "\\s+")[[1]]
        }
        
        n_char <- length(hmm_profile$characters)
        n_trans <- length(hmm_profile$transitions)
        
        hmm_profile$model$match_probabilities <- data.frame(matrix(nrow = hmm_profile$leng + 1, ncol = n_char + 6))
        hmm_profile$model$insert_probabilities <- data.frame(matrix(nrow = hmm_profile$leng, ncol = n_char + 6))
        hmm_profile$model$transition_probabilities <- data.frame(matrix(nrow = hmm_profile$leng + 1, ncol = n_trans + 6))
        
        # Parse the beginning node info
        
        beginning_node_matches <- as.numeric(fields[-1])
        hmm_profile$model$match_probabilities[1,1] <- 0
        hmm_profile$model$match_probabilities[1,2:(n_char+1)] <- beginning_node_matches
        hmm_profile$model$match_probabilities[1,(n_char+2):(n_char+6)] <- c(NA, NA, NA, NA, NA)
        
        beginning_node_transitions_string <- readLines(con, n = 1)
        beginning_node_transitions <- as.numeric(
          strsplit(
            beginning_node_transitions_string, "\\s+"
          )[[1]][-1]
        )
        hmm_profile$model$transition_probabilities[1,1] <- 0
        hmm_profile$model$transition_probabilities[1,2:(n_trans+1)] <- beginning_node_transitions
        hmm_profile$model$transition_probabilities[1,(n_trans+2):(n_trans+6)] <- c(NA, NA, NA, NA, NA)
        
        # Otherwise, parse the node info. We expect the first field to be a number.
        # This block will parse the next three lines.
      } else if (!is.na(as.integer(fields[2]))) {
        # Parse the node number
        number <- as.integer(fields[2])
        
        # Parse the match probabilities
        match_emissions <- as.numeric(fields[3:22])
        
        
        # Note: MAP field, if false, is '-', which becomes NA here.
        map <- as.integer(fields[23])
        consensus <- fields[24]
        rf <- fields[25]
        mm <- fields[26]
        cs <- fields[27]
        # Add to the consensus sequence if possible
        if (consensus != "-") {
          if (map != "-") {
            hmm_profile$consensus[map] <- consensus
          } else {
            hmm_profile$consensus <- append(hmm_profile$consensus, consensus)
          }
        }
        
        # Next line is the insert emission line
        insert_emissions_string <- readLines(con, n = 1)
        insert_emissions <- as.numeric(strsplit(insert_emissions_string, "\\s+")[[1]][-1])
        
        # Next line is transition line
        transitions_string <- readLines(con, n = 1)
        transitions <- as.numeric(strsplit(transitions_string, "\\s+")[[1]][-1])
        
        # Now add the data to the data frames
        hmm_profile$model$match_probabilities[number + 1,1] <- number
        hmm_profile$model$match_probabilities[number + 1,2:(n_char+1)] <- match_emissions
        hmm_profile$model$match_probabilities[number + 1,(n_char+2):(n_char+6)] <- c(map, consensus, rf, mm, cs)
        
        hmm_profile$model$insert_probabilities[number,1] <- number
        hmm_profile$model$insert_probabilities[number,2:(n_char+1)] <- insert_emissions
        hmm_profile$model$insert_probabilities[number,(n_char+2):(n_char+6)] <- c(map, consensus, rf, mm, cs)
        
        hmm_profile$model$transition_probabilities[number + 1,1] <- number
        hmm_profile$model$transition_probabilities[number + 1,2:(n_trans+1)] <- transitions
        hmm_profile$model$transition_probabilities[number + 1,(n_trans+2):(n_trans+6)] <- c(map, consensus, rf, mm, cs)
        
      }
      else if (fields[1] == "//") {
        
        # We should be done with the record. This is the last line unless
        # there is a trailing comment or additional records.
        
        state <- "done"
      } else {
        warning(paste("Unknown field:", fields[1]))
      }
    } else if (state == "done") {
      # We are done with the record. We should not be here.
      warning(paste("Unexpected line after record end: ", line))
    }
  }
  
  # Close the file
  close(con)
  
  # Change NA in consensus to gaps if they exist, then collapse to a string
  hmm_profile$consensus[is.na(hmm_profile$consensus)] <- "-"
  hmm_profile$consensus <- str_flatten(hmm_profile$consensus)
  
  names(hmm_profile$model$match_probabilities) <- c("node", hmm_profile$characters, "map", "consensus", "rf", "mm", "cs")
  names(hmm_profile$model$insert_probabilities) <- c("node", hmm_profile$characters, "map", "consensus", "rf", "mm", "cs")
  names(hmm_profile$model$transition_probabilities) <- c("node", hmm_profile$transitions, "map", "consensus", "rf", "mm", "cs")
  
  return(hmm_profile)
}
