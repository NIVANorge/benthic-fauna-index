sort_results <- function(df){

  if("Replicate" %in% names(df)){
    if("Station" %in% names(df)){
      df <- df %>%
        mutate(ord1 = factor(Station,
                             levels = str_sort(unique(Station), numeric = T),
                             ordered = T)) %>%
        mutate(ord2 = factor(Replicate,
                             levels = str_sort(unique(Replicate), numeric = T),
                             ordered = T)) %>%
        arrange(ord1, ord2) %>%
        select(-ord1, -ord2)
    }else{
      df <- df %>%
        mutate(ord1 = factor(Replicate,
                             levels = str_sort(unique(Replicate), numeric = T),
                             ordered = T)) %>%
        arrange(ord1) %>%
        select(-ord1)
    }
  }else{
    if("Station" %in% names(df)){
    df <- df %>%
      mutate(ord1 = factor(Station,
                           levels = str_sort(unique(Station), numeric = T),
                           ordered = T)) %>%
      arrange(ord1) %>%
      select(-ord1)
  }}
  return(df)
}


match_list<- function(list, s){

  res <- NULL
  for(si in s){
    si <- tolower(si)
    n <- nchar(si)
    if(length(list)>0){
      listmatch <- tolower(list)
      listmatch  <- list[stringr::str_sub(listmatch ,1,n)==si]
      if(length(listmatch )>0){
        res <- listmatch[1]
        break
      }
    }
  }
  return(res)
}


get_number <- function(s){
  n <- lapply(s, get_number_single) %>% unlist()
  return(n)
}

get_number_single <- function(s){
  n2 <- ifelse(is.numeric(s),s,NA)
  s1 <- stringr::str_extract(s, "^\\d+")
  s1 <- ifelse(nchar(s1)==nchar(s),s1,"")
  n1 <- ifelse(nchar(s1)==0,NA,as.numeric(s1))
  n <- ifelse(is.na(n1),n2,n1)
  return(n)
}


reform_data <- function(df, form, idStn, idRep, idSpec, idCount,
                        label_long, label_wide_species, label_wide_station,
                        has_header, progress=NULL){
  split_n <- 10
  ip <- 1
  if(!is.null(progress)){
    ip <- ip + 1
    progress$set(value = ip)
  }

  dropped <- 0
  msg <- ""
  count_variable <- NA

  if(form==label_long){

    if(is.null(idCount)){
      return(NULL)
    }
    cols <- c(idStn,
              idRep,
              idSpec,
              idCount)

    colnames <- c("Station",
                  "Replicate",
                  "Species",
                  "Count")

    colnames <- colnames[cols!="none"]
    cols <- cols[cols!="none"]
    if(length(cols)>0){
      if(!has_header){
        cols <- as.numeric(cols)
        for(i in 1:length(cols)){
          names(df)[cols[i]] <- colnames[i]
        }
      }else{
        if(!idCount %in% names(df)){
          return(NULL)
        }
        for(i in 1:length(cols)){
          names(df)[names(df)==cols[i]] <- colnames[i]
          cols[i] <- colnames[i]
        }
      }
      for(coli in cols)
      if(!coli %in% names(df)){
        return(NULL)
      }
      df <- df[,cols]
      df <- df %>%
        mutate(Count=get_number(Count)) %>%
        filter(!is.na(Count))

    }else{
      return(NULL)
    }
  }else if(form==label_wide_species){

    row_spec <- as.numeric(idSpec)

    if(is.na(row_spec)){
      return(NULL)
    }else{
      group_vars <- c("Species")

      species <- df[row_spec,] %>% unlist()
      col_stn <- idStn
      col_rep <- idRep
      cols_keep <- 1:ncol(df)
      rows_keep <- 1:nrow(df)
      if(col_rep!="none"){
        group_vars <- c("Replicate", group_vars)
        species[names(df)==col_rep] <- "Replicate"
      }
      if(col_stn!="none"){
        group_vars <- c("Station", group_vars)
        species[names(df)==col_stn] <- "Station"
      }

      cols_keep <- cols_keep[!is.na(species)]
      rows_keep <- rows_keep[rows_keep!=row_spec]
      names(df) <- species

      na_cols <- 1:ncol(df)
      na_cols <- na_cols[is.na(species)]

      if(length(n)>0){
        dropped <- df[,na_cols] %>% unlist()
        dropped <- dropped %>%
          as.numeric()
        dropped <- dropped[!is.na(dropped)]
        dropped <- length(dropped)
        msg <- "missing species names"
      }

      if(!is.null(progress)){
        ip <- ip + 1
        progress$set(value = ip)
      }


      cols_piv <- names(df)[!is.na(species)]
      cols_piv <- cols_piv[!cols_piv %in% c("Station","Replicate")]
      df <- df[rows_keep,cols_keep]

      df <- df %>%
        pivot_split(cols_piv=cols_piv,
                    names_to="Species",
                    values_to = "Count",
                    split_n=split_n,
                    progress = progress)


      if(!is.null(progress)){
        ip <- ip + 1
        progress$set(value = ip)
      }

      df <- df %>%
        mutate(Count=get_number(Count)) %>%
        filter(!is.na(Count))


      if(!is.null(progress)){
        ip <- ip + 1
        progress$set(value = ip)
      }

      df <- df %>%
        group_by(across(all_of(group_vars))) %>%
        summarise(Count=sum(Count,na.rm=T), .groups="drop")

      if(!is.null(progress)){
        ip <- ip + 1
        progress$set(value = ip)
      }
    }

  }else if(form==label_wide_station){

    colSpec <- idSpec
    ixcolSpec <- match(colSpec, names(df))

    if(colSpec=="none"){
      return(NULL)
    }else{
      row_stn <- as.numeric(idStn)
      row_rep <- as.numeric(idRep)

      if(is.na(row_stn) & is.na(row_rep)){

        names(df) <- paste0("col_",1:ncol(df))
        names(df)[ixcolSpec] <- "Species"

        if(!"Species" %in% names(df)){
          return(NULL)
        }

        if(!is.null(progress)){
          ip <- ip + 1
          progress$set(value = ip)
        }

        df <- df %>%
          filter(!is.na(Species))

        cols_piv <- 1:ncol(df)
        cols_piv <- cols_piv[cols_piv!=ixcolSpec]
        cols_piv <- names(df)[cols_piv]

        if(!is.null(progress)){
          ip <- ip + 1
          progress$set(value = ip)
        }
        df <- df %>%
          pivot_split(cols_piv=cols_piv,
                      names_to="Station",
                      values_to = "Count",
                      split_n=split_n,
                      progress = progress)

        # df <- df %>%
        #   pivot_longer(cols=all_of(cols_piv),
        #                names_to="Column",
        #                values_to = "Count", values_drop_na=T)

        if(!is.null(progress)){
          ip <- ip + 1
          progress$set(value = ip)
        }

        df <- df %>%
          mutate(Count=get_number(Count)) %>%
          filter(!is.na(Count))

        if(!is.null(progress)){
          ip <- ip + 1
          progress$set(value = ip)
        }

        df <- df %>%
          select(all_of(c("Species","Count")))

        if(!is.null(progress)){
          ip <- ip + 1
          progress$set(value = ip)
        }

        df <- df %>%
          group_by(Species) %>%
          summarise(Count=sum(Count, na.rm = T), .groups="drop")


      }else{ # is.na(row_stn) & is.na(row_rep)
        #
        if(!is.na(row_stn)){
          stns <- df[row_stn,] %>% unlist()
        }else{
          stns <- rep("", ncol(df))
        }

        if(!is.na(row_rep)){
          reps <- df[row_rep,] %>% unlist()
        }else{
          reps <- rep("", ncol(df))
        }

        stns <- paste(stns, reps, sep="_")

        names(df) <- stns
        names(df)[ixcolSpec] <- "Species"


        #rows_keep <- 1:nrow(df)
        #rows_keep <- rows_keep[!rows_keep %in% c(row_stn, row_rep)]
        #df <- df[rows_keep,]

        names_orig <-names(df)
        names_t <- paste0("col_",(1:length(names_orig)))
        names_t[ixcolSpec] <- "Species"

        names(df) <- c(names_t)

        cols_piv <- names_t[names_t!="Species"]

        df <- df %>%
          filter(!is.na(Species))

        df <- df %>%
          pivot_split(cols_piv=cols_piv,
                      names_to="StationX",
                      values_to = "Count",
                      split_n=split_n,
                      progress = progress)

        names_match <- data.frame(StationX=names_t, Station=names_orig)

        df <- df %>%
          left_join(names_match,by="StationX") %>%
          select(-StationX)

        df <- df %>%
          separate_wider_delim("Station", names = c("Station","Replicate"), delim="_", too_few = "align_start") %>%
          mutate(Replicate=ifelse(is.na(Replicate),"",Replicate))


        for(i in 2:nrow(df)){
          if(df[i,"Station"] == "NA"){
            df[i,"Station"] <- df[i-1,"Station"]
          }
          if(df[i,"Replicate"] == "NA"){
            df[i,"Replicate"] <- df[i-1,"Replicate"]
          }
        }

        df <- df %>%
          filter(!is.na(Count)) %>%
          mutate(Count = as.numeric(Count))

        if(!is.null(progress)){
          ip <- ip + 1
          progress$set(value = ip)
        }

        if(is.na(row_stn)){
          df <- df %>%
            group_by(Replicate, Species) %>%
            summarise(Count=sum(Count, na.rm = T), .groups="drop")

          df <- df %>%
            mutate(ord1 = factor(Replicate,
                                 levels = str_sort(unique(Replicate), numeric = T),
                                 ordered = T)) %>%
            arrange(ord1, Species) %>%
            select(-ord1)

        }else{
          if(is.na(row_rep)){
            df <- df %>%
              group_by(Station, Species) %>%
              summarise(Count=sum(Count, na.rm = T), .groups="drop")

            df <- df %>%
              mutate(ord1 = factor(Station,
                                   levels = str_sort(unique(Station), numeric = T),
                                   ordered = T)) %>%
              arrange(ord1, Species) %>%
              select(-ord1)
          }else{
            df <- df %>%
              mutate(ord1 = factor(Station,
                                   levels = str_sort(unique(Station), numeric = T),
                                   ordered = T)) %>%
              mutate(ord2 = factor(Replicate,
                                   levels = str_sort(unique(Replicate), numeric = T),
                                   ordered = T)) %>%
              arrange(ord1, ord2, Species) %>%
              select(-ord1, -ord2)
          }

        }
        if(!is.null(progress)){
          ip <- ip + 1
          progress$set(value = ip)
        }

      } # is.na(row_stn) & is.na(row_rep)
      }
  }

  if("Station" %in% names(df)){
    df <- df %>%
      mutate(Station=ifelse(is.na(Station),"",Station))
  }

  df <- df %>%
    select(any_of(c("Station","Replicate","Species","Count")))

  return(list("df"=df, "dropped"=dropped, "msg"=msg))

}


pivot_part <- function(df, cols_piv, names_to, values_to = "Count", progress=progress, pstep=1){

  df <- df %>%
     pivot_longer(cols=all_of(cols_piv), names_to=names_to, values_to = "Count")

  df <- df %>%
    filter(!is.na(Count)) %>%
    filter(Count!="0")

  if(!is.null(progress)){
    progress$inc(pstep)
  }

  return(df)

}

pivot_split <- function(df, cols_piv, names_to, values_to = "Count", split_n, progress){

  df_split <- df %>%
    mutate(split_id = 1+floor(row_number()/split_n)) %>%
    split(.$split_id)

  pstep = floor(90/length(df_split))

  df <- purrr::map(df_split, pivot_part, cols_piv,
                   names_to=names_to, values_to = "Count",
                   progress=progress, pstep=pstep) %>%
    bind_rows() %>%
    select(-split_id)

  return(df)

}


fix_column_names<- function(s){

  df <- data.frame(ix=1:length(s), s=s)

  df <- df %>%
    mutate(s=ifelse(is.na(s),".",s))

  df <- df %>%
    group_by(s) %>%
    arrange(ix) %>%
    mutate(n=row_number()-1) %>%
    ungroup()
  df <- df %>%
    arrange(ix) %>%
    mutate(s = ifelse(n>0,paste0(s,n),s))

  return(df$s)
}



.classcolors <- function(){
  # returns a vector of the five colours used to represent status classes
  c('#FF0000','#FFC000','#FFFF00','#92D050','#00B0F0')
}

# ---------- .eqr() function-----------------

#' calculates EQR for an indicator value:
#' val
#' given values for the indicator parameter corresponding to
#' status class boundaries:
#' Ref - Reference conditions (corresponds to EQR=1.0)
#' HG - the High/Good boundary               (EQR=0.8)
#' GM - the Good/Moderate boundary           (EQR=0.6)
#' MP - the Moderate/Poor boundary           (EQR=0.4)
#' PB - the Poor/Bad boundary                (EQR=0.2)
#' Worst - the worst possible condition      (EQR=0.0)

.eqr <- function(val, Ref, HG, GM, MP, PB, Worst){

  bnds <- c(Ref, HG, GM, MP, PB, Worst)
  if(any(is.na(bnds))){
    return(NA)
  }
  #class <- c("H","G","M","P","B")
  dir <- ifelse(bnds[1] < bnds[6], 1, -1)
  eqr <- c(1,0.8,0.6,0.4,0.2,0)
  if(dir<1){
    bnds <- rev(bnds)
    eqr <- rev(eqr)
  }
  n0 <- length(bnds[val>bnds])
  n1 <- n0 + 1
  n0 <- ifelse(n0<1, 1, n0)
  n1 <- ifelse(n1>6, 6, n1)
  eqr0 <- eqr[n0]
  eqr1 <- eqr[n1]
  eqr <- eqr0 + (eqr1 - eqr0) * (val - bnds[n0]) / (bnds[n1] - bnds[n0])
  eqr <- ifelse(val<=bnds[1], eqr0, eqr)
  eqr <- ifelse(val>bnds[6], eqr1, eqr)
  #classId <- ifelse(dir>0, 6-n0, n0)
  return(eqr)
}

# ------------ .classID() function -------------

#' gives the status class (1, 2, 3, 4, 5) for
#' a EQR value

.classID <- function(eqr){
  if(is.na(eqr)){
    return(NA)
  }else{
    bnds <- c(0,0.2,0.4,0.6,0.8)
    classID <- length(bnds[eqr>=bnds])
    return(classID)
  }
}

.class_names <- function(lang="en"){
  if(tolower(lang=="no")){
    class_names <- c("Svært dårlig", "Dårlig", "Moderat","God","Svært god")
  }else{
    class_names <- c("Bad", "Poor", "Mod","Good", "High")
  }
  class_names <- factor(class_names, levels=class_names)
  return(class_names)
}

eqr_results <- function(dfinds, dfbnds){

  keep_cols <- c("stn_code","index","n_total")
  res_cols <- c("EQR","n_norm", "pctG", "pctR","ESG12","pctOpp","sumG","sumB","pctB")

  class_names <- .class_names("no")

  dfinds <- dfinds %>%
    select(any_of(c(keep_cols, res_cols))) %>%
    pivot_longer(any_of(res_cols), names_to = "calc", values_to = "value")

  dfinds <- dfinds %>%
    left_join(dfbnds, by=c("index","calc"))

  dfinds$calc <- factor(dfinds$calc, levels = res_cols)

  dfinds <- dfinds %>%
    filter(!is.na(eqr00))

  dfinds <- dfinds %>%
    rowwise() %>%
    mutate(EQR=.eqr(value, eqr10, eqr08, eqr06, eqr04, eqr02, eqr00)) %>%
    mutate(classID = .classID(EQR)) %>%
    mutate(Class = class_names[classID]) %>%
    ungroup()

  dfinds <- dfinds %>%
    mutate(x=ifelse(n_total<14, ifelse(calc %in% c("ESG12","pctR"), "x", ""), ""))

  return(dfinds)

}

eqr_results_mean <- function(dfEQR){

  # 4. Hvis artsantallet er under 14 skal ikke EQR andel rødalger og ESG forhold benyttes i middelverdien.

  class_names <- .class_names("no")

  dfEQR <- dfEQR %>%
    filter(x!="x") %>%
    group_by(stn_code,index) %>%
    summarise(EQR=mean(EQR,na.rm = T), .groups = "drop")

  dfEQR <- dfEQR %>%
    rowwise() %>%
    mutate(classID = .classID(EQR)) %>%
    mutate(Class = class_names[classID]) %>%
    ungroup()

  return(dfEQR)
}


eqr_for_table <- function(dfEQR, dfstn){

  res_cols <- c("EQR","n_norm", "pctG", "pctR","ESG12","pctOpp","sumG","sumB","pctB")
  class_names <- .class_names("no")

  dfstn$calc <- "EQR"
  dfstn$calc <- factor(dfstn$calc,levels=res_cols)
  dfstn <- dfstn %>%
    mutate(description=paste0("EQR samlet resultat ", stn_code))

 dfEQR <- dfEQR %>%
    bind_rows(dfstn)

  dfEQR <- dfEQR %>%
    mutate(x=ifelse(is.na(x),"",x)) %>%
    mutate(note=ifelse(x=="x", paste0("artsantallet < 14 (EQR=", round(EQR,3), ")"), "")) %>%
    mutate(EQR=ifelse(x=="x",NA,EQR)) %>%
    mutate(classID=ifelse(x=="x",NA, classID)) %>%
    mutate(Class = class_names[classID])



  dfEQR <- dfEQR %>%
    select(index, stn_code, calc, description, value, eqr00, eqr02, eqr04, eqr06, eqr08, eqr10, EQR, classID, Class, note)

  dfEQR <- dfEQR %>%
    mutate(istn= stringr::str_extract(stn_code, "[0-9]+") %>% as.numeric()) %>%
    mutate(iix= stringr::str_extract(index, "[0-9]+") %>% as.numeric()) %>%
    arrange(iix, istn, calc) %>%
    select(-c(iix,istn))

  return(dfEQR)

}

# "AMBI"     "AMBI_rep" "matched"  "warnings"

# "Station" "AMBI"    "H"       "S"       "fNA"     "N"
# "I"       "II"      "III"     "IV"      "V"

# "Station" "Replicate" "AMBI"  "S"       "fNA"     "N"
# "I"       "II"      "III"     "IV"      "V"

# "Station"   "Replicate" "Species"   "Count"     "group"     "RA"

# Station Bounds AMBI H S x y z MAMBI EQR

excel_results <- function(ambi_res, mambi_res=NULL){

  wb <- createWorkbook()

  hs1 <- createStyle(textDecoration = "Bold")

  style_nr1 <- createStyle(numFmt = "0.0")
  style_nr3 <- createStyle(numFmt = "0.000")
  style_pct <- createStyle(numFmt = "0.00%")

#

  # -----------------------------------------
  id <- "AMBI"
  addWorksheet(wb, id)

  df <- ambi_res$AMBI
  nr <- 1 +nrow(df)

  writeData(wb, id, df, headerStyle = hs1)

  #openxlsx::setColWidths(wb, id, cols = 1:ncol(df), widths = "auto")
  colnames <- c("Station")
  for(icol in colnames){
    i <- which(tolower(names(df))==tolower(icol))
    openxlsx::setColWidths(wb, id, cols = i, widths = 8)
  }

  colnames <- c("AMBI", "H")
  for(icol in colnames){
    i <- which(tolower(names(df))==tolower(icol))
    openxlsx::addStyle(wb, id, style_nr3, cols = i, rows=(2:nr))
    openxlsx::setColWidths(wb, id, cols = i, widths = 8)
  }

  colnames <- c("N", "S")
  for(icol in colnames){
    i <- which(tolower(names(df))==tolower(icol))
    openxlsx::setColWidths(wb, id, cols = i, widths = 5)
  }

  colnames <- c("fNA","I", "II", "III", "IV","V" )
  for(icol in colnames){
    i <- which(tolower(names(df))==tolower(icol))
    openxlsx::setColWidths(wb, id, cols = i, widths = 8)
    openxlsx::addStyle(wb, id, style_pct, cols = i, rows=(2:nr))
  }


  df <- ambi_res$AMBI_rep

  if(!is.null(df)){
    id <- "AMBI Replicates"
    addWorksheet(wb, id)
    writeData(wb, id, df, headerStyle = hs1)
    nr <- 1 + nrow(df)

    #openxlsx::setColWidths(wb, id, cols = 1:ncol(df), widths = "auto")

    colnames <- c("Station", "Replicates")
    for(icol in colnames){
      i <- which(tolower(names(df))==tolower(icol))
      openxlsx::setColWidths(wb, id, cols = i, widths = 8)
    }

    colnames <- c("N", "S")
    for(icol in colnames){
      i <- which(tolower(names(df))==tolower(icol))
      openxlsx::setColWidths(wb, id, cols = i, widths = 5)
    }

    i <- which(tolower(names(df))==tolower("AMBI"))
    openxlsx::addStyle(wb, id, style_nr3, cols = i, rows=(2:nr))
    openxlsx::setColWidths(wb, id, cols = i, widths = 8)

    colnames <- c("fNA","I", "II", "III", "IV","V" )
    for(icol in colnames){
      i <- which(tolower(names(df))==tolower(icol))
      openxlsx::addStyle(wb, id, style_pct, cols = i, rows=(2:nr))
      openxlsx::setColWidths(wb, id, cols = i, widths = 8)
    }

  }

  df <- ambi_res$matched

  if(!is.null(df)){
    names(df)[names(df)=="source"] <- "Source"
    names(df)[names(df)=="group"] <- "Group"
    names(df)[names(df)=="group"] <- "Group"
    names(df)[names(df)=="group_note"] <- "Group Note"

    id <- "matched"
    addWorksheet(wb, id)
    writeData(wb, id, df, headerStyle = hs1)
    openxlsx::setColWidths(wb, id, cols = 1:ncol(df), widths = "auto")

  }

  df <- ambi_res$warnings

  if(!is.null(df)){
    id <- "warnings"
    addWorksheet(wb, id)
    writeData(wb, id, df, headerStyle = hs1)
    openxlsx::setColWidths(wb, id, cols = 1:ncol(df), widths = "auto")

  }

  df <- mambi_res$MAMBI
  if(!is.null(df)){

    names(df)[names(df)=="MAMBI"] <- "M-AMBI"

    id <- "M-AMBI"
    addWorksheet(wb, id)
    writeData(wb, id, df, headerStyle = hs1)
    nr <- 1 + nrow(df)

    icol <- which(tolower(names(df))==tolower("M-AMBI"))

    df2 <- mambi_res$bounds
    if(!is.null(df2)){
      names(df2)[names(df2)=="MAMBI"] <- "M-AMBI"
      writeData(wb, id, df2, headerStyle = hs1,
                startRow = nr+2, startCol = icol-1)
      nr <- nr + nrow(df2) + 3

    }

    colnames <- c("Station", "Bounds", "Status")
    for(icol in colnames){
      i <- which(tolower(names(df))==tolower(icol))
      openxlsx::setColWidths(wb, id, cols = i, widths = 8)
    }

    i <- which(tolower(names(df))==tolower("S"))
    openxlsx::setColWidths(wb, id, cols = i, widths = 5)

    colnames <- c("AMBI", "M-AMBI","H","EQR","x","y","z")
    for(icol in colnames){
      i <- which(tolower(names(df))==tolower(icol))
      openxlsx::addStyle(wb, id, style_nr3, cols = i, rows=(2:nr))
      openxlsx::setColWidths(wb, id, cols = i, widths = 8)
    }
  }

  # -----------------------------------------

  if(F){
  nr <- 1 + nrow(dfeqr)

  colnames <- c("EQR")
  for(icol in colnames){
    i <- which(tolower(names(dfeqr))==tolower(icol))
    openxlsx::addStyle(wb, id, style_nr3, cols = i, rows=(2:nr))
  }
  colnames <- c("value","eqr00","eqr02","eqr04","eqr06","eqr08","eqr10")
  for(icol in colnames){
    i <- which(tolower(names(dfeqr))==tolower(icol))
    openxlsx::addStyle(wb, id, style_nr1, cols = i, rows=(2:nr))
  }

  openxlsx::setColWidths(wb, id, cols = 1:ncol(dfobs), widths = "auto")
  freezePane(wb, id, firstRow = TRUE)

  # -----------------------------------------
  id <- "Indices"
  addWorksheet(wb, id)

  writeData(wb, id, dfi, headerStyle = hs1)

  nr <- 1 + nrow(dfi)

  colnames <- c("f","ESG12")
  for(icol in colnames){
    i <- which(tolower(names(dfi))==tolower(icol))
    openxlsx::addStyle(wb, id, style_nr3, cols = i, rows=(2:nr))
  }
  colnames <- c("pctG", "pctR","pctB","n_norm","pctOpp","sumB", "sumG")
  for(icol in colnames){
    i <- which(tolower(names(dfi))==tolower(icol))
    openxlsx::addStyle(wb, id, style_nr1, cols = i, rows=(2:nr))
  }

  # openxlsx::setColWidths(wb, id, cols = 1:ncol(dfi), widths = "auto")
  #
  i <- which(tolower(names(dfi))==tolower("f"))
  openxlsx::setColWidths(wb, id, cols = i, widths = 6)

  freezePane(wb, id, firstRow = TRUE)

  # -----------------------------------------
  id <- "Observations"
  addWorksheet(wb, id)

  dfobs <- dfobs %>%
    filter(!is.na(value))

  writeData(wb, id, dfobs, headerStyle = hs1)
  openxlsx::setColWidths(wb, id, cols = 1:ncol(dfobs), widths = "auto")


  freezePane(wb, id, firstRow = TRUE)
  }
  return(wb)
}



obs_indices <- function(df, dfstns){

  df <- df %>%
    mutate(Group =stringr::str_sub(Group,1,1)) %>%
    filter(!is.na(value))

  # Antall rød, grønn, brun

  dfnBGR <- df %>%
    distinct(stn_code, index, Group, taxaID) %>%
    group_by(stn_code, index, Group) %>%
    summarise(n = n(), .groups="drop")

  dfnBGR <- dfnBGR %>%
    pivot_wider(names_from = "Group", values_from = "n",
                names_prefix="n", names_sort = T)

  # Sum antall arter

  dfnTotal <- df %>%
    distinct(stn_code, index, taxaID) %>%
    group_by(stn_code, index) %>%
    summarise(n_total = n(), .groups="drop")

  # Antall ESG1, ESG2

  dfnESG <- df %>%
    filter(!is.na(ESG)) %>%
    distinct(stn_code, index, ESG, taxaID) %>%
    group_by(stn_code, index, ESG) %>%
    summarise(n = n(), .groups="drop")

  dfnESG <- df %>%
    distinct(stn_code, index) %>%
    merge(df %>% distinct(ESG), all=T) %>%
    left_join(dfnESG, by=c("stn_code","index","ESG"))

  dfnESG <- dfnESG %>%
    pivot_wider(names_from = "ESG", values_from = "n",
                names_prefix="nESG", names_sort = T)

  # Antall opportunist

  dfnOpport <- df %>%
    filter(!is.na(Opport.)) %>%
    distinct(stn_code, index, taxaID) %>%
    group_by(stn_code, index) %>%
    summarise(n_opp = n(), .groups="drop")

  dfnOpport <- df %>%
    distinct(stn_code, index) %>%
    left_join(dfnOpport, by=c("stn_code","index")) %>%
    mutate(n_opp = ifelse(is.na(n_opp), 0, n_opp ))

  # Sum forekomst brun, grønn

  dfsumBG <- df %>%
    filter(Group %in% c("B","G")) %>%
    group_by(stn_code, index, Group, taxaID) %>%
    summarise(max_val = max(value, na.rm=T), .groups="drop")

  dfsumBG <- dfsumBG %>%
    mutate(value_exp = value_rescale(max_val))

  dfsumBG <- dfsumBG %>%
    group_by(stn_code, index, Group) %>%
    summarise(sum=sum(value_exp,na.rm=T), .groups="drop")

  dfsumBG <- dfsumBG %>%
    pivot_wider(names_from = "Group", values_from = "sum",
                names_prefix="sum", names_sort = T)

  dfres <- dfstns %>%
    select(stn_code, points, f) %>%
    left_join(dfnTotal, by=c("stn_code")) %>%
    relocate(index, .after=1) %>%
    left_join(dfnBGR, by=c("stn_code","index")) %>%
    left_join(dfnESG, by=c("stn_code","index")) %>%
    left_join(dfnOpport, by=c("stn_code","index")) %>%
    mutate(pctG = 100*nG/n_total) %>%                   # %grønn/tot
    mutate(pctR = 100*nR/n_total) %>%                   # %rød/tot
    mutate(pctB = 100*nB/n_total) %>%                   # %rød/tot
    mutate(n_norm = f * n_total) %>%                    # Normalisert rikhet (ant arter)
    mutate(ESG12 = nESG1/nESG2) %>%                     # ESG1/ESG2
    mutate(pctOpp = 100*n_opp/n_total) %>%              # %opp/tot
    left_join(dfsumBG, by=c("stn_code","index"))        # Sum forekomst brun
                                                        # Sum forekomst grønn

  cols_rnd <- c("pctG", "pctR", "pctB","n_norm", "ESG12", "pctOpp", "sumB", "sumG")

  # dfres <- dfres %>%
  #   mutate(across(all_of(cols_rnd),  \(x) round(x, digits=1)))

  dfres <- dfres %>%
    mutate(istn= stringr::str_extract(stn_code, "[0-9]+") %>% as.numeric()) %>%
    mutate(iix= stringr::str_extract(index, "[0-9]+") %>% as.numeric()) %>%
    arrange(iix, istn) %>%
    select(-c(iix,istn))

  return(dfres)
}



# -------------- fjaerepotensial() -------------------------

fjaerepotensial <- function(poeng){
  # Felt- og beregningsmetodikk for komboindeksen (Makroalger) 28.11.2017
  poeng_sum <- 5:20
  f <- c(1.72, 1.65, 1.58, 1.51, 1.44, 1.36,
         1.29, 1.21, 1.14, 1.07, 1, 0.93,
         0.87, 0.8, 0.74, 0.69)
  ix <- length(poeng_sum[poeng_sum <= round(poeng,0)])
  ix <- max(ix,1)
  return(f[ix])
}

.value_rescale_single <- function(value){
  # rescale from 1-6 scale to exp(1-4)

  value <- ifelse(value<1, NA, value)
  value <- ifelse(value>=7, NA, value)
  if(is.na(value)){
    return(NA)
  }else{
    value = floor(value)
    values_14 <- c(1, 2, 2, 3, 3, 4)
    value <- values_14[value]
    value <- exp(value)
    return(value)
  }
}

value_rescale <- function(values){
 res <- lapply(values, .value_rescale_single) %>%
   unlist()
 return(res)
}

# ------------ species_interpreter() -------------------

.genus_chars <- function(){
  #' I have seen several versions for 'spp'
  #' - some of them probably typos
  #' if it occurs once in a list of names, we
  #' assume that all names are at genus level
  c("spp\\.", "ssp\\.", "sp\\.")
}

.incl_chars <-  function(){
  #' used when a group combines a genus and individual species
  #' e.g. Phyllophora spp. inkl. Coccotylus truncatus
  c("inkl\\.","ink\\.","incl\\.","inc\\." )
}


.add_spp <- function(s){
  # if not a species, add "spp." to the name
  if(!.is_species_single(s)){
    s <- paste0(stringr::str_trim(s)," spp.")
  }
  return(s)
}


split_taxa <- function(taxa_definition, return_list=F, sep_char=";", remove_comment=T){

  taxa_split <- lapply(taxa_definition, .split_taxa_single,
                       return_list=return_list, sep_char=sep_char,
                       remove_comment=remove_comment)
  if(return_list==F){
    taxa_split <- unlist(taxa_split)
  }
  return(taxa_split)
}


.split_taxa_single <- function(taxa_definition, return_list=F, sep_char=";", remove_comment=T){

  # by default return a semi-colon separated list as a single character variable
  # if return_list == T then function returns a vector of separate taxa

  s <- taxa_definition

  if(is.na(s)){
    return(NA_character_)
  }

  CORAX <- c("Calcareous encrusters (CORAX)",
             "Rød skorpeformet kalkalge")

  if(stringr::str_to_lower(s) %in% stringr::str_to_lower(CORAX)){
    return("CORAX")
  }

  if(remove_comment){
    # remove anything within "(......)"
    s <- stringr::str_remove_all(s, "\\([:print:]+\\)")
  }

  genus_level <- lapply(.genus_chars(), stringr::str_detect, string=s) %>%
    unlist() %>%
    max(na.rm=T)

  ix <- lapply(.incl_chars(), stringr::str_detect, string=s) %>%
    unlist()
  s_incl <- .incl_chars()[ix]

  #' there shouldn't been more than one version of "incl" text in a
  #' group definition but who knows...
  #'
  for(s_match in s_incl){
    s <- lapply(s, stringr::str_split_1, s_match) %>%
      unlist()
  }

  s <- lapply(s, stringr::str_split_1, "/") %>%
    unlist()

  # s <- stringr::str_split_1(taxa_definition, "/")
  s <- stringr::str_remove_all(s, "spp\\.")
  s <- stringr::str_remove_all(s, "ssp\\.")
  s <- stringr::str_remove_all(s, "sp\\.")
  s <- stringr::str_trim(s)
  if(genus_level){
    s <- s %>%
      lapply(.add_spp) %>%
      unlist()
  }
  if(!return_list){
    s <- paste0(s, collapse=sep_char)
  }
  return(s)
}


.is_species_single <- function(name){
  # check for a space in the name surrounded by characters
  # if there is a "/" character then NA is returned
  # the names should be split before checking if they are species
  for(s in .genus_chars()){
    name <- name %>%
      stringr::str_remove_all(s) %>%
      stringr::str_trim()
  }

  is_species <- stringr::str_detect(name, "[:alpha:]+ [:alpha:]+")
  is_species <- ifelse(stringr::str_detect(name, "/"), NA_character_, is_species)
  return(is_species)
}

is_species <- function(name){
  is_species <- lapply(name, .is_species_single) %>%
    unlist()
  return(is_species)
}

.get_genus_single <- function(name){


  if(is.na(name)){
    return(NA_character_)
  }else{

  for(s in .genus_chars()){
    name <- name %>%
      stringr::str_remove_all(s) %>%
      stringr::str_trim()
  }
  if(is_species(name)){
    name <- stringr::str_split_i(name," ", 1)
  }else{
    # already genus
  }
  return(name)
  }
}


get_genus <- function(name){
  genus <- lapply(name, .get_genus_single) %>%
    unlist()
  return(genus)
}

# ------------------------------------------------------
# matching species to species lists or groups of species / genus

.match_genus <- function(dfs, dfg){

  dfg <- dfg %>%
    mutate(genus=get_genus(taxa_split)) %>%
    # don't match if the genus table contains species
    mutate(genus=ifelse(is_species == T, NA_character_, genus))

  dfg <- dfg  %>%
    select(genus, taxaIDgenus=taxaID, index)
  dfg <- dfg %>%
    filter(!is.na(genus))

  dfs <- dfs %>%
    mutate(genus = split_taxa(Navn))
  dfs <- dfs %>%
    mutate(genus=get_genus(genus))

  dfs <- dfs %>%
    left_join(dfg, by=c("genus","index"),
              relationship = "many-to-many")
  return(dfs)
}

.match_species <- function(dfs1, dfs2){

  dfs2 <- dfs2  %>%
    select(taxa_split, taxaID, index) %>%
    filter(!is.na(taxa_split))
  dfs1 <- dfs1 %>%
    mutate(taxa_split = split_taxa(Navn)) %>%
    left_join(dfs2, by=c("taxa_split", "index"))

  return(dfs1)
}

.call_progress<- function(progress=NULL, val=1){
  if(!is.null(progress)){
    progress$set(value = val)
  }
}


match_obs_species_lists <- function(dfstns, dfobs, dfgrps, dftype, progress=NULL){

  .call_progress(progress, 2)

  dfobs <- dfobs %>%
    pivot_longer(cols=all_of(dfstns$stn_code), names_to = "stn_code", values_to = "value")

  dfobs <- dfobs %>%
    left_join(dftype, by=c("stn_code"="stn"))

  dfgrps <- dfgrps  %>%
    pivot_longer(cols=c(RSLA1,RSLA2,RSLA3,RSL4), names_to="index", values_to="Used") %>%
    filter(Used==TRUE)

  dfgrps0 <- dfgrps

  dfgrps <- dfgrps  %>%
    mutate(taxa_split = split_taxa(TAXA))

  dfgrps <- dfgrps  %>%
    separate_longer_delim(taxa_split, delim=";")

  dfgrps <- dfgrps  %>%
    mutate(is_species=is_species(taxa_split))

  .call_progress(progress, 3)
  df <- .match_species(dfobs, dfgrps)

  .call_progress(progress, 5)
  df <- .match_genus(df, dfgrps)

  df <- df %>%
    mutate(taxaID = ifelse(is.na(taxaID), taxaIDgenus, taxaID)) %>%
    select(-taxaIDgenus)
  .call_progress(progress, 7)

  df <- df%>%
    filter(!is.na(taxaID))

  df <- df %>%
    left_join(dfgrps0, by=c("taxaID","index"),
              relationship = "many-to-many")
  # df <- dfgrps0 %>%
  #   left_join(df, by=c("taxaID","index"),
  #             relationship = "many-to-many")

  df <- df %>%
    select(stn_code, index, Group, taxaID, Opport., ESG, TAXA, Kode, CF, SP, NB, Navn, value)
           #  taxa_split,  genus,   )
  df <- df %>%
    filter(!is.na(stn_code))


  df <- df %>%
    mutate(istn= stringr::str_extract(stn_code, "[0-9]+") %>% as.numeric()) %>%
    mutate(iix= stringr::str_extract(index, "[0-9]+") %>% as.numeric()) %>%
    arrange(iix, istn,taxaID) %>%
    select(-c(iix,istn))


  return(df)
}



# -------------- stn_points() -------------------------

station_points <- function(df, selected, options){

  if(is.null(selected)){
    return(NULL)
  }else{

    df <- df[selected,]
    df <- df %>%
      select(turbid_water, sand_scouring, ice_scouring,
             ravine, fractured, boulders,
             steep, unspec_hard, rocks, shingle,
             shallow_pool, large_pool, deep_pool,
             small_pool, cave, overhang,
             other_sub,
             points_adjust)

    df <- df %>%
      pivot_longer(cols=1:ncol(df), names_to = "Parameter", values_to="Points")

    df <- df %>%
      .add_param_names(options)
    return(df)
  }

}

# -------------- species_data() -------------------------

species_data <- function(df, dfstns, options){

  if(!is.null(df)){

    col1 <- df[,1] %>%
      unlist()

    col_stn_first <- min(dfstns$col)
    col_max <- max(dfstns$col)


    row_match <- match(options$species_header, col1)
    row_head <- df[row_match,] %>%
      unlist()

    df <- df %>%
      mutate(rowid=row_number())

    df <- df[(row_match+1):nrow(df),1:col_max]

    col_names <- row_head[1:(col_stn_first-1)]
    col_names <- c(col_names, dfstns$stn_code)

    names(df) <- col_names

    df <- df %>%
      mutate(across(all_of(dfstns$stn_code), as.numeric))

    return(df)
  }else{
    return(NULL)
  }

}




# -------------- add_param_names() -------------------------

.add_param_names <- function(df, options){

  df <- df %>%
    rowwise() %>%
    mutate(group=.parameter_group(Parameter, options)) %>%
    mutate(name=.parameter_name(Parameter, options)) %>%
    ungroup() %>%
    relocate(name, .before=1) %>%
    relocate(group, .before=1)

  return(df)
}


# -------------- parameter_group() -------------------------

.parameter_group <- function(Parameter, options){

  grps <- names(options)
  val <- NA_character_
  if(Parameter == "points_adjust"){
    val <- "Poeng justering"
  }else if(Parameter == "points"){
    val <- "Total"
  }else{
    for(grpi in grps){
      suboptions <- options[[grpi]]
      i <- 1
      for(item in names(suboptions)){
        if(i==1){
          val_grp <- suboptions[[item]][["row_name"]]
        }
        i <- i + 1
        if(item == Parameter){
          val <- val_grp
        }
      }
    }
  }
  return(val)
}


# -------------- parameter_name() -------------------------

.parameter_name <- function(Parameter, options){
  #
  grps <- names(options)
  val <- NA_character_
  if(Parameter == "points_adjust"){
    val <- "Poeng justering"
  }else if(Parameter == "points_adjust"){
    val <- "Poeng"
  }else{
    for(grpi in grps){
      suboptions <- options[[grpi]]
      for(item in names(suboptions)){
        if(item == Parameter){
          val <- suboptions[[item]][["row_name"]]
        }
      }
    }
  }
  return(val)
}


# -------------- observation_info() -------------------------

observation_info <- function(df, options,
                             opts=c("species_header","points_adjust")){

  df_stns <- data.frame()
  grps <- names(options)[!names(options) %in% opts]
  for(grpi in grps){
    dfi <- .observation_info_grp(df, options, grpi)
    if(nrow(df_stns)>0){
      df_stns <- df_stns %>%
        left_join(dfi, by="col")
    }else{
      df_stns <- dfi
      ncol1 <- ncol(dfi) + 1
    }
  }

  points_adjust <- ifelse(is.null(options$points_adjust),0,
                          options$points_adjust)
  df_stns <- df_stns %>%
    mutate(points_adjust = points_adjust)

  ncol2 <- ncol(df_stns)

  df_stns <- df_stns %>%
    mutate(points = rowSums(across(any_of(ncol1:ncol2)), na.rm=T ))

  df_stns <- df_stns %>%
    rowwise() %>%
    mutate(f = fjaerepotensial(points)) %>%
    ungroup()

  return(df_stns)
}


# -------------- observation_info_grp() -------------------------

.observation_info_grp <- function(df, options, info_group){

  sub_options <- options[[info_group]]

  info_names <- names(sub_options)

  info_rows_ix <- paste0("row_ix_", info_names)
  info_rows <- lapply(sub_options, function(x) x[["row_name"]]) %>% unlist()

  info_points <- lapply(sub_options, function(x) x[["points"]])

  row_names <- df %>%
    purrr::pluck(1)

  row_ix <- info_rows %>%
    purrr::map(match, row_names) %>%
    unlist()

  if(sum(is.na(row_ix))>0){
    msg <- info_rows[is.na(row_ix)] %>%
      paste0(collapse=";")
    msg <- paste0("missing rows: ",msg)
    cli::cli_warn(msg)
  }

  dfobs <- data.frame()
  for(i in 1:length(info_names)){

    iname <- info_names[i]
    if(iname != info_group){
    #irow <- row_ix[match(info_rows[i],row_names)]
    irow <- match(info_rows[i],row_names)
    assign(info_rows_ix[i], irow)
    dfi <- .stn_param_values(irow, df, name=info_names[i], points=info_points[[i]])

    if(nrow(dfobs)>0){
      dfobs <- dfobs  %>%
        full_join(dfi, by="col")
    }else{
      dfobs <- dfi
    }
    }
  }

  return(dfobs)
}


# -------------- stn_type_points() -------------------------

.stn_type_points <- function(recorded, points=c(0)){
  recorded <- as.numeric(recorded)
  recorded <- ifelse(is.na(recorded),0,recorded)
  recorded <- ifelse(recorded>0,1,recorded)
  value <- ifelse(length(points)==1,
                  ifelse(recorded==1, points, 0),
                  ifelse(recorded==0, points[1], points[2]))
  return(value)
}


# -------------- stn_param_values() -------------------------

.stn_param_values <- function(row_ix, df, name="value", type="character", points=NULL, first_col=3){

  type <- ifelse(stringr::str_detect(name, "date"), "date", type)
  type <- ifelse(stringr::str_detect(name, "time"), "time", type)
  type <- ifelse(is.null(points), type, "points")

  res  <- df[row_ix,first_col:ncol(df)] %>%
    pivot_longer(cols=everything(), names_to = "col", values_to = "value") %>%
    mutate(col=stringr::str_extract(col, "[\\d]+")) %>%
    mutate(col=as.numeric(col))

  res <- res %>%
    filter(!is.na(value))


  if(type=="date"){
    res <- res %>%
      mutate(value=as.numeric(value)) %>%
      mutate(value=openxlsx::convertToDateTime(value))
  }else if(type=="time"){
    res <- res %>%
      mutate(value=as.numeric(value)) %>%
      mutate(value=openxlsx::convertToDateTime(value)) %>%
      mutate(value = format.POSIXct(value, "%H:%M"))
  }else if(type=="points"){
    res <- res %>%
      rowwise() %>%
      mutate(value=.stn_type_points(value, points)) %>%
      ungroup()

  }

  names(res)[names(res)=="value"] <- name
  return(res)
}

# -------------- read_excel_sheet() -------------------

.repair_nms <- function(nms){
  nms <- 1:length(nms)
  nms <- paste0("_", nms)
  return(nms)
}

read_excel_sheet <- function(sheet, xl_path, header=F){
  require(readxl)

  df <- suppressMessages(readxl::read_excel(sheet=sheet, path=xl_path,
                            col_names=header, col_types = "text",
                            .name_repair = "unique"))

  return(df)
}

# -------------- read_excel_all() -------------------

read_excel_all <- function(xl_path, header=F, progress=NULL){
  require(readxl)
  sheets <- readxl::excel_sheets(xl_path)
  list_df <- list()
  header <- ifelse(is.null(header),T,header)
  is <- 0
  for(i in 1:length(sheets)){
    df <- read_excel_sheet(sheets[i], xl_path,header=header)
    # only add sheets which are not empty
    if(nrow(df)>0){
      is <- is+1
      list_df[[is]] <- df
      names(list_df)[is] <- sheets[i]
    }
    if(!is.null(progress)){
      progress$set(value = i+1)
    }
  }
  #list_df <- lapply(sheets, read_excel_sheet, xl_path)
  # names(list_df) <- sheets
  return(list_df)
}


