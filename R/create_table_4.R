### Table e-2
create_table_e_2 <- function (data,
                            columns) {
  
  te2 <- data %>% 
    select(columns) %>% 
    group_by(trends) %>%
    summarise_if(is.numeric, 
                 list(~ sum(., na.rm = TRUE), 
                      ~ median(., na.rm = TRUE), 
                      ~ IQR(., na.rm = TRUE)))
  
  ### getting descriptive stats for all variables
  counts <- names(te2)[grep("sum", names(te2))]
  medians <- names(te2)[grep("median", names(te2))]
  iqrs <- names(te2)[grep("IQR", names(te2))]
  table_e2 <- matrix(nrow = nrow(te2))
  
  for (i in seq_along(medians)) {
    
    #### looking for NA values and turning them into spaces
    .counts <- which(names(te2) == counts[i])
    .med <- which(names(te2) == medians[i])
    .iqr <- which(names(te2) == iqrs[i])
    
    table_e2 <- cbind(
      table_e2,
      cbind(
        paste0(round(as.numeric(unlist(te2[, .counts])), 2), ", ",
               round(as.numeric(unlist(te2[, .med])), 2), " (",
               paste0(round(as.numeric(unlist(te2[, .iqr])), 2), ")"))))
    table_e2[grep("NA", table_e2[, i + 1]), i + 1] = ""
    
  }
  
  table_e2 <- table_e2[, -1]
  table_e2 <- t(table_e2)
  
  table_e2 <- rbind(
    get_stats(data, id),
    get_stats(data, gender_w1, "male"),
    get_stats(data, educ_binary, "secondary or more"),
    get_stats(data, marital_status_binary_w1, "married"),
    get_stats(data, disab_prestroke, "some disab"),
    table_e2)
  colnames(table_e2) <- unlist(te2[, 1])
  
  ####rownames
  table_e2 <- cbind(Variable = c(
    "n (%)", "Males (%)", "Education",
    "Marital status", "mrs prestroke",
    trimws(gsub("median|t0|.y|_", " ", medians))
  ), table_e2)
  
  ### creating space between assessments
  cols <- c("stolic bp", 'moca', 'mmse', 'stroop',
            "ravens", 'time taken', 'lds', 'madrs',
            'nihss', 'barthel', 'mrs score', 
            'aerobic', 'acs', 'wsas', 'sis')
  .te2 <- matrix(ncol = ncol(table_e2))
  
  grab <- 1
  
  for (i in seq_along(cols)) {
    
    space <- first(grep(cols[i], table_e2[, 1])) - 1
    .te2 <- rbind(.te2, table_e2[grab:space, ], "")
    grab <- first(grep(cols[i], table_e2[, 1]))
    
  }
  
  #### adding the last category
  .te2 <- rbind(.te2[-1, ], 
               table_e2[grep("sis", table_e2[, 1]), ])

}

##### binding count variable stats
get_stats <- function (x, var, categ = NULL) {
  
  if (!is.null(categ)) {
    ### mapping zeroes
    miss <- cbind(x %>% select(trends), 
                  (x %>% select({{var}})) == {{categ}}) %>% 
      table %>%
      data.frame() %>% 
      filter({{var}} == TRUE, Freq == 0) %>% 
      select(trends, Freq)
    
    names(miss) <- c("trends","categ")
    
    #### getting values
    .nums <- x %>% 
      filter({{var}} == {{categ}}) %>%
      group_by(trends) %>% 
      summarize(categ = n()) %>% 
      rbind(miss) %>%
      arrange(trends) %>% 
      select(categ) %>% 
      unlist
    
    paste0(.nums, ' (', 
           round(.nums / nrow(x) * 100, 2),
           "%)")
    
  } else {
    
    paste0(
      x %>% 
        group_by(trends) %>% 
        select(trends) %>%
        summarize(var = n()) %>% 
        select(var) %>% 
        unlist(),
      " (",
      round(
        ((x %>% group_by(trends) %>% select(trends) %>%
            summarize(var = n()) %>% select(var) %>% 
            unlist()) / nrow(x)) * 100, 2),
      "%)")
  }
}
