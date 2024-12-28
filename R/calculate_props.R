# Function to calculate proportions and confidence intervals
calculate_props <- function(data, var) {
  data %>%
    dplyr::summarise(
      N_total = sum(!is.na(!!sym(var))),
      n_endorsed = sum(!!sym(var) == 1, na.rm = TRUE),
      prop = mean(!!sym(var), na.rm = TRUE),
      loci_p = ifelse(N_total > 0, prop.test(n_endorsed, N_total, conf.level = 0.95)$conf.int[1], NA),
      hici_p = ifelse(N_total > 0, prop.test(n_endorsed, N_total, conf.level = 0.95)$conf.int[2], NA),
      .groups = 'drop' #this will ungroup immediately within the summarise. Using here instead of explicitly calling ungroup()
    ) %>%
    dplyr::mutate(across(c('prop', 'loci_p', 'hici_p'), ~ round(. * 100, 1))) %>%
    dplyr::mutate(pct_ci = paste0('(', loci_p, ', ', hici_p, ')')) %>%
    dplyr::rename(pct = prop,
           pct_loci = loci_p,
           pct_hici = hici_p) %>%
    dplyr::select(!c(pct_loci, pct_hici))

}
