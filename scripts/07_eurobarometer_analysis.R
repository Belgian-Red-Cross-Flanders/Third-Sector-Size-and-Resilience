# Vector of EU countries according to master
eu_countries <- c(
  "Austria","Belgium","Bulgaria","Croatia","Cyprus","Czechia","Denmark",
  "Estonia","Finland","France","Germany","Greece","Hungary","Ireland",
  "Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands",
  "Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden"
)

eu_df <- master %>%
  filter(Country %in% eu_countries) %>%
  dplyr::select(
    Country,
    Total_tpt,
    volunteers_per_100k_ifrc_2024,
    volunteers_staff_per_100k_ifrc_2024,
    volunteer_sp547_eurobar,
    family_friends_sp547_eurobar,
    neighbors_sp547_eurobar,
    nonprofit_sp547_eurobar,
    emergency_sp547_eurobar,
    authorities_sp547_eurobar,
    work_sp547_eurobar,
    private_sp547_eurobar,
    trust_es_authorities_sp547_eurobar,
    effective_eu_fl546_eurobar,
    coping_capacity_inform,
    readiness_ndgain,
    adaptive_capacity_ndgain,
    natural_hazard_inform,
    log1p_affected_per_100k_owid_15_24_sum_owid
  )

#  test whether structural volunteering (Total_tpt) and organizational volunteering (IFRC volunteers) align with self‑reported volunteering (SP547).
cor_analysis <- eu_df %>%
  summarize(
    cor_tpt_eurobar = cor(Total_tpt, volunteer_sp547_eurobar, use = "complete.obs"),
    cor_rcvol_eurobar = cor(volunteers_per_100k_ifrc_2024, volunteer_sp547_eurobar, use = "complete.obs"),
    cor_tpt_rcvol = cor(Total_tpt, volunteers_per_100k_ifrc_2024, use = "complete.obs"),
    cor_tpt_rcvolstaff = cor(Total_tpt, volunteers_staff_per_100k_ifrc_2024, use = "complete.obs")
  )
cor_analysis