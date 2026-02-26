library(RWDataPlyr)
library(tidyverse)
library(bench)

scen_path <- '\\\\manoa.colorado.edu\\bor\\Shared\\P26\\WYConcept_Nov2025\\Scenarios_10.30.2025'
scens <- c(
  '2016Dems,CRMMSTrace3,SuperEnsembleV3,OMP9004.mdl,9002.rls,PCurveWY,LDSelev1.5,ConOff.UDSPIP',
  '2016Dems,CRMMSTrace3,SuperEnsembleV3,OMP9004.mdl,9002.rls,PCurveWY,LDSper1.5,UBActOff',
  '2016Dems,CRMMSTrace10,SuperEnsembleV3,OMP9004.mdl,9002.rls,PCurveWY,LDSper1.5,ConOff.UDSPIP'
)
rdfs <- c('KeySlots.rdf', 'OWDAnn.rdf', 'xtraRes.rdf', "AllRes.rdf")
rdfs <- rdfs[4]

results <- bench::mark(
  rdf_to_rwtbl2 = {
    # all in memory ----------------------------------
    df <- data.frame()
    for (ss in scens) {
      for (rr in rdfs) {
        tmp <- rdf_to_rwtbl2(file.path(scen_path, ss, rr), scenario = ss)
        
        if (rr == 'KeySlots.rdf') {
          t2 <- bind_rows(
            filter(tmp, ObjectSlot == 'Powell.Pool Elevation') |>
              group_by(Scenario, TraceNumber, Year) |>
              summarise(Value = min(Value)) |>
              mutate(ObjectSlot = 'min_powell'),
            filter(tmp, ObjectSlot == 'Mead.Pool Elevation', Month == 'December') |>
              mutate(Value = as.numeric(Value <= 1035), ObjectSlot = 'mead_dec_lt_1035'),
            filter(tmp, ObjectSlot == 'TotVal.Powell') |>
              group_by(Scenario, TraceNumber, Year) |>
              summarise(Value = sum(Value)) |>
              mutate(ObjectSlot = 'lf_annual')
          )
        } else if (rr == 'OWDAnn.rdf') {
          t2 <- filter(tmp, ObjectSlot %in% 
                         c("LBHydrologicShortage.AnnualTotalShortage",
                           "LBHydrologicShortage.AnnualPolicyShortage" ))
        } else if (rr == 'AllRes.rdf') {
          t2 <- bind_rows(
            filter(tmp, ObjectSlot == "BlueMesa.Outflow") |>
              group_by(Scenario, TraceNumber, Year) |>
              summarise(Value = sum(Value)) |>
              mutate(ObjectSlot = 'bm_ann_release'),
            filter(tmp, ObjectSlot == "Powell.Spill") |>
              group_by(Scenario, TraceNumber, Year) |>
              summarise(Value = sum(Value)) |>
              mutate(Value = as.numeric(Value > 0), ObjectSlot = 'powell_ann_spill'),
            filter(tmp, ObjectSlot == "Havasu.Inflow") |>
              group_by(Scenario, TraceNumber, Year) |>
              summarise(Value = mean(Value)) |>
              mutate(ObjectSlot = 'avg_havasu_inflow')
          )
        } else {
          t2 <- bind_rows(
            filter(tmp, ObjectSlot == "Mead.Energy") |>
              group_by(Scenario, TraceNumber, Year) |>
              summarise(Value = sum(Value)) |>
              mutate(ObjectSlot = 'mead_ann_energy'),
            filter(tmp, ObjectSlot == "Powell.Spill") |>
              group_by(Scenario, TraceNumber, Year) |>
              summarise(Value = sum(Value)) |>
              mutate(Value = as.numeric(Value > 0), ObjectSlot = 'powell_ann_spill'),
            filter(tmp, ObjectSlot == "Havasu.Inflow") |>
              group_by(Scenario, TraceNumber, Year) |>
              summarise(Value = mean(Value)) |>
              mutate(ObjectSlot = 'avg_havasu_inflow')
          )
        }
        
        df <- bind_rows(df, t2)
      }
    }
    df
  },
  
  bigrdf_to_rwtbl = {
    # as arrow file system ---------------------------
    df2 <- data.frame()
    for (ss in scens) {
      for (rr in rdfs) {
        tmp <- bigrdf_to_rwtbl(file.path(scen_path, ss, rr), scenario = ss)
        
        if (rr == 'KeySlots.rdf') {
          t2 <- bind_rows(
            filter(tmp, ObjectSlot == 'Powell.Pool Elevation') |>
              group_by(Scenario, TraceNumber, Year) |>
              summarise(Value = min(Value)) |>
              mutate(ObjectSlot = 'min_powell') |>
              collect(),
            filter(tmp, ObjectSlot == 'Mead.Pool Elevation', Month == 'December') |>
              mutate(Value = as.numeric(Value <= 1035), ObjectSlot = 'mead_dec_lt_1035') |>
              collect(),
            filter(tmp, ObjectSlot == 'TotVal.Powell') |>
              group_by(Scenario, TraceNumber, Year) |>
              summarise(Value = sum(Value)) |>
              mutate(ObjectSlot = 'lf_annual') |>
              collect()
          )
        } else if (rr == 'OWDAnn.rdf') {
          t2 <- filter(tmp, ObjectSlot %in% 
                         c("LBHydrologicShortage.AnnualTotalShortage",
                           "LBHydrologicShortage.AnnualPolicyShortage" )) |>
            collect()
        } else {
          t2 <- bind_rows(
            filter(tmp, ObjectSlot == "Mead.Energy") |>
              group_by(Scenario, TraceNumber, Year) |>
              summarise(Value = sum(Value)) |>
              mutate(ObjectSlot = 'mead_ann_energy') |>
              collect(),
            filter(tmp, ObjectSlot == "Powell.Spill") |>
              group_by(Scenario, TraceNumber, Year) |>
              summarise(Value = sum(Value)) |>
              mutate(Value = as.numeric(Value > 0), ObjectSlot = 'powell_ann_spill') |>
              collect(),
            filter(tmp, ObjectSlot == "Havasu.Inflow") |>
              group_by(Scenario, TraceNumber, Year) |>
              summarise(Value = mean(Value)) |>
              mutate(ObjectSlot = 'avg_havasu_inflow') |>
              collect()
          )
        }
        
        df2 <- bind_rows(df2, t2)
      }
    }
    df2
  },
  iterations = 5, check = FALSE
)

results

# this is for all 3 rdfs
# # A tibble: 2 × 13
# expression          min median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory     time       gc      
# <bch:expr>      <bch:t> <bch:>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list>     <list>     <list>  
#   1 rdf_to_rwtbl2     1.06m  1.08m   0.0155     4.41GB    0.281     5    91      5.39m <NULL> <Rprofmem> <bench_tm> <tibble>
#   2 bigrdf_to_rwtbl   1.86m  1.89m   0.00857    2.94GB    0.139     5    81      9.73m <NULL> <Rprofmem> <bench_tm> <tibble>

# just key slots ~ 29 MB
# A tibble: 2 × 13
# A tibble: 2 × 13
#   expression          min median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory     time       gc      
#   <bch:expr>      <bch:t> <bch:>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list>     <list>     <list>  
# 1 rdf_to_rwtbl2     13.2s    15s    0.0683    1.35GB    0.478     5    35      1.22m <NULL> <Rprofmem> <bench_tm> <tibble>
# 2 bigrdf_to_rwtbl   26.9s  29.9s    0.0338  896.43MB    0.155     5    23      2.47m <NULL> <Rprofmem> <bench_tm> <tibble>

# just xtra res ~ 65 MB
# A tibble: 2 × 13
# expression          min median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory     time       gc      
# <bch:expr>      <bch:t> <bch:>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list>     <list>     <list>  
#   1 rdf_to_rwtbl2    36.56s 39.51s    0.0257    2.97GB    0.252     5    49      3.25m <NULL> <Rprofmem> <bench_tm> <tibble>
#   2 bigrdf_to_rwtbl   1.13m  1.16m    0.0143    1.97GB    0.109     5    38      5.81m <NULL> <Rprofmem> <bench_tm> <tibble>

# just OWDAnn ~ 1 MB
# A tibble: 2 × 13
# expression        min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory     time       gc      
# <bch:expr>   <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list>     <list>     <list>  
#   1 rdf_to_rwtb… 344.62ms 357.95ms     2.75     30.1MB    0.549     5     1      1.82s <NULL> <Rprofmem> <bench_tm> <tibble>
#   2 bigrdf_to_r…    3.76s    3.97s     0.249      27MB    0.597     5    12      20.1s <NULL> <Rprofmem> <bench_tm> <tibble>

# just AllRes ~230 MB
# A tibble: 2 × 13
# expression          min median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory     time       gc      
# <bch:expr>      <bch:t> <bch:>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list>     <list>     <list>  
#   1 rdf_to_rwtbl2     6.29m  6.74m   0.00234    11.2GB   0.0248     5    53      35.6m <NULL> <Rprofmem> <bench_tm> <tibble>
#   2 bigrdf_to_rwtbl   4.45m  4.69m   0.00349    7.54GB   0.0475     5    68      23.9m <NULL> <Rprofmem> <bench_tm> <tibble>

