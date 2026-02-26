library(RWDataPlyr)
library(tidyverse)
library(bench)

scen_path <- '\\\\manoa.colorado.edu\\bor\\Shared\\P26\\WYConcept_Nov2025\\Scenarios_10.30.2025'
scens <- c(
  '2016Dems,CRMMSTrace3,SuperEnsembleV3,OMP9004.mdl,9002.rls,PCurveWY,LDSelev1.5,ConOff.UDSPIP',
  '2016Dems,CRMMSTrace3,SuperEnsembleV3,OMP9004.mdl,9002.rls,PCurveWY,LDSper1.5,UBActOff',
  '2016Dems,CRMMSTrace10,SuperEnsembleV3,OMP9004.mdl,9002.rls,PCurveWY,LDSper1.5,ConOff.UDSPIP'
)
rdfs <- c('KeySlots.rdf', 'OWDAnn.rdf', 'xtraRes.rdf')

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

# # A tibble: 2 × 13
# expression          min median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory     time       gc      
# <bch:expr>      <bch:t> <bch:>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list>     <list>     <list>  
#   1 rdf_to_rwtbl2     1.06m  1.08m   0.0155     4.41GB    0.281     5    91      5.39m <NULL> <Rprofmem> <bench_tm> <tibble>
#   2 bigrdf_to_rwtbl   1.86m  1.89m   0.00857    2.94GB    0.139     5    81      9.73m <NULL> <Rprofmem> <bench_tm> <tibble>
