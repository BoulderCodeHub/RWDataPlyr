//============================================================================
// Name        : rwdataplyr.cpp
// Author      : Alan Butler
// Version     :
// Copyright   : CC0
// Description : Optimized version (thanks Chat GPT)
//============================================================================

#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

// Preallocate memory and parse RDF metadata
std::vector<std::vector<std::string>> parse_rdf_meta(const std::vector<std::string>& rdf) {
  std::vector<std::vector<std::string>> meta;
  std::vector<std::string> row(2);
  std::string line, token;
  size_t i = 0;
  
  meta.reserve(10); // Assuming max of 10 meta rows for efficiency
  
  while (rdf[i] != "END_PACKAGE_PREAMBLE") {
    line = rdf[i++];
    size_t pos = line.find(":");
    
    if (pos != std::string::npos) {
      row[0] = line.substr(0, pos);
      row[1] = line.substr(pos + 1);
      meta.push_back(row);
    }
  }
  
  return meta;
}

// Extract year and month efficiently
std::vector<std::string> get_year_month(const std::string& timestep) {
  std::vector<std::string> ym(2);
  static const std::vector<std::string> months = {
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
  };
  
  size_t first_dash = timestep.find("-");
  size_t second_dash = timestep.find("-", first_dash + 1);
  
  if (first_dash != std::string::npos && second_dash != std::string::npos) {
    ym[0] = timestep.substr(0, 4); // Year
    int month_index = std::stoi(timestep.substr(first_dash + 1, second_dash - first_dash - 1)) - 1;
    ym[1] = (month_index >= 0 && month_index < 12) ? months[month_index] : "Unknown";
  }
  
  return ym;
}

std::vector<std::string> parse_line(const std::string& line) {
  std::vector<std::string> row(2);
  
  size_t pos = line.find(":");
  if (pos != std::string::npos) {
    row[0] = line.substr(0, pos);
    
    // Trim leading whitespace for the value
    size_t start = line.find_first_not_of(" \t", pos + 1);
    row[1] = (start != std::string::npos) ? line.substr(start) : "";
  }
  
  return row;
}

// Optimized RDF parsing
std::vector<std::vector<std::string>> parse_rdf(const std::vector<std::string>& rdf, int num_traces, String const scenario) {
  size_t ncol = 13;
  if (scenario != NA_STRING) {
    ncol = 14;
  }
  
  std::vector<std::vector<std::string>> table(ncol);
  size_t i = 0;
  int trace_count = 0;
  
  // Preallocate memory
  size_t estimated_size = rdf.size() / 10;
  for (auto& col : table) {
    col.reserve(estimated_size);
  }
  
  while (rdf[i] != "END_PACKAGE_PREAMBLE") ++i;
  ++i; // Skip END_PACKAGE_PREAMBLE
  
  while (trace_count < num_traces) {
    ++trace_count;
    
    std::vector<std::string> timesteps, year, month;
    std::string slot_set, rule_set, trace_number;
    size_t num_time_steps = 0;
    
    while (rdf[i] != "END_RUN_PREAMBLE") {
      std::vector<std::string> row = parse_line(rdf[i++]);
      
      if (row[0] == "time_steps") {
        num_time_steps = std::stoi(row[1]);
      } else if (row[0] == "slot_set") {
        slot_set = row[1];
      } else if (row[0] == "rule_set") {
        rule_set = row[1];
      } else if (row[0] == "trace_num" || row[0] == "trace") {
        trace_number = row[1];
      } 
    }
    ++i; // Skip END_RUN_PREAMBLE
    
    if (trace_number.empty()) {
      trace_number = std::to_string(trace_count);
    }
    
    timesteps.reserve(num_time_steps);
    year.reserve(num_time_steps);
    month.reserve(num_time_steps);
    
    for (size_t j = 0; j < num_time_steps; ++j) {
      timesteps.push_back(rdf[i]);
      auto ym = get_year_month(rdf[i++]);
      year.push_back(ym[0]);
      month.push_back(ym[1]);
    }
    
    while (rdf[i] != "END_RUN") {
      std::vector<std::string> vals, row;
      std::string obj, slot, obj_slot, object_type, units, scale;
      
      while (rdf[i] != "END_SLOT_PREAMBLE") {
        row = parse_line(rdf[i++]);
        if (row[0] == "object_name") obj = row[1];
        else if (row[0] == "slot_name") slot = row[1];
        else if (row[0] == "object_type") object_type = row[1];
      }
      ++i; // Skip END_SLOT_PREAMBLE
      
      
      
      while (rdf[i] != "END_SLOT") {
        row = parse_line(rdf[i]);
        if (row[0] == "units") units = row[1];
        else if (row[0] == "scale") scale = row[1];
        else if (rdf[i] != "END_COLUMN") vals.push_back(rdf[i]);
        ++i;
      }
      
      obj_slot = obj + "." + slot;
      
      for (size_t k = 0; k < num_time_steps; ++k) {
        table[0].push_back(timesteps[k]);
        table[1].push_back(vals[k]);
        table[2].push_back(year[k]);
        table[3].push_back(month[k]);
        table[4].push_back(obj);
        table[5].push_back(slot);
        table[6].push_back(obj_slot);
        table[7].push_back(trace_number);
        table[8].push_back(slot_set);
        table[9].push_back(rule_set);
        table[10].push_back(object_type);
        table[11].push_back(units);
        table[12].push_back(scale);
        
        if (scenario != NA_STRING) {
          table[13].push_back(scenario);
        }
      }
      ++i; // Skip END_SLOT
    }
    ++i; // Skip END_RUN
  }
  
  return table;
}

// Extracts the number of runs from the RDF metadata
int get_n_runs(const std::vector<std::vector<std::string>>& meta) {
  for (const auto& row : meta) {
    if (row[0] == "number_of_runs") {
      return std::stoi(row[1]);
    }
  }
  Rcpp::stop("Error: 'number_of_runs' not found in metadata.");
  return 0; // This line will never be reached
}

// [[Rcpp::export]]
List rdf_to_rwtbl_cpp(std::vector<std::string> rdf, std::vector<std::string> keep_cols, String const scenario = NA_STRING, bool add_ym = true) {
  auto meta = parse_rdf_meta(rdf);
  int num_runs = get_n_runs(meta);
  auto rwtbl = parse_rdf(rdf, num_runs, scenario);
  
  size_t nn = rwtbl[0].size();
  
  std::vector<std::string> col_names = {
    "Timestep", "Value", "Year", "Month",
    "ObjectName", "SlotName", "ObjectSlot",
    "TraceNumber", "SlotSet", "RuleSet",
    "ObjectType", "Unit", "Scale"
  };
  
  // Conditionally add the 'Scenario' column if provided
  if (scenario != NA_STRING) {
    col_names.push_back("Scenario");
    keep_cols.push_back("Scenario");
  }
  
  // Ensure that 'Year' and 'Month' are included if add_ym is TRUE
  if (add_ym) {
    keep_cols.push_back("Year");
    keep_cols.push_back("Month");
  }
  
  // Identify which columns to keep (preserve original order in rwtbl)
  std::vector<int> col_indices;
  std::vector<std::string> final_col_names;
  
  for (size_t i = 0; i < col_names.size(); ++i) {
    if (std::find(keep_cols.begin(), keep_cols.end(), col_names[i]) != keep_cols.end()) {
      col_indices.push_back(i);
      final_col_names.push_back(col_names[i]);
    }
  }
  
  // Create a new data frame with only the selected columns
  List filtered_rwtbl(col_indices.size());
  for (size_t i = 0; i < col_indices.size(); ++i) {
    filtered_rwtbl[i] = rwtbl[col_indices[i]];
  }
  
  // Apply attributes to the data frame
  filtered_rwtbl.attr("names") = wrap(final_col_names);
  filtered_rwtbl.attr("stringsAsFactors") = false;
  filtered_rwtbl.attr("class") = "data.frame";
  filtered_rwtbl.attr("row.names") = IntegerVector::create(NA_INTEGER, -nn);
  
  // Preserve RDF metadata as attributes
  filtered_rwtbl.attr("mrm_config_name") = meta.at(0).at(1);
  filtered_rwtbl.attr("owner") = meta.at(1).at(1);
  filtered_rwtbl.attr("description") = meta.at(2).at(1);
  filtered_rwtbl.attr("create_date") = meta.at(3).at(1);
  filtered_rwtbl.attr("n_traces") = std::stoi(meta.at(4).at(1));
  
  return filtered_rwtbl;
}
