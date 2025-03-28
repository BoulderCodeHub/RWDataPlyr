//============================================================================
// Name        : rwdataplyr.cpp
// Author      : Alan Butler
// Version     :
// Copyright   : CC0
// Description : 
//============================================================================

#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]

// parse RDF metadata
std::vector<std::vector<std::string>> parse_rdf_meta(const std::vector<std::string>& rdf) {
  std::vector<std::vector<std::string>> meta;
  std::vector<std::string> row(2);
  std::string line, token;
  size_t i = 0;
  
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
List rdf_to_rwtbl_cpp(std::vector<std::string> rdf, 
                      std::vector<std::string> keep_cols, 
                      String const scenario = NA_STRING, 
                      bool add_ym = true,
                      bool big = false,
                      size_t row_index = 0,
                      int last_trace = 0) {
  auto meta = parse_rdf_meta(rdf);
  int num_runs = get_n_runs(meta);
  
  size_t i = 0; // index into the rdf lines
  int trace_count = last_trace;
  
  while (rdf[i] != "END_PACKAGE_PREAMBLE") ++i;
  ++i; // Skip END_PACKAGE_PREAMBLE
  if (row_index > 0) {
    i = row_index;
  }
  
  // ***********************************
  // parse trace 1 - this will be used to determine how big 
  // ***********************************
  ++trace_count;
  std::vector<std::string> timesteps;
  std::string slot_set, rule_set;
  size_t num_time_steps = 0;
  int trace_number = -999; // initialize to -999 
  
  // read the run preamble
  while (rdf[i] != "END_RUN_PREAMBLE") {
    std::vector<std::string> row = parse_line(rdf[i++]);
    
    if (row[0] == "time_steps") {
      num_time_steps = std::stoul(row[1]);
    } else if (row[0] == "slot_set") {
      slot_set = row[1];
    } else if (row[0] == "rule_set") {
      rule_set = row[1];
    } else if (row[0] == "trace_num" || row[0] == "trace") {
      trace_number = std::stoi(row[1]);
    } 
  }
  ++i; // Skip END_RUN_PREAMBLE
  // if still -999, then not set by run preamble meta data, so need to set it
  if (trace_number == -999) {
    trace_number = trace_count;
  }
  
  // now start reading the timesteps
  timesteps.reserve(num_time_steps);
  
  for (size_t j = 0; j < num_time_steps; ++j) {
    timesteps.push_back(rdf[i]);
    ++i;
  }
  
  std::vector<std::string> month;
  std::vector<int> year;
  if (add_ym) {
    
    year.reserve(num_time_steps);
    month.reserve(num_time_steps);
    
    for (size_t j = 0; j < num_time_steps; ++j) {
      auto ym = get_year_month(timesteps[j]);
      year.push_back(std::stoi(ym[0]));
      month.push_back(ym[1]);
    }
  }
  // have all the time steps
  
  // start parsing the slots and slot preambles
  std::vector<std::string> row, obj_vec, slot_vec, obj_slot_vec, obj_type_vec, units_vec;
  std::vector<double> vals, scale_vec;
  size_t n_slots = 0;
  
  while (rdf[i] != "END_RUN") {
    std::string obj, slot, obj_slot, object_type, units;
    double scale;
    ++n_slots;
    
    while (rdf[i] != "END_SLOT_PREAMBLE") {
      row = parse_line(rdf[i++]);
      if (row[0] == "object_name") obj = row[1];
      else if (row[0] == "slot_name") slot = row[1];
      else if (row[0] == "object_type") object_type = row[1];
    }
    ++i; // Skip END_SLOT_PREAMBLE
    
    size_t val_counter = 0;
    while (rdf[i] != "END_SLOT") {
      row = parse_line(rdf[i]);
      
      if (row[0] == "units") units = row[1];
      else if (row[0] == "scale") scale = std::stod(row[1]);
      else if (rdf[i] != "END_COLUMN") {
        vals.push_back(std::stod(rdf[i]));
        val_counter++;
      }
      ++i;
    }
    
    if (val_counter != num_time_steps) {
      // must be a scalar slot; so need to copy the value for all timesteps
      for (size_t j = 1; j < num_time_steps; j++) {
        vals.push_back(vals.back());
      }
    }
    // done with one slot
    
    obj_slot = obj + "." + slot;
    
    // fill/copy the attributes for this slot
    for (size_t k = 0; k < num_time_steps; ++k) {
      obj_vec.push_back(obj);
      slot_vec.push_back(slot);
      obj_slot_vec.push_back(obj_slot);
      obj_type_vec.push_back(object_type);
      units_vec.push_back(units);
      scale_vec.push_back(scale);
    }
    ++i; // Skip END_SLOT
  }
  ++i; // Skip END_RUN
  
  
  // Done reading one full trace ********************************
  // now a bunch of computations to pre-allocate vectors for all the different
  // attributes and their values
  size_t n_per_trace = vals.size();
  size_t total_rows;
  if (big) {
    total_rows = n_per_trace;
  } else {
    total_rows = n_per_trace * num_runs;
  }
  
  
  if (n_per_trace != n_slots * num_time_steps)
    throw std::runtime_error("Something happened and the total values != to num slots * num time steps for trace 1");
  
  //  need to take timesteps and duplicate for every slot
  std::vector<std::string> timesteps_vec;
  timesteps_vec.reserve(total_rows);
  for (size_t j = 0; j < n_slots; ++j) {
    timesteps_vec.insert(timesteps_vec.end(), timesteps.begin(), timesteps.end());
  } 
  
  // and now duplicate year and month for every slot too (if needed)
  std::vector<std::string> month_vec;
  std::vector<int> year_vec;
  if (add_ym) {
    year_vec.reserve(total_rows);
    month_vec.reserve(total_rows);
    
    for (size_t j = 0; j < n_slots; ++j) {
      year_vec.insert(year_vec.end(), year.begin(), year.end());
      month_vec.insert(month_vec.end(), month.begin(), month.end());
    }
  }
  
  // these three slots have constant value for full trace so need vector versions
  std::vector<std::string> slot_set_vec(n_per_trace, slot_set);
  std::vector<std::string> rule_set_vec(n_per_trace, rule_set);
  std::vector<int> trace_vec(n_per_trace, trace_number);
  
  if (!big) {
    // allocate all of the vectors for remaining traces
    // these already have data for one full trace
    obj_vec.reserve(total_rows - n_per_trace);
    slot_vec.reserve(total_rows - n_per_trace);
    obj_slot_vec.reserve(total_rows - n_per_trace);
    obj_type_vec.reserve(total_rows - n_per_trace);
    units_vec.reserve(total_rows - n_per_trace);
    scale_vec.reserve(total_rows - n_per_trace);
    vals.reserve(total_rows - n_per_trace);
    
    // these three slots have constant value for full trace so need vector versions
    slot_set_vec.reserve(total_rows - n_per_trace);
    rule_set_vec.reserve(total_rows - n_per_trace);
    trace_vec.reserve(total_rows - n_per_trace);
    
    // **********************************
    // now loop through all remaining traces, process data, and add them to the 
    // correct vectors
    
    while (trace_count < num_runs) {
      ++trace_count;
      trace_number = -999; // initialize to -999 
      
      // read the run preamble
      while (rdf[i] != "END_RUN_PREAMBLE") {
        row = parse_line(rdf[i++]);
        
        if (row[0] == "time_steps") {
          num_time_steps = std::stoi(row[1]);
        } else if (row[0] == "slot_set") {
          slot_set = row[1];
        } else if (row[0] == "rule_set") {
          rule_set = row[1];
        } else if (row[0] == "trace_num" || row[0] == "trace") {
          trace_number = std::stoi(row[1]);
        } 
      }
      ++i; // Skip END_RUN_PREAMBLE
      
      // if still -999, then not set by run preamble meta data, so need to set it
      if (trace_number == -999) {
        trace_number = trace_count;
      }
      
      // now start reading the timesteps
      timesteps.clear();
      
      for (size_t j = 0; j < num_time_steps; ++j) {
        timesteps.push_back(rdf[i]);
        ++i;
      }
      
      if (add_ym) {
        year.clear();
        month.clear();
        
        for (size_t j = 0; j < num_time_steps; ++j) {
          auto ym = get_year_month(timesteps[j]);
          year.push_back(std::stoi(ym[0]));
          month.push_back(ym[1]);
        }
      }
      // have all the time steps
      
      // start parsing the slots and slot preambles
      
      while (rdf[i] != "END_RUN") {
        std::string obj, slot, obj_slot, object_type, units;
        double scale;
        
        while (rdf[i] != "END_SLOT_PREAMBLE") {
          row = parse_line(rdf[i++]);
          if (row[0] == "object_name") obj = row[1];
          else if (row[0] == "slot_name") slot = row[1];
          else if (row[0] == "object_type") object_type = row[1];
        }
        ++i; // Skip END_SLOT_PREAMBLE
        size_t val_counter = 0;
        while (rdf[i] != "END_SLOT") {
          row = parse_line(rdf[i]);
          if (row[0] == "units") units = row[1];
          else if (row[0] == "scale") scale = std::stod(row[1]);
          else if (rdf[i] != "END_COLUMN") {
            vals.push_back(std::stod(rdf[i]));
            val_counter++;
          }
          ++i;
        }
        
        if (val_counter != num_time_steps) {
          // must be a scalar slot; so need to copy the value for all timesteps
          for (size_t j = 1; j < num_time_steps; j++) {
            vals.push_back(vals.back());
          }
        }
        // done with one slot
        
        obj_slot = obj + "." + slot;
        
        // fill/copy the attributes for this slot
        for (size_t k = 0; k < num_time_steps; ++k) {
          obj_vec.push_back(obj);
          slot_vec.push_back(slot);
          obj_slot_vec.push_back(obj_slot);
          obj_type_vec.push_back(object_type);
          units_vec.push_back(units);
          scale_vec.push_back(scale);
        }
        ++i; // Skip END_SLOT
      }
      ++i; // Skip END_RUN
      
      // copy over time steps, year, month, slot_set, rule_set, trace_number
      for (size_t j = 0; j < n_slots; ++j) {
        timesteps_vec.insert(timesteps_vec.end(), timesteps.begin(), timesteps.end());
      } 
      
      // and now duplicate year and month for every slot too (if needed)
      if (add_ym) {
        for (size_t j = 0; j < n_slots; ++j) {
          year_vec.insert(year_vec.end(), year.begin(), year.end());
          month_vec.insert(month_vec.end(), month.begin(), month.end());
        }
      }
      
      std::fill_n(std::back_inserter(slot_set_vec), n_per_trace, slot_set);
      std::fill_n(std::back_inserter(rule_set_vec), n_per_trace, rule_set);
      std::fill_n(std::back_inserter(trace_vec), n_per_trace, trace_number);
      
    }
  }
  
  // now construct and return the table
  
  std::vector<std::string> col_names = {
    "Timestep","Year", "Month", "Value",
    "ObjectName", "SlotName", "ObjectSlot",
    "TraceNumber", "InputDMIName", "RulesetFileName",
    "ObjectType", "Unit", "Scale"
  };
  
  // Conditionally add the 'Scenario' column if provided
  std::vector<std::string> scenario_vec;
  if (scenario != NA_STRING) {
    col_names.push_back("Scenario");
    keep_cols.push_back("Scenario");
    scenario_vec.reserve(total_rows);
    scenario_vec.assign(total_rows, scenario);
  }
  
  // Ensure that 'Year' and 'Month' are included if add_ym is TRUE
  
  if (add_ym) {
    keep_cols.push_back("Year");
    keep_cols.push_back("Month");
  }
  
  // Identify which columns to keep (preserve original order in rwtbl)
  std::vector<std::string> final_col_names;
  
  for (size_t j = 0; j < col_names.size(); ++j) {
    if (std::find(keep_cols.begin(), keep_cols.end(), col_names[j]) != keep_cols.end()) {
      final_col_names.push_back(col_names[j]);
    }
  }
  
  // Create a new data frame with only the selected columns
  List filtered_rwtbl(final_col_names.size());
  size_t j = 0;
  filtered_rwtbl[j++] = timesteps_vec;
  if (add_ym) {
    filtered_rwtbl[j++] = year_vec;
    filtered_rwtbl[j++] = month_vec;
  }
  filtered_rwtbl[j++] = vals;
  if (std::find(final_col_names.begin(), final_col_names.end(), "ObjectName") != final_col_names.end()) {
    filtered_rwtbl[j++] = obj_vec;
  }
  if (std::find(final_col_names.begin(), final_col_names.end(), "SlotName") != final_col_names.end()) {
    filtered_rwtbl[j++] = slot_vec;
  }
  if (std::find(final_col_names.begin(), final_col_names.end(), "ObjectSlot") != final_col_names.end()) {
    filtered_rwtbl[j++] = obj_slot_vec;
  }
  if (std::find(final_col_names.begin(), final_col_names.end(), "TraceNumber") != final_col_names.end()) {
    filtered_rwtbl[j++] = trace_vec;
  }
  if (std::find(final_col_names.begin(), final_col_names.end(), "InputDMIName") != final_col_names.end()) {
    filtered_rwtbl[j++] = slot_set_vec;
  }
  if (std::find(final_col_names.begin(), final_col_names.end(), "RulesetFileName") != final_col_names.end()) {
    filtered_rwtbl[j++] = rule_set_vec;
  }
  if (std::find(final_col_names.begin(), final_col_names.end(), "ObjectType") != final_col_names.end()) {
    filtered_rwtbl[j++] = obj_type_vec;
  }
  if (std::find(final_col_names.begin(), final_col_names.end(), "Unit") != final_col_names.end()) {
    filtered_rwtbl[j++] = units_vec;
  }
  if (std::find(final_col_names.begin(), final_col_names.end(), "Scale") != final_col_names.end()) {
    filtered_rwtbl[j++] = scale_vec;
  }
  if (std::find(final_col_names.begin(), final_col_names.end(), "Scenario") != final_col_names.end()) {
    filtered_rwtbl[j++] = scenario_vec;
  }
  
  // Apply attributes to the data frame
  filtered_rwtbl.attr("names") = wrap(final_col_names);
  //filtered_rwtbl.attr("stringsAsFactors") = false;
  filtered_rwtbl.attr("class") = "data.frame";
  IntegerVector row_names(total_rows);
  std::iota(row_names.begin(), row_names.end(), 1);
  filtered_rwtbl.attr("row.names") = row_names;
  
  // Preserve RDF metadata as attributes
  filtered_rwtbl.attr("mrm_config_name") = meta.at(0).at(1);
  filtered_rwtbl.attr("owner") = meta.at(1).at(1);
  filtered_rwtbl.attr("description") = meta.at(2).at(1);
  filtered_rwtbl.attr("create_date") = meta.at(3).at(1);
  filtered_rwtbl.attr("n_traces") = std::stoi(meta.at(4).at(1));
  filtered_rwtbl.attr("last_i") = (i == rdf.size()) ? static_cast<int>(-999) : static_cast<int>(i + 1);
  
  return filtered_rwtbl;
}
