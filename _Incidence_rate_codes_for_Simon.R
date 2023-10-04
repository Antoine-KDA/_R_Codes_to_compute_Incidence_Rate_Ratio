


# The following package if not installed on your computer: 
# 
install.packages("dplyr")
install.packages("biostat3")

library(dplyr)
library(biostat3)
library(survival)


# Import the dataset


ExampleSimon_key <- rio::import("./Output files/ExampleSimon_key.xlsx")


# 1- Create a function to calculate the incidence rate accross different variables------------

IncRate <- function(data, start, stop, status, varlist, dig=2, pop=100){
  univ_formulas <- sapply(varlist,
                          function(x) as.formula(paste(paste0('Surv(',start,",",stop, ",",status,')~'), x)))
  univ_models <- lapply(univ_formulas, function(x){biostat3::survRate(x, data=data)})
  # Extract data 
  Export <- NULL
  for(i in names(univ_models)){ 
    t <- as.data.frame(univ_models[[i]])
    t$Est_Confint <- paste0(sprintf(paste0("%.", dig, "f"), t$rate*pop), " (", 
                            sprintf(paste0("%.", dig, "f"), t$lower*pop), "; ",
                            sprintf(paste0("%.", dig, "f"), t$upper*pop), ")")
    
    t[,1] <- as.character(t[,1])
    t <- rbind(c(names(t)[1], rep("", ncol(t)-1)), t)
    names(t)[1] <- "Variable"
    
    Export <- as.data.frame(rbind(Export, t))
    Export
    
  }
  names(Export)[length(names(Export))] <- paste0("Rate (95% CI) per ", paste0(pop),
                                                 " persons-years")
  Export
  
  
}

# 2- Apply the function to the dataset - -------------


# List of variables for which to compute the incidence rate ratios 

var = c("age_entry_cat", "sex", "year_entry_cat", 
        "cci_unw_cat", "educ_max", "partner")  # Put here all the variables that you want to compute the IR for.


Patients = IncRate(data=ExampleSimon_key, # This is the dataset to be used for the computation
                   start="tstart", # This is the date of start of FU, usually 0 if non-recurrent event, 
                   # in my case it is recurrent (so I have a long dataset format), so only the first
                   # observation is 0
                   stop="tstop", # This is the end of FU (equals to the total person-year for the individual
                   # if it is non-recureent event)
                   # 
                   # NOTE: tstart and tstop are variables in years. So the result is in person-years already
                   # # Convert the original variables in the correct scales before feeding the model
                   # Exempl: to get the tstop as follow: tstop = (end_of_FU_date - start_of_FU_date)/365.24
                   status="death_MCL", # The event status variable (Yes/no or 1/0, should be logical)
                   varlist=var, # The variables to compute the rate (IR) across, should be a character vector
                   dig=2, # the number of decimal to display, the default is 2 decimal place
                   pop=1)  # the unit of the results, the default is 1 person-year

Patients



# 3- Export the result ------------

Patients |>
  dplyr::rename(Total_person_years = tstop, 
                Total_Number_Of_event = event) -> Patients


# Insure to replace the directory correctly to export the result in the "file=" argument
rio::export(Patients, file="./Output files/example_of_result_for_Simon.xlsx")




