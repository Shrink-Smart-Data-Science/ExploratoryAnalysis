library(plyr)
library(readr)
library(tidyverse)

#Pull in data from Data-Sources
load("~/Documents/Data-Sources/Data/distance_Data.Rdata")
center_cities <- read_csv("/Users/denisebradford/Documents/Data-Sources/Data/place_centroids.csv")

#Combine data from the distance data
ia_dist_data <- center_cities %>% 
  dplyr::rename(City = name) %>%
  #Include the hospital distance information
  left_join(ia_hospitals_dist %>% select(-dist), by ="City") %>% 
  dplyr::rename(hosp.dist.mi = dist.mi) %>%
  #Include the fire department distance information
  left_join(ia_firedept_dist %>% select(-dist), by = c("City" = "center.city")) %>%
  dplyr::rename(fire.dist.mi = dist.mi) %>%
  #Include the school distance information
  left_join(ia_city_school_dist %>% group_by(City) %>%
              summarise(dist.public.Elementary = mean(dist_public_Elementary, na.rm = TRUE),
                        dist.public.Middle = mean(dist_public_Middle, na.rm = TRUE),
                        dist.public.High = mean(dist_public_High, na.rm = TRUE)) , by ="City") %>%
  #dplyr::rename(school.dist.mi = dist.mi) %>%
  #Include the post office distance information
  left_join(ia_postoffice_dist %>% select(-dist), by ="City") %>%
  dplyr::rename(postoff.dist.mi = dist.mi)

# ia_city_school_dist %>% 
#   group_by(City) %>%
#   summarise(dist.public.Elementary = mean(dist_public_Elementary, na.rm = TRUE),
#             dist.public.Middle = mean(dist_public_Middle, na.rm = TRUE),
#             dist.public.High = mean(dist_public_High, na.rm = TRUE)) %>% 
#   head() 

#, dist_public_Middle, dist_public_High) %>% unique() %>% head()


ia.dist.data.nested <- 
  ia_dist_data %>%
  group_by(City, hosp.dist.mi, fire.dist.mi, dist.public.Elementary, dist.public.Middle, dist.public.High, postoff.dist.mi) %>%
  nest()

#Combine data from the "clean datasets" in the data folder
EDA_combine_data <- 
  read_csv("IowaGov/assisted_living.csv") %>% 
  select(program_name,dementia_specific,certification,occupancy,county,zip5,city = location_1_city) %>%
  full_join(read_csv("IowaGov/ems.csv") %>% 
              select(county = County,zip5,ems_level = level,ems_specialty=specialty,ems_license = license,ems_owner = Owner)) %>%
  full_join(read_csv("IowaGov/fire_dept.csv") %>% 
              select(county = County,fire_org_type = organization_type,fire_dept_type = dept_type,active_firefighters_career,active_firefighters_volunteer,
                     active_firefighters_paid_per_call,non_firefighting_civilian,non_firefighting_volunteer, primary_agency_for_em, 
                     zip5, Firefighters)) %>%
  full_join(read_csv("IowaGov/hospitals.csv") %>% 
              select(city = City,county = County,hospital_status = Status,hospital_type = Type,hospital_owner = Owner,Helipad,zip5,hospital_beds,hospital_trauma_level)) %>%
  #bind_rows(read_csv("IowaGov/post_offices.csv") %>% 
   #           select()) %>%
  full_join(read_csv("IowaGov/retirement_homes.csv") %>% 
              select(city = mailing_city,retire_loc = physical_location,zip5)) %>%
  full_join(read_csv("IowaGov/schools.csv") %>% 
              select(county,grade_start,grade_end,city = mailing_city,school_public = public,school_level = type,zip5)) %>%
  full_join(read_csv("child_care_data.csv", skip = 1) %>% 
              select(county = COUNTY,child_care_type = `PROVIDER TYPE`,active_cca = `IS ACTIVE CCAProvider`,
                     zip5 = `PROVIDER ZIP CODE`,qrs_rate = `PROVIDER QRS RATING`))  %>%
  distinct()



#Combining the distance from fire, schools, post offices and hospitals
EDA_dist_combine <- ia_dist_data %>% 
  select(city = City, county = County.x, hosp_dist,fire_dist,school_dist,postoff_dist) %>%
  full_join(EDA_combine_data) %>%
  distinct()

#Pulling the top 20 cities in Iowa
EDA_dist_combine_small <- EDA_dist_combine %>%
  filter(!(city %in% c("Des Moines","Cedar Rapids","Davenport","Sioux City","Iowa City","West Des Moines",
                     "Ankeny","Waterloo","Ames","Council Bluffs","Dubuque","Urbandale","Cedar Falls",
                     "Marion","Bettendorf","Mason City","Marshalltown","Clinton","Burlington","Ottumwa")))
  

