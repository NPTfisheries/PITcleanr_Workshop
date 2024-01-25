# -----------------------
# Author(s): Ryan N. Kinzer, Mike Ackerman, and Kevin See
# Purpose: Script for PITcleanr hands-on workshop
# 
# Created Date: January 25, 2024
# 
#
# Notes: This script was created to go along with the R markdown document
# used for the PITcleanr hands-on workshop. Workshop initially given at the
# 2024 PIT Tag Workshop at Skamania Lodge, WA

# install PITcleanr, if necessary
# install.packages("remotes")
# remotes::install_github("KevinSee/PITcleanr",
#                         build_vignettes = TRUE)

# load necessary libraries
library(kableExtra)
library(PITcleanr)
library(tidyverse)
library(sf)
library(kableExtra)

# interrogation site metadata
int_meta_KRS = queryInterrogationMeta(site_code = "KRS") # South Fork Salmon River, Krassel
int_meta = queryInterrogationMeta(site_code = NULL)

# count active instream remote sites by organization
int_meta %>%
  filter(siteType == "Instream Remote Detection System",
         active) %>%
  count(operationsOrganizationCode) %>%
  ggplot(aes(x = operationsOrganizationCode, y = n)) +
    geom_col() + 
    coord_flip()

# plot location of interrogation sites
# install.packages('maps')
# get state boundaries
pnw = st_as_sf(maps::map("state", 
                   region = c('ID', 'WA', 'OR'), 
                   plot = FALSE, 
                   fill = TRUE))

# create spatial feature object of IPTDS
int_sf <- int_meta %>% 
  filter(siteType == "Instream Remote Detection System",
         !is.na(longitude),
         !is.na(latitude)) %>%
  st_as_sf(coords = c('longitude', 'latitude'), 
           crs = 4326) # WGS84

# map of sites
ggplot() + 
  geom_sf(data = pnw) + 
  geom_sf(data = int_sf, aes(color = active))

# configuration of an interrogation site
int_config <- queryInterrogationConfig(site_code = 'ZEN') # Secesh River, Zena Creek Ranch

int_config %>%
  kable() %>%
  kable_styling(full_width = TRUE,
                bootstrap_options = 'striped')

# all MRR sites
mrr_meta <- queryMRRMeta(site = NULL)

head(mrr_meta, 3) %>%
  kable() %>%
  kable_styling(full_width = TRUE,
                bootstrap_options = "striped")

# wrapper to download all site meta, but NOT configuration
# ptagis_meta <- queryPtagisMeta()

# wrapper to download site metadata and INT configuration data at once, and apply some formatting
config <- buildConfig(node_assign = "array",
                      array_suffix = "UD")

# number of unique sites in PTAGIS
n_distinct(config$site_code)

# number of unique nodes in PTAGIS
n_distinct(config$node)

# the number of detection locations/antennas in PTAGIS
nrow(config)

head(config, 9) %>%
  # remove site_description column for formatting
  select(-site_description) %>% 
  kable() %>%
  kable_styling(full_width = TRUE,
                bootstrap_options = "striped")

# example to buildConfig using sites instead
# tmp_config <- buildConfig(node_assign = "site")

# check for timer tag to evaluate site operation
api_key = '1AA5CF55-C98E-4001-96E4-1E96CEE1E806'
test_tag = queryTestTagSite(site_code = "ZEN", 
                            year = 2023, 
                            api_key = api_key) # requires an API key

# plot ZEN operations
test_tag %>%
  ggplot(aes(x = time_stamp, y = 1)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = -45, vjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  facet_grid(antenna_id~transceiver_id)

# generate file path to example tag list in PITcleanr
tag_list = system.file("extdata", 
                       "TUM_chnk_tags_2018.txt", 
                       package = "PITcleanr",
                       mustWork = TRUE)

read_delim(tag_list, delim='\t') %>%
  head(5) %>%
  kable(col.names = NULL) %>%
  kable_styling(full_width = TRUE,
                bootstrap_options = "striped")

# or simple example to your own file on desktop
#tag_list = "C:/Users/username/Desktop/my_tag_list.txt"

# file path to the example CTH in PITcleanr
ptagis_file <- system.file("extdata", 
                          "TUM_chnk_cth_2018.csv",
                          package = "PITcleanr",
                          mustWork = TRUE)

# read complete tag history
raw_ptagis = readCTH(ptagis_file,
                     file_type = "PTAGIS") %>%
  # filter for only detections after start of run year
  filter(event_date_time_value >= lubridate::ymd(20180301))

# number of detections
nrow(raw_ptagis)

# number of unique tags
dplyr::n_distinct(raw_ptagis$tag_code)

# compare reading object versus file
head(raw_ptagis, 4)
head(read_csv(ptagis_file), 4)

# qc data
# using the complete tag history file path
qc_detections = qcTagHistory(ptagis_file)

# or the complete tag history tibble from readCTH()
qc_detections = qcTagHistory(raw_ptagis)

# view release batch information
qc_detections$rel_time_batches %>%
  kable() %>%
  kable_styling(full_width = TRUE,
                bootstrap_options = "striped")

### complete tag history
# query PTAGIS complete tag history for a single tag code; some examples
tagID <- "3D6.1D594D4AFA" # IR3, GOJ, BO1 -> IR5
tagID <- "3DD.003D494091" # GRA, IR1 -> IML

# query capture history
tag_cth = queryCapHist(ptagis_tag_code = tagID)

# example to summarise CTH
tag_cth %>%
  group_by(event_type_name, 
           event_site_code_value) %>%
  summarise(n_dets = n(),
            min_det = min(event_date_time_value),
            max_det = max(event_date_time_value)) %>%
  mutate(duration = difftime(max_det, min_det, units = "hours")) %>%
  kable() %>%
  kable_styling(full_width = TRUE,
            bootstrap_options = 'striped') 

# MRR tag file summaries
#mrr_file <- "NBD15065.TUM"
mrr_file <- "JSW-2022-175-001.xml"

mrr_data <- queryMRRDataFile(mrr_file)

# summary of injuries
mrr_data %>%
  group_by(species_run_rear_type, 
           text_comments) %>%
  filter(!is.na(text_comments)) %>% # remove NA comments
  count() %>%
  ggplot(aes(x = text_comments, 
             y = n, 
             fill = species_run_rear_type)) +
  geom_col() +
  coord_flip() +
  labs(title = paste0(unique(mrr_data$release_site), ' : ', mrr_file))

# multiple MRR files
julian = str_pad(1:10, 3, pad = 0) # julian 001 - 010
yr = 2024                          # tagging year

# Imnaha smolt trap, first 10 days of 2024
mrr_files = paste0("IMN-", yr, "-", julian, "-NT1.xml")

# iterate over files
mrr_data <- map_df(mrr_files,  
             .f = queryMRRDataFile)

# view mrr data
head(mrr_data, 5)

# summarise new marks by day
mrr_data %>% 
  filter(event_type == 'Mark') %>%
  mutate(release_date = date(release_date)) %>%
  group_by(release_date, 
           species_run_rear_type) %>%
  count() %>%
  kable() %>%
  kable_styling(full_width = TRUE,
                bootstrap_options = "striped")

# compress observations
comp_obs = compress(cth_file = raw_ptagis,
                    configuration = config)

# view compressed observations
head(comp_obs)

# compare number of detections
nrow(raw_ptagis)
nrow(comp_obs)

# distinct tags
n_distinct(comp_obs$tag_code)

# number of unique tags per node
comp_obs %>%
  group_by(node) %>%
  summarise(n = n_distinct(tag_code)) %>%
  ggplot(aes(x = fct_reorder(node,n), y = n)) +
  geom_col() +
  coord_flip()

# extract sites from complete tag histories
sites_sf = extractSites(cth_file = raw_ptagis,
                        as_sf = T,
                        min_date = '20180301',
                        configuration = config)

# remove some sites not of interest
sites_sf = sites_sf %>%
  # all sites in Wenatchee have an rkm greater than or equal to 754
  filter(str_detect(rkm, '^754'),
         type != "MRR",            # ignore MRR sites
         site_code != 'LWE') %>%   # ignore a site downstream of Tumwater
  mutate(across(site_code,
                ~ recode(.,
                         "TUF" = "TUM"))) # combine TUM and TUF

# download subset of NHDPlus flowlines
nhd_list = queryFlowlines(sites_sf = sites_sf,
                          root_site_code = "TUM",
                          min_strm_order = 2,
                          dwnstrm_sites = T,
                          dwn_min_stream_order_diff = 2)

# join upstream and downstream flowlines in nhd_list
flowlines = nhd_list$flowlines %>%
    rbind(nhd_list$dwn_flowlines)

# load ggplot
library(ggplot2)

tum_map <- ggplot() +
  geom_sf(data = flowlines,
          aes(color = as.factor(StreamOrde),
              size = StreamOrde)) +
  scale_color_viridis_d(direction = -1,
                        option = "D",
                        name = "Stream\nOrder",
                        end = 0.8) +
  scale_size_continuous(range = c(0.2, 1.2),
                        guide = 'none') +
  geom_sf(data = nhd_list$basin,
          fill = NA,
          lwd = 2) +
  theme_bw() +
  theme(axis.title = element_blank())

# tumwater map
tum_map +
  geom_sf(data = sites_sf,
          size = 4,
          color = "black") +
  ggrepel::geom_label_repel(
    data = sites_sf,
    aes(label = site_code, 
        geometry = geometry),
    size = 2,
    stat = "sf_coordinates",
    min.segment.length = 0,
    max.overlaps = 50
  )

# build parent-child table
parent_child = buildParentChild(sites_sf,
                                flowlines)

# plot parent-child table
plotNodes(parent_child)

# edit parent-child table
parent_child = editParentChild(parent_child,
                               fix_list = list(c(NA, "PES", "TUM"),
                                               c(NA, "LNF", "ICL"),
                                               c("PES", "ICL", "TUM")),
                               switch_parent_child = list(c("ICL", 'TUM'))) %>%
  filter(!is.na(parent))

# plot new parent-child table
plotNodes(parent_child)

# add nodes for arrays
parent_child_nodes = addParentChildNodes(parent_child = parent_child,
                                        configuration = config)

# plot parent-child w/ nodes
plotNodes(parent_child_nodes)

# build paths and add node order
node_order = buildNodeOrder(parent_child = parent_child_nodes,
                            direction = "u")

# view node paths and orders
node_order %>%
  arrange(node) %>%
  kable() %>%
  kable_styling(full_width = TRUE,
                bootstrap_options = "striped")

# add direction based on parent-child table
comp_dir <- addDirection(compress_obs = comp_obs, 
                         parent_child = parent_child_nodes, 
                         direction = "u")

head(comp_dir, 6) %>%
  kable() %>%
  kable_styling(full_width = TRUE,
                bootstrap_options = "striped")

# add direction, and filter detections
comp_filter = filterDetections(compress_obs = comp_obs, 
                               parent_child = parent_child_nodes,
                               # remove any detections after spawning season
                               max_obs_date = "20180930")

# view filtered observations
comp_filter %>%
  select(tag_code:min_det, 
         direction,
         ends_with("keep_obs")) %>%
  # view a strange movement path
  filter(is.na(user_keep_obs)) %>%
  filter(tag_code == tag_code[1]) %>%
  kable() %>%
  kable_styling(full_width = T,
                bootstrap_options = "striped")

# use PITcleanr recommendations
comp_final = comp_filter %>%
  filter(auto_keep_obs = TRUE)

# summarise detections
# number of unique tags per node
node_tags = comp_final %>%
  group_by(node) %>%
  summarise(n_tags = n_distinct(tag_code))

# view tags per node
node_tags

# estimate detection efficiencies for nodes
node_eff = estNodeEff(capHist_proc = comp_final,
                      node_order = node_order)

# view node efficiencies
node_eff %>%
  kable() %>%
  kable_styling(full_width = T,
                bootstrap_options = "striped")

# build capture histories
cap_hist = buildCapHist(filter_ch = comp_filter,
                        parent_child = parent_child,
                        configuration = config,
                        drop_nodes = T)

# view capture histories
cap_hist

# capture history column names
col_nodes <- defineCapHistCols(parent_child = parent_child,
                               configuration = config,
                               use_rkm = T)
col_nodes


### LEMHI SURVIVAL EXAMPLE
# clear environment, if desired
rm(list = ls())

# read in PTAGIS detections
ptagis_file <- system.file("extdata",
                           "LEMTRP_chnk_cth_2021.csv",
                           package = "PITcleanr",
                           mustWork = TRUE)

ptagis_cth <- readCTH(ptagis_file) %>%
  arrange(tag_code,
          event_date_time_value)

# qcTagHistory(ptagis_cth,
#              ignore_event_vs_release = T)

# lemhi configuration
configuration <-
  buildConfig(node_assign = "site") %>% 
  mutate(across(node,
                ~ if_else(as.numeric(str_sub(rkm, 1, 3)) <= 234,
                          "B2J",
                          .)),
         across(node,
                ~ if_else(site_code == "GRS",
                          "GRJ",
                          .))) %>%
  filter(!is.na(node))

# or load from PITcleanr
configuration <- system.file("extdata/LEMTRP",
                             "LEMTRP_configuration.csv",
                             package = "PITcleanr",
                             mustWork = TRUE) %>%
 readr::read_csv(show_col_types = F)



# compress lemhi detections
comp_obs <-
  compress(ptagis_cth,
           configuration = configuration,
           units = "days") %>%
  # drop a couple of duplicate mark records
  filter(event_type_name != "Mark Duplicate")


# lemhi obs table
comp_obs %>% 
  mutate(across(where(is.difftime),
                as.numeric),
         across(where(is.numeric),
                ~ round(., digits = 3))) %>% 
  DT::datatable(filter = "top")

# lemhi sites
sites_sf <-
  extractSites(ptagis_cth,
               as_sf = T,
               configuration = configuration,
               max_date = "20220630") %>%
  arrange(desc(rkm))

sites_sf

# filter lemhi sites
sites_sf <-
  sites_sf %>% 
  left_join(configuration %>% 
              select(site_code, 
                     rkm_total) %>% 
              distinct()) %>% 
  filter(nchar(rkm) <= 7 |
           (str_detect(rkm, "522.303.416") &
              rkm_total <= rkm_total[site_code == "LEMTRP"] &
              nchar(rkm) == 15),
         !site_code %in% c("HAYDNC",
                           "S3A"))

# load flowlines from PITcleanr
flowlines <- system.file("extdata/LEMTRP",
                         "LEMTRP_flowlines.gpkg",
                         package = "PITcleanr",
                         mustWork = TRUE) %>%
  st_read(quiet = T) %>%
  rename(geometry = geom) %>%
  select(-id)


# map of lemhi
ggplot() +
  geom_sf(data = flowlines,
          aes(color = as.factor(StreamOrde))) +
  scale_color_viridis_d(direction = -1,
                        option = "D",
                        name = "Stream\nOrder",
                        end = 0.8) +
  geom_sf(data = sites_sf,
          size = 3,
          color = "black") +
  ggrepel::geom_label_repel(
    data = sites_sf,
    aes(label = site_code,
        geometry = geometry),
    size = 1.5,
    stat = "sf_coordinates",
    min.segment.length = 0,
    max.overlaps = 100
  ) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.position = "bottom")

# construct parent-child table
parent_child = sites_sf %>%
  buildParentChild(flowlines,
                   rm_na_parent = T,
                   add_rkm = F) %>% 
  select(parent,
         child)

# flip direction of parent/child relationships
parent_child <-
  parent_child %>%
  select(p = parent,
         c = child) %>%
  mutate(parent = c,
         child = p) %>%
  select(parent,
         child)

plotNodes(parent_child)

prepped_df <- prepWrapper(compress_obs = comp_obs,
                          parent_child = parent_child,
                          start_node = "LEMTRP",
                          add_tag_detects = T,
                          save_file = F)

prepped_df <- prepped_df %>%
  mutate(
    across(user_keep_obs,
           ~ if_else(is.na(.),
                     auto_keep_obs,
                     .))) %>% 
  filter(user_keep_obs)

# translate PIT tag observations into capture histories, one per tag
cap_hist <- buildCapHist(prepped_df,
                         parent_child = parent_child,
                         configuration = configuration)

# show an example
cap_hist

# to find out the node associated with each column
col_nodes <- defineCapHistCols(parent_child = parent_child,
                               configuration = configuration)
col_nodes

cap_hist2 <- buildCapHist(prepped_df,
                          parent_child = parent_child,
                          configuration = configuration,
                          drop_nodes = F)
cap_hist2

cap_hist2 %>% select(-c(tag_code,
                        cap_hist)) %>% 
  colSums()

prepped_df %>% 
  group_by(node) %>% 
  summarize(n_tags = n_distinct(tag_code),
            .groups = "drop") %>% 
  mutate(across(node,
                ~ factor(.,
                         levels = col_nodes))) %>% 
  arrange(node)


### fit CJS model
# load needed package
library(marked)

# process capture history into necessary format
cjs_proc <-
  cap_hist %>%
  select(tag_code,
         ch = cap_hist) %>% 
  as.data.frame() %>%
  process.data(model = "CJS")

# create design data
cjs_ddl <-
  make.design.data(cjs_proc)

# set model construction
Phi.time <- list(formula = ~ time)
p.time <- list(formula = ~ time)

# fit model
mod1 <- crm(data = cjs_proc,
            ddl = cjs_ddl,
            model.parameters = list(Phi = Phi.time,
                                    p = p.time),
            hessian = T)

# pull out cjs parameter estimates
est_preds <- predict(mod1) %>%
  map(.f = as_tibble)

est_preds$Phi <-
  est_preds$Phi %>%
  left_join(parent_child %>%
              left_join(buildNodeOrder(parent_child),
                        by = join_by(child == node)) %>% 
              arrange(node_order) %>%
              mutate(occ = node_order - 1) %>% 
              select(occ, parent, child) %>%
              unite(col = "reach",
                    parent,
                    child,
                    sep = "_"),
            by = join_by(occ)) %>% 
  relocate(reach,
           .after = occ)

est_preds$p <-
  est_preds$p %>%
  mutate(site = rev(parent_child$child)) %>% 
  relocate(site,
           .after = occ)

# plot survival
est_preds$Phi %>%
  ggplot(aes(x = fct_reorder(reach, est_preds$Phi$occ), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lcl, ymax = ucl))

# examine survival estimates
est_preds$Phi %>% 
  mutate(across(where(is.numeric),
         ~ round(., digits = 3))) %>%
  kable() %>%
  kable_styling(full_width = TRUE,
                bootstrap_options = "striped")

# plot detection probabilities
est_preds$p %>%
  ggplot(aes(x = fct_reorder(site, est_preds$p$occ), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lcl, ymax = ucl))

# Detection
est_preds$p %>% 
  mutate(across(where(is.numeric),
         ~ round(., digits = 3))) %>%
  kable() %>%
  kable_styling(full_width = TRUE,
                bootstrap_options = "striped")

# cumulative survival to Lower Granite, McNary and John Day dams
est_preds$Phi %>% 
  # group_by(life_stage) %>% 
  summarize(lower_granite = prod(estimate[occ <= 6]),
            mcnary = prod(estimate[occ <= 10]),
            john_day = prod(estimate[occ <= 11]))
