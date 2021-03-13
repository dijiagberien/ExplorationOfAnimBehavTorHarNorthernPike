# Title: Utilization of unsupervised machine learning for the exploratory
#        analysis of acoustic telemetry data
# Author: Adogbeji Agberien
# Date: March 13, 2021


# Clear working directory
rm(list = ls())

# Import and load packages
required_packages <- c("RColorBrewer", "cowplot", "kableExtra", "extrafont",
                       "factoextra", "FactoMineR", "lubridate", "FSA",
                       "tidyverse", "data.table",  "remotes",
                       "ggdendro", "dendextend",
                       "glatos", "rmarkdown", "knitr")

packageCheck <- lapply(required_packages, FUN = function(x) {
  if(!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts()

# Set working directory
setwd("C:/Users/diji_/Desktop/GradSchoolFiles/Thesis/Toronto Harbour/Analysis")

# Import the data
pike.detections.raw <- data.table::fread("TH_Northern_Pike_18Oct2019_01.csv") # Pike detection data
habitat.data.raw <- data.table::fread("habitat.information.csv") # Environmental variables

# Get the names and classes of the columns in habitat.data
str(habitat.data.raw)

# Select the necessary columns and drop empty rows
habitat.data.clean <- habitat.data.raw[, c("receiver.group", "fetch(m)", "mean.depth(m)", "SAV(%)", "pre.strat.temp.low(deg.C)", "strat.temp(deg.C)")] %>%
  na.omit()

# Remove the raw habitat data from environment 
rm(habitat.data.raw)

# Rename the columns
setnames(habitat.data.clean,
         old = c("receiver.group", "fetch(m)", "mean.depth(m)", "SAV(%)", "pre.strat.temp.low(deg.C)", "strat.temp(deg.C)"),
         new = c("Receiver group", "Fetch", "Mean depth", "% SAV", "Pre-stratification temp.", "Stratification temp."),
         skip_absent = T)

# Pre-processing of detection data
# Select required columns and order by datetime
pike.detections.clean <- pike.detections.raw[, c("animal_id", "length", "detection_timestamp_utc", "deploy_long", "deploy_lat",
                               "glatos_array", "station",  "min_lag")] %>%
  .[order(animal_id, detection_timestamp_utc)]

# Remove the raw pike detection data from the environment
rm(pike.detections.raw)

# Convert the datetime to appropriate format
pike.detections.clean[,c("detection_timestamp_utc")
             := lapply(.SD, ymd_hms),
             .SDcols = c("detection_timestamp_utc")]

# Remove false detections
pike.detections.clean <- false_detections(pike.detections.clean, tf = 3600, show_plot = F) %>% .[passed_filter == 1] %>% .[, c("min_lag", "passed_filter") := NULL] %>%
  .[order(animal_id, detection_timestamp_utc)] %>%
  .[, c("detection_timestamp_utc") := lapply(.SD, ymd_hms), .SDcols = c("detection_timestamp_utc")]

# Get all the station names
station <- sort(unique(pike.detections.clean$station))

# The list of receiver groups
receiver.group <- c("Billy Bishop East", "Billy Bishop West", "Cell 2", "Cell 1", "Cell 1", "Cell 1", "Cell 2",
                    "Cell 3", "Cherry Beach", "Cherry Beach 2b", "Don River Mouth", "Don River", "N Eastern Gap",
                    "S Eastern Gap", "EBG-101", "Embayment A", "Embayment B", "Embayment C", "Embayment C", "Embayment C",
                    "Embayment D", "Embayment D", "Embayment D", "Exhibition Gr.", "Peter Slip", "Peter Slip", "Peter Slip",
                    "Peter Slip", "Peter Slip", "Curtain", "Curtain", "Curtain", "Curtain", "Curtain", "Curtain", "Curtain",
                    "Curtain", "GBB-083", "HAM-005", "Jarvis", "Jarvis", "Jarvis", "Jarvis", "Jarvis", "LKO-053", "Mid.Waterfront",
                    "OHM", "Parliament", "Parliament", "Parliament", "Parliament", "Parliament", "Parliament", "Spadina",
                    "Spadina", "Spadina", "Spadina", "Spadina", "Spadina", "TOI-027", "TOI-040", "TOI-041", "TOI-042",
                    "TOI-043", "TOI-044", "TOI-045", "TOI-046", "TOI-047", "Turning Basin", "W Western Gap", "E Western Gap",
                    "WL18-018", "WL19-019", "Lake Ontario", "Lake Ontario", "Lake Ontario")

# Merge station names with receiver groups
rec.group.station.name <- cbind(station, receiver.group)

# Merge the stations and receivers groups with the complete dataset
pike.detections.clean <- merge(rec.group.station.name, pike.detections.clean, by = "station") %>% 
  data.table() 

# Order by animal ID and detection time
pike.detections.clean <- pike.detections.clean[order(animal_id, detection_timestamp_utc)]

# Create new columns for previous and next locations of detection
pike.detections.clean <- pike.detections.clean[, by = animal_id ,":=" (
  glatos_array_lag = data.table::shift(glatos_array, fill = glatos_array[1L], type = "lag"),
  glatos_array_lead = data.table::shift(glatos_array, fill = glatos_array[.N], type = "lead"),
  time_btw_detect = data.table::shift(detection_timestamp_utc,
                                      fill = detection_timestamp_utc[.N], 
                                      type = "lead") -
    detection_timestamp_utc
  )]

# Declare singleton threshold
singleton_threshold <- 6*60*60

# Remove singletons
pike.detections.clean <- pike.detections.clean[!(time_btw_detect < singleton_threshold &
                                   glatos_array != glatos_array_lag &
                                   glatos_array != glatos_array_lead)]

# Recalculate time between detection
pike.detections.clean <- pike.detections.clean[, by = animal_id ,":=" (
  time_btw_detect = data.table::shift(detection_timestamp_utc,
                                      fill = detection_timestamp_utc[.N],
                                      type = "lead") -
    detection_timestamp_utc)]

# Exclude long absences, movements, and resets
pike.detections.clean <- pike.detections.clean[time_btw_detect <= singleton_threshold]

# Remove other unrequired columns
pike.detections.clean <- pike.detections.clean[, c("glatos_array_lag",
                                                   "glatos_array_lead",
                                                   "glatos_array") := NULL]

# State season duration
# Year 2010
spring.2010 <- c(ymd("2010-04-30"), ymd("2010-05-25"))
summer.2010 <- c(ymd("2010-05-25"), ymd("2010-10-03"))
fall.2010 <- c(ymd("2010-10-03"), ymd("2010-11-12"))
winter.2010 <- c(ymd("2010-11-12"), ymd("2011-04-22"))

# Year 2011
spring.2011 <- c(ymd("2011-04-22"), ymd("2011-06-07"))
summer.2011 <- c(ymd("2011-06-07"), ymd("2011-10-16"))
fall.2011 <- c(ymd("2011-10-16"), ymd("2011-11-30"))
winter.2011 <- c(ymd("2011-11-30"), ymd("2012-04-13"))

# Year 2012
spring.2012 <- c(ymd("2012-04-13"), ymd("2012-05-21"))
summer.2012 <- c(ymd("2012-05-21"), ymd("2012-09-22"))
fall.2012 <- c(ymd("2012-09-22"), ymd("2012-11-15"))
winter.2012 <- c(ymd("2012-11-15"), ymd("2013-04-20"))

# Year 2013
spring.2013 <- c(ymd("2013-04-20"), ymd("2013-06-12"))
summer.2013 <- c(ymd("2013-06-12"), ymd("2013-10-18"))
fall.2013 <- c(ymd("2013-10-18"), ymd("2013-11-19"))
winter.2013 <- c(ymd("2013-11-19"), ymd("2014-04-26"))

# Year 2014
spring.2014 <- c(ymd("2014-04-26"), ymd("2014-06-19"))
summer.2014 <- c(ymd("2014-06-19"), ymd("2014-10-05"))
fall.2014 <- c(ymd("2014-10-05"), ymd("2014-11-24"))
winter.2014 <- c(ymd("2014-11-24"), ymd("2015-04-21"))

# Year 2015
spring.2015 <- c(ymd("2015-04-21"), ymd("2015-06-17"))
summer.2015 <- c(ymd("2015-06-17"), ymd("2015-10-14"))
fall.2015 <- c(ymd("2015-10-14"), ymd("2015-11-26"))
winter.2015 <- c(ymd("2015-11-26"), ymd("2016-04-15"))

# Year 2016
spring.2016 <- c(ymd("2016-04-15"), ymd("2016-05-28"))
summer.2016 <- c(ymd("2016-05-28"), ymd("2016-10-23"))
fall.2016 <- c(ymd("2016-10-23"), ymd("2016-11-21"))
winter.2016 <- c(ymd("2016-11-21"), ymd("2017-04-09"))

# Year 2017
spring.2017 <- c(ymd("2017-04-09"), ymd("2017-06-12"))
summer.2017 <- c(ymd("2017-06-12"), ymd("2017-10-25"))
fall.2017 <- c(ymd("2017-10-25"), ymd("2017-11-18"))
winter.2017 <- c(ymd("2017-11-18"), ymd("2018-04-20"))

# Year 2018
spring.2018 <- c(ymd("2018-04-20"), ymd("2018-06-11"))
summer.2018 <- c(ymd("2018-06-11"), ymd("2018-10-16"))
fall.2018 <- c(ymd("2018-10-16"), ymd("2018-11-27"))
winter.2018 <- c(ymd("2018-11-27"), ymd("2018-12-31"))

# Assign season
# Spring
pike.detections.clean <- pike.detections.clean[
  detection_timestamp_utc %between% spring.2010|
    detection_timestamp_utc %between% spring.2011|
    detection_timestamp_utc %between% spring.2012|
    detection_timestamp_utc %between% spring.2013|
    detection_timestamp_utc %between% spring.2014|
    detection_timestamp_utc %between% spring.2015|
    detection_timestamp_utc %between% spring.2016|
    detection_timestamp_utc %between% spring.2017|
    detection_timestamp_utc %between% spring.2018,
  season := "spring"]

# Summer
pike.detections.clean <- pike.detections.clean[
  detection_timestamp_utc %between% summer.2010|
    detection_timestamp_utc %between% summer.2011|
    detection_timestamp_utc %between% summer.2012|
    detection_timestamp_utc %between% summer.2013|
    detection_timestamp_utc %between% summer.2014|
    detection_timestamp_utc %between% summer.2015|
    detection_timestamp_utc %between% summer.2016|
    detection_timestamp_utc %between% summer.2017|
    detection_timestamp_utc %between% summer.2018,
  season := "summer"]

# Fall
pike.detections.clean <- pike.detections.clean[
  detection_timestamp_utc %between% fall.2010|
    detection_timestamp_utc %between% fall.2011|
    detection_timestamp_utc %between% fall.2012|
    detection_timestamp_utc %between% fall.2013|
    detection_timestamp_utc %between% fall.2014|
    detection_timestamp_utc %between% fall.2015|
    detection_timestamp_utc %between% fall.2016|
    detection_timestamp_utc %between% fall.2017|
    detection_timestamp_utc %between% fall.2018,
  season := "fall"]

# Winter
pike.detections.clean <- pike.detections.clean[
  detection_timestamp_utc %between% winter.2010|
    detection_timestamp_utc %between% winter.2011|
    detection_timestamp_utc %between% winter.2012|
    detection_timestamp_utc %between% winter.2013|
    detection_timestamp_utc %between% winter.2014|
    detection_timestamp_utc %between% winter.2015|
    detection_timestamp_utc %between% winter.2016|
    detection_timestamp_utc %between% winter.2017|
    detection_timestamp_utc %between% winter.2018,
  season := "winter"]

# Exclude locations not in Toronto harbour from analysis
pike.detections.clean <- pike.detections.clean[!(receiver.group %chin% c("EBG-101", "Exhibition Gr.", "GBB-083", "HAM-005",
                                                       "Lake Ontario", "LKO-053", "WL18-018", "WL19-019"))]

# Calculate daily time spent ----
pike.detections.clean[, by = .(animal_id, date(detection_timestamp_utc)), ":="
                      (daily.time.spent = 
                          data.table::shift(detection_timestamp_utc,
                                            fill = detection_timestamp_utc[.N],
                                            type = "lead") - 
                          detection_timestamp_utc)]

# Get and export receiver positions for mapping purpose ----
receiver.positions <- pike.detections.clean[, by = receiver.group, 
                                       .(longitude = mean(deploy_long), 
                                         latitude = mean(deploy_lat))]

fwrite(receiver.positions, "receiver.positions.csv")


# Data for software project ----
detection.data <- copy(pike.detections.clean)

detection.data[, by = .(animal_id, date(detection_timestamp_utc)) ,":=" (
  time_btw_detect = data.table::shift(detection_timestamp_utc, fill = NA, type = "lead") - detection_timestamp_utc)]

detection.data <- na.omit(detection.data)

daily.detection.data <- detection.data[, by = .(animal_id, receiver.group, date(detection_timestamp_utc)), 
                                       .(longitude = mean(deploy_long), 
                                         latitude = mean(deploy_lat),
                                         `time spent` = as.numeric(sum(time_btw_detect)),
                                         `detection count` = uniqueN(detection_timestamp_utc)
                                       )] %>% 
  setnames(old = c("animal_id", "receiver.group"), 
           new = c("animal ID", "receiver group"))

fwrite(daily.detection.data, "C:/Users/diji_/Desktop/Data Science/Projects/UML Exploration of animal behaviour/daily detection data.csv")



# Summary by receiver group for each pike ----
pike.receiver.summary <- pike.detections.clean[, by = .(
  animal_id, date(detection_timestamp_utc), receiver.group),
  .(time.spent = as.numeric(sum(daily.time.spent)))]

# Determine groups of pike based on seasonal detections
pike.receiver.wide <- dcast(pike.receiver.summary,
                            animal_id ~ receiver.group,
                            value.var = "time.spent",
                            fun = mean) %>%
  as.matrix(rownames = "animal_id")

pike.receiver.wide[is.nan(pike.receiver.wide)] = 0

pike.receiver.wide <- pike.receiver.wide %>% t() %>% scale() %>% t() %>% data.frame()

# Get the number of clusters
fviz_nbclust(pike.receiver.wide, FUNcluster = hcut, method = "wss", 
             linecolor = "black") +
  labs(title = "", x = "Number of clusters, k", y = "Total Within Cluster Sum of Squares")

# Hierarchical clustering
pike.hclust <- pike.receiver.wide %>% 
  dist() %>% 
  hclust(method = "ward.D2") %>% 
  as.dendrogram() %>% 
  color_branches(k = 3, col = c("#7570B3", "#E7298A", "#66A61E"))

pike.hclust.treeCut <- cutree(pike.hclust, k = 3)

table(pike.hclust.treeCut)

pike.groups <- levels(as.factor(pike.hclust.treeCut))

# Hierarchical clustering viz
plot(pike.hclust, horiz = T, leaflab = "none")

legend("topleft",
       legend = pike.groups, 
       fill = c("#7570B3", "#E7298A", "#66A61E"),
       bty = "n",
       horiz = T,
       title = "Northern pike clusters:")

# Select the pikes and their cluster group
pike.grps.hcpc <- cbind(pike.receiver.wide, as.factor(pike.hclust.treeCut)) %>%
  data.table(keep.rownames = T) %>%
  setnames(old = c("rn", "as.factor(pike.hclust.treeCut)"),
           new = c("animal_id", "Northern pike cluster")) %>%
  dplyr::select(animal_id, `Northern pike cluster`)

# Merge pike groups to the data table
pike.detections.clean  <- merge(pike.detections.clean, pike.grps.hcpc, by = "animal_id")

# Convert to wide format
receiver.month.wide <- dcast(pike.receiver.summary, 
                             receiver.group ~ lubridate::month(date, label = T), 
                             value.var = "time.spent", 
                             fun = sum) %>% 
  as.matrix(rownames = "receiver.group") %>% t()

ma <- function(x, n = 3){stats::filter(x, rep(1 / n, n), sides = 2, circular = T)}

receiver.month.wide[1:12, ] <- ma(receiver.month.wide[1:12, ], )

receiver.month.wide <- receiver.month.wide %>% scale() %>% t()

receiver.pca <- PCA((receiver.month.wide), scale.unit = F, graph = T, ncp = length(receiver.month.wide))

# Data visualization
fviz_eig(receiver.pca, addlabels = T, ncp = length(receiver.month.wide))


# Perform hierarchical clustering 
receiver.hclust <- receiver.month.wide %>% 
  dist() %>% 
  hclust(method = "ward.D2") %>% 
  as.dendrogram() %>% 
  color_branches(k = 2, col = c("#1B9E77", "#D95F02"))

receiver.hclust.treeCut <- cutree(receiver.hclust, k = 2)

table(receiver.hclust.treeCut)

receiver.groups <- levels(as.factor(receiver.hclust.treeCut))

# Hierarchical clustering viz
receiver.dendrogram <- fviz_dend(receiver.hclust, k = 2,
                                 k_colors = c("#1B9E77", "#D95F02"),
                                 type = "phylogenic", repel = T) +
  theme_dendro()

# Bi-plot visualization
receiver.month.biplot <- fviz_pca_biplot(receiver.pca,
                                         habillage = as.factor(receiver.hclust.treeCut),
                                         geom = "point", 
                                         col.var = "black",
                                         alpha.var = 0.5, 
                                         palette = c("#D95F02", "#1B9E77"),
                                         addEllipses = F, 
                                         title = " ", 
                                         legend.title = "Receiver cluster", 
                                         ggtheme = theme_classic(base_size = 12,
                                                                 base_family = "Times New Roman")) 

receiver.cluster.legend <- get_legend(receiver.month.biplot)

ggdraw() +
  draw_plot(receiver.dendrogram, 0.2, 0.5, 0.5, 0.5) +
  draw_plot(receiver.month.biplot + theme(legend.position = "none"), 0.2, 0, 0.5, 0.5) +
  draw_plot(receiver.cluster.legend, 0.70, 0.3, 0.2, 0.5) +
  draw_plot_label(c("A", "B"), c(0.15, 0.15), c(1, 0.5))

# Select the receivers and their cluster groups
receiver.grps.hcpc <- cbind(receiver.month.wide, as.factor(receiver.hclust.treeCut)) %>% data.table(keep.rownames = T) %>%
  setnames(old = c("rn", "V13"), new = c("receiver.group", "Receiver cluster")) %>%
  dplyr::select(receiver.group, `Receiver cluster`)

# Merge receiver groups to the data table
pike.detections.clean <- merge(pike.detections.clean, receiver.grps.hcpc, by = "receiver.group")

# Get the different lengths within each pike group
length.location.check <- pike.detections.clean[, by = .(`Northern pike cluster`),
                                        .(`Northern pike length` = unique(length))]

# Get some summary statistics for the different pike groups
pike.lengths <- length.location.check[, by = `Northern pike cluster`,
                                      .(`minimum length` = min(`Northern pike length`),
                                        `mean length` = mean(`Northern pike length`),
                                        `standard deviation` = sd(`Northern pike length`),
                                        `maximum length` = max(`Northern pike length`))]

# Perform Kruskal Wallis test the check the presence of length differences among the different pike groups
kruskal.test.pike.length <- kruskal.test(`Northern pike length` ~ 
                                           `Northern pike cluster`,
                                         data = length.location.check)

# Create a table of results for the Kruskal Wallis test and export it 
kruskal.result.pike.length <- data.table(
  `Chi-squared` = round(kruskal.test.pike.length$statistic, 2), 
  `Degrees of freedom` = kruskal.test.pike.length$parameter,
  `p-value` = round(kruskal.test.pike.length$p.value, 4))

fwrite(kruskal.result.pike.length, "kruskal.result.pike.length.csv")

# Perform Dunn's test to compare length differences among the groups
dunn.test.pike.length <- dunnTest(`Northern pike length` ~
                                    `Northern pike cluster`, 
                                  data= length.location.check, method="bh")

# Create a table of results for the Dunn's test and export it 
dunn.result.pike.length <- data.table(dunn.test.pike.length$res) %>% 
  setnames(old = c("Comparison", "Z", "P.unadj", "P.adj"),
           new = c("Cluster comparison", "Z-score", "P unadjusted", "P adjusted")) %>% 
  .[, c("Z-score", "P unadjusted", "P adjusted") := 
      .(round(`Z-score`, 4), 
        round(`P unadjusted`, 4), 
        round(`P adjusted`, 4))]

fwrite(dunn.result.pike.length, "dunn.result.pike.length.csv")

# Voilin-boxplot showing length distribution among groups
ggplot(data = length.location.check, 
       aes(x = as.factor(`Northern pike cluster`),
           y = `Northern pike length`,
           fill = as.factor(`Northern pike cluster`))) +
  geom_violin(trim = F) +
  geom_boxplot(width = 0.4) +
  scale_fill_manual(values = c("#7570B3", "#E7298A", "#66A61E")) +
  labs(title = " ", x = "Northern pike cluster", 
       y = "Northern pike length (m)",
       fill = "Northern pike cluster") +
  theme_classic(base_size = 13, base_family = "Times New Roman") +
  theme(legend.position = "none")

# Rename "receiver.group" column in receiver data 
receiver.grps.hcpc %>% setnames(old = "receiver.group", new = "Receiver group")

# Merge receiver groups to the habitat data
rec.grps.by.detect <- merge(receiver.grps.hcpc, habitat.data.clean, by = "Receiver group") 

# Convert the groups to factors
rec.grps.by.detect[,c("Receiver cluster")
            := lapply(.SD, as.factor),
            .SDcols = c("Receiver cluster")]

# Summarize the environmental measures of the receiver groups and export table
# Remember to perform wilcox test calculations without rounding
rec.grps.summary <- rec.grps.by.detect[
  , by = .(`Receiver cluster`),
  .(`Mean Fetch (m)` = round(mean(`Fetch`), 2), `St. dev. of fetch` = round(sd(`Fetch`), 2),
    `Mean Depth (m)` = round(mean(`Mean depth`), 2), `St. dev. of depth` = round(sd(`Mean depth`), 2),
    `Mean % SAV` = round(mean(`% SAV`), 2), `St. dev. of SAV` = round(sd(`% SAV`), 2),
    `Mean Strat. temp (?C)` = round(mean(`Stratification temp.`), 2),
    `St. dev. of Strat. temp (?C)` = round(sd(`Stratification temp.`), 2))] 

fwrite(rec.grps.summary, "rec.grps.summary.csv")

rec.grps.summary <- rec.grps.by.detect[
     , by = .(`Receiver cluster`),
     .(`Mean fetch` = mean(`Fetch`), `St. dev. of fetch` = sd(`Fetch`),
             `Mean depth` = mean(`Mean depth`), `St. dev. of depth` = sd(`Mean depth`),
             `Mean SAV` = mean(`% SAV`), `St. dev. of SAV` = sd(`% SAV`),
             `Mean strat. temp` = mean(`Stratification temp.`),
             `St. dev. of strat. temp` = sd(`Stratification temp.`))] 


# Perform Wilcox test 
wilcox.fetch <- wilcox.test(Fetch ~ `Receiver cluster`, data = rec.grps.by.detect, exact = F)
wilcox.depth <- wilcox.test(`Mean depth` ~ `Receiver cluster`, data = rec.grps.by.detect, exact = F)
wilcox.sav <- wilcox.test(`% SAV` ~ `Receiver cluster`, data = rec.grps.by.detect, exact = F)
wilcox.strat <- wilcox.test(`Stratification temp.` ~ `Receiver cluster`, data = rec.grps.by.detect, exact = F)

# Create a table of results for the Wilcox test and export results 
wilcox.env.vars <- data.table(
  Variable = c(
    "Fetch", "Depth", "% SAV", "Stratification temp. (?C)"), 
  `Test statistic, W` = c(
    round(wilcox.fetch$statistic, 1), 
    round(wilcox.depth$statistic, 1),
    round(wilcox.sav$statistic, 1),
    round(wilcox.strat$statistic, 1)),
  `p-value` = c(
    round(wilcox.fetch$p.value, 4),
    round(wilcox.depth$p.value, 4), 
    round(wilcox.sav$p.value, 4),
    round(wilcox.strat$p.value, 4)))

fwrite(wilcox.env.vars, "wilcox.env.vars.csv")

# Visualize distribution of measures among groups 
fetch.plot.2 <- ggplot(data = rec.grps.by.detect,
                       aes(x = `Receiver cluster`, y = Fetch,
                           fill = `Receiver cluster`)) +
  geom_violin(trim = F) +
  geom_boxplot(width = 0.2) +
  scale_fill_manual(values = c("#D95F02", "#1B9E77")) +
  labs(x = "Receiver cluster", y = "Fetch (m)") +
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(legend.position = "none")

depth.plot.2 <- ggplot(data = rec.grps.by.detect,
                       aes(x = `Receiver cluster`, y = `Mean depth`,
                           fill = `Receiver cluster`)) +
  geom_violin(trim = F) +
  geom_boxplot(width = 0.2) +
  scale_fill_manual(values = c("#D95F02", "#1B9E77")) +
  labs(x = "Receiver cluster", y = "depth (m)") +
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(legend.position = "none")

sav.plot.2 <- ggplot(data = rec.grps.by.detect, 
                     aes(x = `Receiver cluster`, y = `% SAV`,
                         fill = `Receiver cluster`)) +
  geom_violin(trim = F) +
  geom_boxplot(width = 0.2) +
  scale_fill_manual(values = c("#D95F02", "#1B9E77")) +
  labs(x = "Receiver cluster", y = "% Submerged Aquatic Vegetation", fill = "Group") +
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(legend.position = "none")

strat.plot.2 <- ggplot(data = rec.grps.by.detect, 
                       aes(x = `Receiver cluster`, y = `Stratification temp.`,
                           fill = `Receiver cluster`)) +
  geom_violin(trim = F) +
  geom_boxplot(width = 0.2) +
  scale_fill_manual(values = c("#D95F02", "#1B9E77")) +
  labs(x = "Receiver cluster", y = expression(paste("Stratification temp. ", " (", degree, "C", ")")), fill = "Group") +
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(legend.position = "none")

# Viz environmental vars by receiver cluster
ggdraw() +
  draw_plot(fetch.plot.2, 0, 0.5, 0.5, 0.5) +
  draw_plot(depth.plot.2, 0.5, 0.5,  0.5, 0.5) +
  draw_plot(sav.plot.2, 0.02, 0, 0.5, 0.5) +
  draw_plot(strat.plot.2, 0.5, 0, 0.5, 0.5) +
  draw_plot_label(c("A", "B", "C", "D"),
                  c(0, 0.5, 0, 0.5),
                  c(1, 1, 0.5, 0.5))

# Temporal Depth and % SAV visualizations for the different pike clusters 
# Select the necessary columns from pike detection data
pike.hab.behav <- pike.detections.clean[, c("receiver.group", "detection_timestamp_utc",
                                            "season", "daily.time.spent",
                                            "Northern pike cluster", "Receiver cluster")] %>% 
  setnames(old = c("receiver.group"), new = "Receiver group") 

# Select the necessary columns from the habitat data 
hab.data <- rec.grps.by.detect[, c("Receiver group", "Mean depth", "% SAV")]

pike.hab.behav <- merge(pike.hab.behav, hab.data, by = "Receiver group")

pike.hab.behav <- pike.hab.behav[
  , by = .(`Northern pike cluster`,
           lubridate::month(detection_timestamp_utc, label = T)),
  .(`weighted mean depth` = wtd.mean(x = `Mean depth`,
                                     weights = as.numeric(daily.time.spent)), 
    `weighted st. dev. depth` = sqrt(wtd.var(x = `Mean depth`,
                                             weights = as.numeric(daily.time.spent))),
    `weighted mean sav` = wtd.mean(x = `% SAV`,
                                   weights = as.numeric(daily.time.spent)), 
    `weighted st. dev. sav` = sqrt(wtd.var(x = `% SAV`,
                                           weights = as.numeric(daily.time.spent))))] %>% 
  setnames(old = "lubridate", new = "Month")

# Group 1 pike depth preference through time 
grpOnePikeBehavDepth <- ggplot(data = pike.hab.behav[`Northern pike cluster` == 1], aes(x = Month)) +
  geom_bar(aes(y = `weighted mean depth`), stat = "identity", fill = "slategray1") +
  geom_errorbar(aes(ymin = `weighted mean depth` - `weighted st. dev. depth`, 
                    ymax = `weighted mean depth` + `weighted st. dev. depth`), 
                width = 0.3, colour = "black", alpha = 0.5) +
  labs(y = "Weighted Mean Depth (m)") +
  theme_classic() +
  theme(axis.line.x = element_blank(), 
        axis.line.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))                                         

# Group 1 pike SAV preference through time
grpOnePikeBehavSAV <- ggplot(data = pike.hab.behav[`Northern pike cluster` == 1], aes(x = Month)) +
  geom_bar(aes(y = `weighted mean sav`), stat = "identity", fill = "olivedrab") +
  geom_errorbar(aes(ymin = `weighted mean sav` - `weighted st. dev. sav`, 
                    ymax = `weighted mean sav` + `weighted st. dev. sav`), 
                width = 0.3, colour = "black", alpha = 0.5) +
  labs(y = "Weighted mean % SAV") +
  theme_classic() +
  theme(axis.line.x = element_blank(), 
        axis.line.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Temporal activity for group 1 pikes ----
group.1.pike.temporal.summary <- pike.detections.clean[
  `Northern pike cluster` == 1,
  by = .(`Receiver cluster`, 
         lubridate::month(detection_timestamp_utc, label = T)), 
  .(`Total time` = as.numeric(sum(daily.time.spent)))] %>% 
  setnames(old = "lubridate", new = "Month") %>% 
  .[, by = Month, ":=" 
    (`Percent of monthly time` = (`Total time`/sum(`Total time`))*100)]  %>% 
  .[, `Percent of yearly time` := (`Total time`/sum(`Total time`))*100]

group.1.pike.temporal.summary[, "Receiver cluster" := 
                                lapply(.SD, as.factor), 
                              .SDcols = c("Receiver cluster")]

g1.percent.yearly.res <- ggplot(group.1.pike.temporal.summary,
                             aes(x = Month, y = `Percent of yearly time`,
                                 group = `Receiver cluster`, color = `Receiver cluster`)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("#D95F02", "#1B9E77")) +
  labs(y = "% Time spent") +
  theme_classic() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g1.percent.monthly.res <- ggplot(group.1.pike.temporal.summary,
                              aes(x = Month, y = `Percent of monthly time`,
                                  group = `Receiver cluster`, color = `Receiver cluster`)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("#D95F02", "#1B9E77")) +
  labs(y = "% Time spent") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g1.temporal.rec.legend <- get_legend(g1.percent.monthly.res + theme(legend.position = "right"))

# For paper ---
ggdraw() +
  draw_plot(g1.percent.yearly.res, 0.25, 0.5, 0.5, 0.5) +
  draw_plot(g1.percent.monthly.res, 0.25, 0, 0.5, 0.5) +
  draw_plot(g1.temporal.rec.legend, 0.8, 0.5, 0.2, 0.2) +
  draw_plot_label(c("A", "B"), 
                  c(0.20, 0.20), 
                  c(1, 0.5))
#For thesis ----
ggdraw() +
  draw_plot(g1.percent.yearly.res, 0, 0.5, 0.4, 0.5) +
  draw_plot(g1.percent.monthly.res, 0, 0, 0.4, 0.5) +
  draw_plot(grpOnePikeBehavDepth, 0.6, 0.5, 0.4, 0.5) +
  draw_plot(grpOnePikeBehavSAV, 0.6, 0, 0.4, 0.5) +
  draw_plot(g1.temporal.rec.legend, 0.4, 0.45, 0.2, 0.2) + 
  draw_plot_label(c("A", "B", "C", "D"), 
                  c(0, 0, 0.55, 0.55), 
                  c(1, 0.5, 1, 0.5))
  
# Group 2 pike depth preference through time 
grpTwoPikeBehavDepth <- ggplot(data = pike.hab.behav[`Northern pike cluster` == 2], aes(x = Month)) +
  geom_bar(aes(y = `weighted mean depth`), stat = "identity", fill = "slategray1") +
  geom_errorbar(aes(ymin = `weighted mean depth` - `weighted st. dev. depth`, 
                    ymax = `weighted mean depth` + `weighted st. dev. depth`), 
                width = 0.3, colour = "black", alpha = 0.5) +
  labs(y = "Weighted Mean Depth (m)") +
  theme_classic() +
  theme(axis.line.x = element_blank(), 
        axis.line.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))                                       

# Group 2 pike SAV preference through time
grpTwoPikeBehavSAV <- ggplot(data = pike.hab.behav[`Northern pike cluster` == 2], aes(x = Month)) +
  geom_bar(aes(y = `weighted mean sav`), stat = "identity", fill = "olivedrab") +
  geom_errorbar(aes(ymin = `weighted mean sav` - `weighted st. dev. sav`, 
                    ymax = `weighted mean sav` + `weighted st. dev. sav`), 
                width = 0.3, colour = "black", alpha = 0.5) +
  labs(y = "Weighted mean % SAV") +
  theme_classic() +
  theme(axis.line.x = element_blank(), 
        axis.line.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Temporal activity for group 2 pikes ----
group.2.pike.temporal.summary <- pike.detections.clean[
  `Northern pike cluster` == 2,
  by = .(`Receiver cluster`, 
         lubridate::month(detection_timestamp_utc, label = T)), 
  .(`Total time` = as.numeric(sum(daily.time.spent)))] %>% 
  setnames(old = "lubridate", new = "Month") %>% 
  .[, by = Month, ":=" 
    (`Percent of monthly time` = (`Total time`/sum(`Total time`))*100)]  %>% 
  .[, `Percent of yearly time` := (`Total time`/sum(`Total time`))*100]

group.2.pike.temporal.summary[, "Receiver cluster" := 
                                lapply(.SD, as.factor), 
                              .SDcols = c("Receiver cluster")]

g2.percent.yearly.res <- ggplot(group.2.pike.temporal.summary,
                             aes(x = Month, y = `Percent of yearly time`,
                                 group = `Receiver cluster`, color = `Receiver cluster`)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("#D95F02", "#1B9E77")) +
  labs(y = "% Time spent") +
  theme_classic() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g2.percent.monthly.res <- ggplot(group.2.pike.temporal.summary,
                              aes(x = Month, y = `Percent of monthly time`,
                                  group = `Receiver cluster`, color = `Receiver cluster`)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("#D95F02", "#1B9E77")) +
  labs(y = "% Time spent") +
  theme_classic() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g2.temporal.rec.legend <- get_legend(g2.percent.monthly.res + theme(legend.position = "right"))

# For paper ----
ggdraw() +
  draw_plot(g2.percent.yearly.res, 0.25, 0.5, 0.5, 0.5) +
  draw_plot(g2.percent.monthly.res, 0.25, 0, 0.5, 0.5) +
  draw_plot(g2.temporal.rec.legend, 0.8, 0.5, 0.2, 0.2) +
  draw_plot_label(c("A", "B"), 
                  c(0.20, 0.20), 
                  c(1, 0.5))
#For thesis ----
ggdraw() +
  draw_plot(g2.percent.yearly.res, 0, 0.5, 0.4, 0.5) +
  draw_plot(g2.percent.monthly.res, 0, 0, 0.4, 0.5) +
  draw_plot(grpTwoPikeBehavDepth, 0.6, 0.5, 0.4, 0.5) +
  draw_plot(grpTwoPikeBehavSAV, 0.6, 0, 0.4, 0.5) +
  draw_plot(g2.temporal.rec.legend, 0.4, 0.45, 0.2, 0.2) + 
  draw_plot_label(c("A", "B", "C", "D"), 
                  c(0, 0, 0.55, 0.55), 
                  c(1, 0.5, 1, 0.5))

# Group 3 pike depth preference through time 
grpThreePikeBehavDepth <- ggplot(data = pike.hab.behav[`Northern pike cluster` == 3], aes(x = Month)) +
  geom_bar(aes(y = `weighted mean depth`), stat = "identity", fill = "slategray1") +
  geom_errorbar(aes(ymin = `weighted mean depth` - `weighted st. dev. depth`, 
                    ymax = `weighted mean depth` + `weighted st. dev. depth`), 
                width = 0.3, colour = "black", alpha = 0.5) +
  labs(y = "Weighted Mean Depth (m)") +
  theme_classic() +
  theme(axis.line.x = element_blank(), 
        axis.line.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))                                       

# Group 2 pike SAV preference through time
grpThreePikeBehavSAV <- ggplot(data = pike.hab.behav[`Northern pike cluster` == 3], aes(x = Month)) +
  geom_bar(aes(y = `weighted mean sav`), stat = "identity", fill = "olivedrab") +
  geom_errorbar(aes(ymin = `weighted mean sav` - `weighted st. dev. sav`, 
                    ymax = `weighted mean sav` + `weighted st. dev. sav`), 
                width = 0.3, colour = "black", alpha = 0.5) +
  labs(y = "Weighted mean % SAV") +
  theme_classic() +
  theme(axis.line.x = element_blank(), 
        axis.line.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Temporal activity for group 3 pikes ----
group.3.pike.temporal.summary <- pike.detections.clean[
  `Northern pike cluster` == 3,
  by = .(`Receiver cluster`, 
         lubridate::month(detection_timestamp_utc, label = T)), 
  .(`Total time` = as.numeric(sum(daily.time.spent)))] %>% 
  setnames(old = "lubridate", new = "Month") %>% 
  .[, by = Month, ":=" 
    (`Percent of monthly time` = (`Total time`/sum(`Total time`))*100)]  %>% 
  .[, `Percent of yearly time` := (`Total time`/sum(`Total time`))*100]

group.3.pike.temporal.summary[, "Receiver cluster" := 
                                lapply(.SD, as.factor), 
                              .SDcols = c("Receiver cluster")]

g3.percent.yearly.res <- ggplot(group.3.pike.temporal.summary,
                                aes(x = Month, y = `Percent of yearly time`,
                                    group = `Receiver cluster`, color = `Receiver cluster`)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("#D95F02", "#1B9E77")) +
  labs(y = "% Time spent") +
  theme_classic() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g3.percent.monthly.res <- ggplot(group.3.pike.temporal.summary,
                                 aes(x = Month, y = `Percent of monthly time`,
                                     group = `Receiver cluster`, color = `Receiver cluster`)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("#D95F02", "#1B9E77")) +
  labs(y = "% Time spent") +
  theme_classic() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g3.temporal.rec.legend <- get_legend(g3.percent.monthly.res + theme(legend.position = "right"))

# For paper ----
ggdraw() +
  draw_plot(g3.percent.yearly.res, 0.25, 0.5, 0.5, 0.5) +
  draw_plot(g3.percent.monthly.res, 0.25, 0, 0.5, 0.5) +
  draw_plot(g3.temporal.rec.legend, 0.8, 0.5, 0.2, 0.2) +
  draw_plot_label(c("A", "B"), 
                  c(0.20, 0.20), 
                  c(1, 0.5))

#For thesis ----
ggdraw() +
  draw_plot(g3.percent.yearly.res, 0, 0.5, 0.4, 0.5) +
  draw_plot(g3.percent.monthly.res, 0, 0, 0.4, 0.5) +
  draw_plot(grpThreePikeBehavDepth, 0.6, 0.5, 0.4, 0.5) +
  draw_plot(grpThreePikeBehavSAV, 0.6, 0, 0.4, 0.5) +
  draw_plot(g3.temporal.rec.legend, 0.4, 0.45, 0.2, 0.2) + 
  draw_plot_label(c("A", "B", "C", "D"), 
                  c(0, 0, 0.55, 0.55), 
                  c(1, 0.5, 1, 0.5))

# Calculate time spent by groupings 
overall.time.spent <- pike.detections.clean[
  , by = .(season,
           `Northern pike cluster`, 
           `Receiver cluster`,
           `receiver.group`), 
  .(`time spent` = as.numeric(sum(daily.time.spent)))]

# Get the top 3 receivers by pike cluster
overall.time.spent.top.5 <- overall.time.spent[, by = .(`Northern pike cluster`, receiver.group), 
                   .(`Total time spent` = sum(`time spent`))] %>% 
  .[order(`Northern pike cluster`, -`Total time spent`)] %>% 
  .[, head(.SD, 3), by = `Northern pike cluster`]

overall.time.spent.top.5 <- merge(
  overall.time.spent,
  overall.time.spent.top.5,
  by = c("Northern pike cluster", "receiver.group"))

# Visuals for the receiver detections 
receiver.activity <- overall.time.spent[
  , by = .(`Northern pike cluster`, receiver.group, `Receiver cluster`), 
  .(`Total time spent` = sum(`time spent`))] %>% 
  .[, by  = .(`Northern pike cluster`), ":="
    (`Percent time spent` = (`Total time spent`/sum(`Total time spent`))*100)] %>% 
  .[order(`Northern pike cluster`, `Total time spent`)]

receiver.activity$receiver.group <- as.factor(receiver.activity$receiver.group)
receiver.activity$`Receiver cluster` <- as.factor(receiver.activity$`Receiver cluster`)

# Bar plot of time spent at the receiver groups by cluster 1 pike ----
receiver.activity[`Northern pike cluster` == 1] %>% 
  mutate(receiver.group = fct_reorder(receiver.group, -`Percent time spent`)) %>% 
  ggplot(aes(y=`Percent time spent`, x=receiver.group, fill = `Receiver cluster`)) + 
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("") +
  scale_fill_manual(values = c("#D95F02", "#1B9E77")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# Percent time spent at the receiver clusters by cluster 1 pike ----
time.spent.at.rec.cluster <- receiver.activity[
  `Northern pike cluster` == 1, by = .(`Receiver cluster`), 
  .(`time spent` = sum(`Total time spent`))] 

time.spent.at.rec.cluster[, `percent time spent` := `time spent`/sum(`time spent`)]

# Bar plot of time spent at the receiver groups by cluster 2 pike ----
receiver.activity[`Northern pike cluster` == 2] %>% 
  mutate(receiver.group = fct_reorder(receiver.group, -`Percent time spent`)) %>% 
  ggplot(aes(y=`Percent time spent`, x=receiver.group, fill = `Receiver cluster`)) + 
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("") +
  scale_fill_manual(values = c("#D95F02", "#1B9E77")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# Percent time spent at the receiver clusters by cluster 2 pike ----
time.spent.at.rec.cluster <- receiver.activity[
  `Northern pike cluster` == 2, by = .(`Receiver cluster`), 
  .(`time spent` = sum(`Total time spent`))] 

time.spent.at.rec.cluster[, `percent time spent` := `time spent`/sum(`time spent`)]

# Bar plot of time spent at the receiver groups by cluster 3 pike ----
receiver.activity[`Northern pike cluster` == 3] %>% 
  mutate(receiver.group = fct_reorder(receiver.group, -`Percent time spent`)) %>% 
  ggplot(aes(y=`Percent time spent`, x=receiver.group, fill = `Receiver cluster`)) + 
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("") +
  scale_fill_manual(values = c("#D95F02", "#1B9E77")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# Percent time spent at the receiver clusters by cluster 1 pike ----
time.spent.at.rec.cluster <- receiver.activity[
  `Northern pike cluster` == 3, by = .(`Receiver cluster`), 
  .(`time spent` = sum(`Total time spent`))] 

time.spent.at.rec.cluster[, `percent time spent` := `time spent`/sum(`time spent`)]

# Get the proportion of time spent by season
overall.time.spent.top.5[, by = .(`Northern pike cluster`, season), ":="
                         (`Seasonal proportion` = `time spent`/sum(`time spent`))] %>% 
  .[, by = .(`Northern pike cluster`, receiver.group), ":="
    (`Receiver proportion` = `time spent`/sum(`time spent`))]

# Convert to ordered factor
overall.time.spent.top.5$season <- factor(overall.time.spent.top.5$season, 
                                          levels = c("winter", "spring", "summer", "fall"))

# Visualization of seasonal proportions 
ggplot(overall.time.spent.top.5[`Northern pike cluster` == 3],
       aes(fill= `receiver.group`, y=`Seasonal proportion`, x=season)) + 
  geom_bar(position="stack", stat="identity", color = "black") +
  geom_text(aes(label = round(`Seasonal proportion`, 2)),
            position = position_stack(), 
            vjust = 1.5, hjust = 0) +
  labs(x = "Season", y = "Proportion of time spent", fill = "Receiver group") +
  scale_fill_brewer(palette = "Greys") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# Visualization of receiver proportions 
ggplot(overall.time.spent.top.5[`Northern pike cluster` == 3],
       aes(fill= `receiver.group`, y=`Receiver proportion`, x=season)) + 
  geom_bar(position="stack", stat="identity", color = "black") +
  geom_text(aes(label = round(`Receiver proportion`, 2)),
            position = position_stack(), 
            vjust = 1.5, hjust = 0) +
  labs(x = "Season", y = "Proportion of time spent", fill = "Receiver group") +
  scale_fill_brewer(palette = "Greys") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# Visualize environmental vars for top 3 most utilized receiver groups
habitat.data.clean$`Receiver group`

the.top.3.rec.by.pike.group <- c("Mid.Waterfront", "Spadina", "TOI-041", 
                                 "Cherry Beach", "Embayment C", "OHM", 
                                 "Cell 2", "Cell 3")
env.vars.top.3.by.pike <- habitat.data.clean[`Receiver group` %chin% the.top.3.rec.by.pike.group]

env.vars.top.3.by.pike <- merge(env.vars.top.3.by.pike, receiver.grps.hcpc, by = "Receiver group")

factor(env.vars.top.3.by.pike$`Receiver cluster`, levels = c(1, 2))

# Fetch visualization ----
fetch.top.3 <- env.vars.top.3.by.pike %>% 
  mutate(`Receiver group` = fct_reorder(`Receiver group`, -Fetch)) %>% 
  ggplot(aes(x=as.factor(`Receiver group`),
             y = Fetch, 
             fill=as.factor(`Receiver cluster`) )) + 
  geom_bar(position="dodge", stat="identity", color = "black") +
  labs(x = " ", y = "Fetch (m)", fill = "Receiver cluster") +
  scale_fill_manual(values = c("#D95F02", "#1B9E77")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90), 
        legend.position = "none")

# Depth visualization ----
depth.top.3 <- env.vars.top.3.by.pike %>% 
  mutate(`Receiver group` = fct_reorder(`Receiver group`, -`Mean depth`)) %>% 
  ggplot(aes(x=as.factor(`Receiver group`),
             y = `Mean depth`, 
             fill=as.factor(`Receiver cluster`) )) + 
  geom_bar(position="dodge", stat="identity", color = "black") +
  labs(x = " ", y = "Mean depth (m)", fill = "Receiver cluster") +
  scale_fill_manual(values = c("#D95F02", "#1B9E77")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90), 
        legend.position = "none")

# %SAV visualization 
sav.top.3 <- env.vars.top.3.by.pike %>% 
  mutate(`Receiver group` = fct_reorder(`Receiver group`, -`% SAV`)) %>% 
  ggplot(aes(x=as.factor(`Receiver group`),
             y = `% SAV`, 
             fill=as.factor(`Receiver cluster`) )) + 
  geom_bar(position="dodge", stat="identity", color = "black") +
  labs(x = " ", y = "% SAV", fill = "Receiver cluster") +
  scale_fill_manual(values = c("#D95F02", "#1B9E77")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90), 
        legend.position = "none")

# Stratification temperature visualization 
strat.top.3 <- env.vars.top.3.by.pike %>% 
  mutate(`Receiver group` = fct_reorder(`Receiver group`, -`Stratification temp.`)) %>% 
  ggplot(aes(x=as.factor(`Receiver group`),
             y = `Stratification temp.`, 
             fill=as.factor(`Receiver cluster`) )) + 
  geom_bar(position="dodge", stat="identity", color = "black") +
  labs(x = " ", y = expression(paste("Strat. temp. ", " (", degree, "C", ")")),
       fill = "Receiver cluster") +
  scale_fill_manual(values = c("#D95F02", "#1B9E77")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90), 
        legend.position = "none")

rec.legend <- get_legend(strat.top.3 + theme(legend.position = "right"))

ggdraw() +
  draw_plot(fetch.top.3, 0, 0.5, 0.4, 0.5) +
  draw_plot(depth.top.3, 0.4, 0.5, 0.4, 0.5) +
  draw_plot(sav.top.3, 0, 0, 0.4, 0.5) +
  draw_plot(strat.top.3, 0.4, 0, 0.4, 0.5) +
  draw_plot(rec.legend, 0.8, 0.4, 0.2, 0.2) +
  draw_plot_label(c("A", "B", "C", "D"),
                  c(0.15, 0.75, 0.15, 0.75),
                  c(1, 1, 0.5, 0.5))

# Get the data for group 1 pike detections ----
groupOnePikeDetections <- pike.detections.clean[
  `Northern pike cluster` == 1] %>% 
  .[, c("animal_id", "season", "detection_timestamp_utc", 
        "deploy_long", "deploy_lat", "daily.time.spent")] %>% 
  .[, by = .(animal_id, season, date(detection_timestamp_utc), 
             deploy_long, deploy_lat), 
             .(`time spent (hrs)` = round(as.numeric(sum(daily.time.spent))/(60*60), 2))] %>% 
  setnames(old = c("deploy_long", "deploy_lat"), 
           new = c("longitude", "latitude"))

fwrite(groupOnePikeDetections, "groupOnePikeDetections.csv")

# Get the data for group 2 pike detections ----
groupTwoPikeDetections <- pike.detections.clean[
  `Northern pike cluster` == 2] %>% 
  .[, c("animal_id", "season", "detection_timestamp_utc", 
        "deploy_long", "deploy_lat", "daily.time.spent")] %>% 
  .[, by = .(animal_id, season, date(detection_timestamp_utc), 
             deploy_long, deploy_lat), 
    .(`time spent (hrs)` = round(as.numeric(sum(daily.time.spent))/(60*60), 2))] %>% 
  setnames(old = c("deploy_long", "deploy_lat"), 
           new = c("longitude", "latitude"))

fwrite(groupTwoPikeDetections, "groupTwoPikeDetections.csv")

# Get the data for group 3 pike detections ----
groupThreePikeDetections <- pike.detections.clean[
  `Northern pike cluster` == 3] %>% 
  .[, c("animal_id", "season", "detection_timestamp_utc", 
        "deploy_long", "deploy_lat", "daily.time.spent")] %>% 
  .[, by = .(animal_id, season, date(detection_timestamp_utc), 
             deploy_long, deploy_lat), 
    .(`time spent (hrs)` = round(as.numeric(sum(daily.time.spent))/(60*60), 2))] %>% 
  setnames(old = c("deploy_long", "deploy_lat"), 
           new = c("longitude", "latitude"))

fwrite(groupThreePikeDetections, "groupThreePikeDetections.csv")

# Get 90% of the most used receiver groups by pike cluster 
library(Hmisc) # For weighted mean

receiver.activity.100 <- overall.time.spent[
  , by = .(`Northern pike cluster`, receiver.group, `Receiver cluster`), 
  .(`Total time spent` = sum(`time spent`))] %>% 
  .[, by  = .(`Northern pike cluster`), ":="
    (`Percent time spent` = (`Total time spent`/sum(`Total time spent`))*100)] %>% 
  .[order(`Northern pike cluster`, -`Total time spent`)]

receiver.activity.100 <- receiver.activity.100 %>% 
  setnames(old = "receiver.group", new = "Receiver group") 

receiver.activity.100 <- merge(receiver.activity.100, habitat.data.clean, by = "Receiver group") %>% 
  .[order(`Northern pike cluster`, `Percent time spent`)]

receiver.activity.100 <- receiver.activity.100[
  , by = .(`Northern pike cluster`, `Receiver clu*ster`), 
  .(`Number of receiver groups` = uniqueN(`Receiver group`),
    `Mean Depth (m)` = round(wtd.mean(`Mean depth`, `Percent time spent`), 2),
    `Standard dev. of depth (m)` = round(sqrt(wtd.var(`Mean depth`, `Percent time spent`)), 2),
    `Mean % SAV` = round(wtd.mean(`% SAV`, `Percent time spent`), 2),
    `Standard dev. of %SAV ` = round(sqrt(wtd.var(`% SAV`, `Percent time spent`)), 2))] %>% 
  .[order(`Northern pike cluster`)
  ]

fwrite(receiver.activity.100, "pikeClust.recClust.EnvVars.csv")

# Export the data ----
fwrite(pike.detections.clean, "detection.grouped.csv")