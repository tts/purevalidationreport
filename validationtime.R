library(readxl)
library(tidyverse)
library(RColorBrewer)

# Data created in 2017, PURE report
data <- read_excel("Validation_time-23_11_2017.xls")
names(data) <- c("Id", "Type", "Created", "Step", "Entered_step", "Source", "Unit")
data$Created <- as.Date(data$Created, format = "%d/%m/%Y")
data$Entered_step <- as.Date(data$Entered_step, format = "%d/%m/%Y")
data$Source <- ifelse(is.na(data$Source), "Manual", data$Source)

# Calculate the number of days between creation and validation
validated <- data %>% 
  filter(Step == "Validated") %>% 
  mutate(DiffDays = Entered_step - Created)

# Group by source and calculate median
validated_grouped_source <- validated %>% 
  group_by(Source) %>% 
  summarise(MedValidDays = round(median(DiffDays), 2))

# Ditto by type
validated_grouped_type <- validated %>% 
  group_by(Type) %>% 
  summarise(MedValidDays = round(median(DiffDays), 2)) %>% 
  filter(!is.na(Type))

# and unit
validated_grouped_unit <- validated %>% 
  group_by(Unit) %>% 
  summarise(MedValidDays = round(median(DiffDays), 2)) 

# Shorten type and unit names for plotting
validated_grouped_type$Type <- gsub("^.*\\s-\\s([A-Z].*)", "\\1)", validated_grouped_type$Type)
validated_grouped_type$Type <- gsub("\\)", "", validated_grouped_type$Type)
validated_grouped_type$Type <- substr(validated_grouped_type$Type, 1, 30)
validated_grouped_unit$Unit <- substr(validated_grouped_unit$Unit, 1, 30)

doplot <- function(d, col, cc, title) {

  getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
  
  g <- ggplot(d, 
              aes_string(x = paste0("reorder(", col, ", -MedValidDays)"),
                         y = "MedValidDays",
                         fill = col)) +
    geom_bar(position="dodge", stat="identity") +
    scale_fill_manual(values = getPalette(cc)) +
    scale_y_continuous(expand = c(0,0), breaks = seq(from=0, to=)) +
    ggtitle(title) +
    labs(y = "Days", x = as.character(col)) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 75, hjust = 1))
  
  g
  
}

colourCount <- length(unique(validated_grouped_source$Source))
doplot(d = validated_grouped_source, col = "Source", 
       cc = colourCount, title = "Median validation time in 2017 by source")

colourCount <- length(unique(validated_grouped_type$Type))
doplot(d = validated_grouped_type, col = "Type", 
       cc = colourCount, title = "Median validation time in 2017 by type")

colourCount <- length(unique(validated_grouped_unit$Unit))
doplot(d = validated_grouped_unit, col = "Unit", 
       cc = colourCount, title = "Median validation time in 2017 by unit")

