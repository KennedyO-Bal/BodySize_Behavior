library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=12, base_family = "Times"))
library(egg)
###Showing both Ovary/Body size for combined species and as single plots
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

solitary_raw2 <- read_excel_allsheets("Behavioral categories_solitary_Body size.xlsx")
social_raw2 <- read_excel_allsheets("Behavioral categories_Social_Body size.xlsx") 

social2 <- social_raw2[sapply(social_raw2, ncol)==7] %>%
  bind_rows(.id="video") %>%
  mutate(
    type="Social bees"
  )

solitary2 <- solitary_raw2[sapply(solitary_raw2, ncol)==7] %>%
  bind_rows(.id="video") %>%
  mutate(
    type="Solitary bees"
  )

all2 <- social2 %>%
  bind_rows(solitary2) #use ggplot with social 2 or solitary 2 for just individual and not all2

g0001 <- ggplot(all2) +
  geom_point(aes(`ovarybodysize`, occurrence, fill=type, shape=type), size=2) +
  geom_smooth(aes(`ovarybodysize`, occurrence), method="lm") +
  scale_x_continuous("Ovary Body size") +
  scale_y_continuous("Occurrence") +
  scale_shape_manual(values=c(21, 24)) +
  scale_color_viridis_d(begin=0.4) +
  scale_fill_viridis_d(begin=0.4) +
  facet_wrap(~category, scale="free") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank()
  )

g0001

g0001_alone <- ggplot(social2) +
  geom_point(aes(`ovarybodysize`, occurrence, fill=type, shape=type), size=2) +
  geom_smooth(aes(`ovarybodysize`, occurrence), method="lm") +
  scale_x_continuous("Ovary Body size") +
  scale_y_continuous("Occurrence") +
  scale_shape_manual(values=c(21, 24)) +
  scale_color_viridis_d(begin=0.4) +
  scale_fill_viridis_d(begin=0.4) +
  facet_wrap(~category, scale="free") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank()
  )
g0001_alone

g0001_alone2 <- ggplot(solitary2) +
  geom_point(aes(`ovarybodysize`, occurrence, fill=type, shape=type), size=2) +
  geom_smooth(aes(`ovarybodysize`, occurrence), method="lm", color="red") +
  scale_x_continuous("Ovary Body size") +
  scale_y_continuous("Occurrence") +
  scale_shape_manual(values=c(21, 24)) +
  scale_color_viridis_d(begin=0.4) +
  scale_fill_viridis_d(begin=0.4) +
  facet_wrap(~category, scale="free") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank()
  )
g0001_alone2

g0002 <- ggplot(all2) +
  geom_point(aes(`ovarybodysize`, duration, fill=type, shape=type), size=2) +
  geom_smooth(aes(`ovarybodysize`, duration), method="lm") +
  scale_x_continuous("Ovary Body size") +
  scale_y_continuous("Duration") +
  scale_shape_manual(values=c(21, 24)) +
  scale_color_viridis_d(begin=0.4) +
  scale_fill_viridis_d(begin=0.4) +
  facet_wrap(~category, scale="free") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank()
  )

g0002

g0002_alone <- ggplot(all2) +
  geom_point(aes(`ovarybodysize`, duration, fill=type, shape=type), size=2) +
  geom_smooth(aes(`ovarybodysize`, duration), method="lm") +
  scale_x_continuous("Ovary Body size") +
  scale_y_continuous("Duration") +
  scale_shape_manual(values=c(21, 24)) +
  scale_color_viridis_d(begin=0.4) +
  scale_fill_viridis_d(begin=0.4) +
  facet_wrap(~category, scale="free") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank()
  )


gtot <- ggarrange(g001, g002, nrow=2, labels=c("A", "B"))

ggsave("figure_size_behavior.pdf", gtot, width=8, height=8)
