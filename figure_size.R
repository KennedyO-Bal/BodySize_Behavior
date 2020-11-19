library(readxl)
library(lme4)
library(zoo)
library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=12, base_family = "Times"))
library(egg)

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

solitary_raw <- read_excel_allsheets("Behavioral categories_solitary_Body size.xlsx")
social_raw <- read_excel_allsheets("Behavioral categories_Social_Body size.xlsx") 
social_raw

social <- social_raw[sapply(social_raw, ncol)==7] %>%
  bind_rows(.id="video") %>%
  mutate(
    type="Social bees"
  )

solitary <- solitary_raw[sapply(solitary_raw, ncol)==7] %>%
  bind_rows(.id="video") %>%
  mutate(
    type="Solitary bees"
  )

all <- social %>%
  bind_rows(solitary)

t.test(
  filter(all, category=="Aggressive", type=="Social bees")$ovarybodysize,
  filter(all, category=="Aggressive", type=="Solitary bees")$ovarybodysize
)

#t test for each group separately
t.test(
  filter(social, category=="Aggressive", type=="Social bees")$ovarybodysize
  )

G1 <- ggplot(filter(all, category=="Aggressive")) +
  geom_boxplot(aes(type, bodysize, fill=type)) +
  scale_y_continuous("Body size") +
  scale_fill_viridis_d(begin=0.4) +
  theme(
    axis.title.x = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )

G1

G_alone <-  ggplot(filter(social, category=="Aggressive")) +
  geom_boxplot(aes(type, bodysize, fill=type)) +
  scale_y_continuous("Body size") +
  scale_fill_viridis_d(begin=0.4) +
  theme(
    axis.title.x = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )

G_alone

G_alone2 <-  ggplot(filter(solitary, category=="Aggressive")) +
  geom_boxplot(aes(type, bodysize, fill=type)) +
  scale_y_continuous("Body size") +
  scale_fill_viridis_d(begin=0.4) +
  theme(
    axis.title.x = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  
  )

G_alone2

t.test(
  filter(all, category=="Aggressive", type=="Social")$ovarybodysize,
  filter(all, category=="Aggressive", type=="Solitary")$ovarybodysize
)

t.test(filter(all, category=="Aggressive", type=="Social")$ovarybodysize)
  
G2 <- ggplot(filter(all, category=="Aggressive")) +
  geom_boxplot(aes(type, ovarysize, fill=type)) +
  scale_y_continuous("Ovary size") +
  scale_fill_viridis_d(begin=0.4) +
  theme(
    axis.title.x = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )

G2

G2_alone<-ggplot(filter(social, category=="Aggressive")) +
  geom_point(aes(bodysize, ovarysize, shape=type), size=3, col='blue') +
  geom_smooth(aes(bodysize, ovarysize, col=type, fill=type) , method="lm", color='blue') +
  scale_x_continuous("Body size") +
  scale_y_continuous("Ovary size") +
  scale_shape_manual(values=c(21, 24)) +
  scale_fill_viridis_d(begin=0.4) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank()
  )

G2_alone

G2_alone2<-ggplot(filter(solitary, category=="Aggressive")) +
  geom_point(aes(bodysize, ovarysize, shape=type), size=3, color="red") +
  geom_smooth(aes(bodysize, ovarysize, col=type, fill=type) , method="lm", color='red') +
  scale_x_continuous("Body size") +
  scale_y_continuous("Ovary size") +
  scale_shape_manual(values=c(21, 24)) +
  scale_fill_viridis_d(begin=0.4) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank()
  )

G2_alone2

gtot <- ggarrange(G_alone, G_alone2,G2_alone, G2_alone2, nrow=2, labels=c("A", "B", "C","D"))

G3 <- ggplot(filter(all, category=="Aggressive")) +
  geom_point(aes(bodysize, ovarysize, fill=type, shape=type), size=3) +
  scale_x_continuous("Body size") +
  scale_y_continuous("Ovary size") +
  scale_shape_manual(values=c(21, 24)) +
  scale_fill_viridis_d(begin=0.4) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank()
  )

G3

G3_alone <- ggplot(filter(social, category=="Aggressive")) +
  geom_point(aes(bodysize, ovarysize, fill=type, shape=type), size=3) +
  scale_x_continuous("Body size") +
  scale_y_continuous("Ovary size") +
  scale_shape_manual(values=c(21, 24)) +
  scale_fill_viridis_d(begin=0.4) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank()
  )
G3_alone

gtot <- ggarrange(g1, g2, g3, nrow=1,
          labels=c("A", "B", "C"))

ggsave("figure_size.pdf", gtot, width=12, height=4)

#EXRA


all_each <- all %>%
  group_by(video, subject, type) %>%
  summarize(
    bodysize=unique(bodysize),
    ovarysize=unique(ovarysize),
    `ovarybodysize`=unique(`ovarybodysize`)
  )


G4<-ggplot(all_each,aes(x= video, y=ovarybodysize, fill=type)) + 
  geom_boxplot() +
  scale_x_discrete("category") +
  theme(
    axis.title.x = element_blank(),
    panel.grid = element_blank(),
    legend.title =  element_blank()
  )

G4

t.test(
  filter(all_each, type=="Social bees")$`ovarybodysize`,  #ideally we shouldn't compare the two groups since they are different species. We though compare, e.g workers and queens, males vs female workers et.c within the same species
  filter(all_each, type=="Solitary bees")$`ovarybodysize`
)

library("ggpubr")

#Correlation test

Corr1<- ggscatter(social, x = "bodysize", y = "ovarysize", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Body Size", ylab = "Ovary Size")
Corr1

Corr2<- ggscatter(solitary, x = "bodysize", y = "ovarysize", 
                  add = "reg.line", conf.int = TRUE, 
                  cor.coef = TRUE, cor.method = "pearson",
                  xlab = "Body Size", ylab = "Ovary Size")
Corr2

reg<- lm(bodysize~ovarysize, data = all)
with(all, plot(bodysize, ovarysize))
abline(reg)

