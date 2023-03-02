# Attach data
library(readr)
library(tidyverse)
data <- read_csv("GBvideos.csv")

# Remove duplicated video_id
video_id.unique <- length(unique(data$video_id))
video_id.unique
df <- data %>% 
  dplyr::arrange(desc(views)) %>%
  group_by(video_id) %>%
  slice(1) %>%
  ungroup(video_id)

# Convert categorical and binary variable to factor
df$category_id <- as.factor(df$category_id)
df$comments_disabled <- as.factor(df$comments_disabled)
df$ratings_disabled <- as.factor(df$ratings_disabled)
df$video_error_or_removed <- as.factor(df$video_error_or_removed)

# Convert, formulate and add new date-time variables
df$trending_date <- format(as.Date(df$trending_date, format="%Y.%d.%m"), "20%y-%m-%d")
df$trending_date <- strptime(df$trending_date, format="%Y-%m-%d")
df$publish_time <- strptime(df$publish_time, format="%Y-%m-%d %H:%M:%S")
df$publish.date <- strptime(df$publish_time, format="%Y-%m-%d")
df$publish.time <- format(df$publish_time, format="%H")
df$publish.time <- as.numeric(df$publish.time)
df$trending.lag <- as.numeric(df$trending_date-df$publish.date)/(60*60*24)

# Distribution of data
library(tidyr)
library(ggplot2)
df %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales="free", labeller=labeller(key=
                                                      c("comment_count"="Number of comments",
                                                        "dislikes"="Number of dislikes",
                                                        "likes"="Number of likes",
                                                        "trending.lag"="Trending lag",
                                                        "views"="Number of views",
                                                        "publish.time"="Publish time"))) +
  geom_density(col="cornflowerblue", fill="aliceblue") +
  theme_classic() +
  theme(plot.title=element_text(size=12,face="bold"),
        plot.subtitle=element_text(size=12),
        axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=12),
        strip.text=element_text(size=12)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title="Diagram 3.1",
       subtitle="Distribution of numerical variables, before logarithmic transformation",
       x="", y="")

# Data transformation
df$views <- log(df$views+1)
df$likes <- log(df$likes+1)
df$dislikes <- log(df$dislikes+1)
df$comment_count <- log(df$comment_count+1)
df$trending.lag <- log(df$trending.lag+1)
df %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales="free", labeller=labeller(key=
                                                      c("comment_count"="Number of comments",
                                                        "dislikes"="Number of dislikes",
                                                        "likes"="Number of likes",
                                                        "trending.lag"="Trending lag",
                                                        "views"="Number of views",
                                                        "publish.time"="Publish time"))) +
  geom_density(col="cornflowerblue", fill="aliceblue") +
  theme_classic() +
  theme(plot.title=element_text(size=12,face="bold"),
        plot.subtitle=element_text(size=12),
        axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=12),
        strip.text=element_text(size=12)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title="Diagram 3.2",
       subtitle="Distribution of numerical variables, after logarithmic transformation",
       x="", y="")

# Explore correlations between numerical variables
library(ggcorrplot)
library(GGally)
ggpairs(df, 
        columns=c("views", "likes", "dislikes", "comment_count", 
                  "trending.lag", "publish.time"),
        columnLabels=c("Views", "Likes", "Dislikes", "Comments", 
                       "Trending lag", "Publish time"),
        lower=list(continuous=wrap(ggally_points, size=0.1, 
                                   color="cornflowerblue")),
        diag=list(continuous=wrap("densityDiag", 
                                  col="cornflowerblue", fill="aliceblue")),
        upper=list(continuous=wrap("cor", size=4.4, color="black")),
        corSize=12) +
  labs(title="Diagram 4",
       subtitle="Scatterplot matrix of all numerical variables") +
  theme_classic() +
  theme(panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        panel.background=element_blank(), 
        axis.line=element_line(colour="black")) +
  theme(plot.title=element_text(size=12,face="bold"),
        plot.subtitle=element_text(size=12),
        axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=12),
        strip.text.x=element_text(size=12),
        strip.text.y=element_text(size=12))

# Splitting data
library(caret)
set.seed(3456)
sample <- createDataPartition(df$views, p=0.75, list=FALSE)
df.train <- df[sample, ]
df.test <- df[-sample, ]

# Regression modelling
model1 <- lm(views~likes+dislikes+comment_count+
               trending.lag+publish.time+
               video_error_or_removed+
               comments_disabled+ratings_disabled+
               category_id, data=df.train)
summary(model1)

model2 <- lm(views~likes+dislikes+comment_count+
               trending.lag+publish.time+
               (likes*comment_count)+(dislikes*comment_count)+
               (likes*dislikes)+
               video_error_or_removed+
               comments_disabled+ratings_disabled+
               category_id, data=df.train)
summary(model2)

model3 <- lm(views~likes+dislikes+comment_count+
               trending.lag+publish.time+
               (likes*comment_count)+(dislikes*comment_count)+
               (likes*dislikes)+
               comments_disabled+ratings_disabled, data=df.train)
summary(model3)

# Compare performance
library(performance)
compare_performance(model1, model2, model3, rank=TRUE)

# Regression diagnostics
library(ggplot2)
residuals.fitted <- ggplot(data=df.train, 
                           aes(x=model2$fitted, y=model2$residuals)) +
  theme_classic() +
  geom_point(col="cornflowerblue", size=.8, shape=16) + 
  geom_abline(intercept=0, slope=0, 
              colour="black", linetype="dashed", size=.6) +
  geom_smooth(method="loess", formula=y~x, span=2, 
              se=FALSE, colour="red1", size=.6) +
  labs(title="Diagram 5.2.1",
       subtitle="Residuals versus fitted values",
       x="Fitted values", 
       y="Residuals") +
  theme(plot.title=element_text(size=12,face="bold"),
        plot.subtitle=element_text(size=12),
        axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=12))

normal.qq <- ggplot(model2, aes(qqnorm(.stdresid)[[1]], .stdresid)) +
  theme_classic() +
  geom_point(col="cornflowerblue", size=.8, shape=16) +
  geom_abline(colour="black", linetype="dashed", size=.6) +
  labs(title="Diagram 5.2.2",
       subtitle="Normal quantile-quantile plot",
       x="Theoretical quantiles", 
       y ="Standardised residuals") +
  theme(plot.title=element_text(size=12,face="bold"),
        plot.subtitle=element_text(size=12),
        axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=12))

library(cowplot)
plot_grid(residuals.fitted, normal.qq,
          ncol=2, nrow=1)
library(dplyr)
shapiro.test(df.train$views)

library(broom)
aug.model2 <- augment(model2, data=df.train)
scale.location <- ggplot(aug.model2, aes(x=.fitted, y=sqrt(abs(.std.resid)))) +
  theme_classic() +
  geom_point(col="cornflowerblue", size=.8, shape=16) +
  labs(title="Diagram 5.2.3",
       subtitle="Scale-location plot",
       x="Fitted values", 
       y="Square root of standardised residuals") +
  geom_smooth(method="loess", formula=y~x, span=2, 
              se=FALSE, colour="red1", size=.6) +
  theme(plot.title=element_text(size=12,face="bold"),
        plot.subtitle=element_text(size=12),
        axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=12))

residuals.leverage <- ggplot(aug.model2, aes(x=.hat, y=.std.resid)) +
  theme_classic() +
  geom_point(col="cornflowerblue", size=.8, shape=16) +
  geom_abline(intercept=0, slope=0, 
              colour="black", linetype="dashed", size=.6) +
  geom_smooth(method="loess", formula=y~x, span=2, 
              se=FALSE, colour="red1", size=.6) +
  labs(title="Diagram 5.2.4",
       subtitle="Standardised residuals versus leverage",
       x="Leverage", 
       y ="Standardised residuals") +
  theme(plot.title=element_text(size=12,face="bold"),
        plot.subtitle=element_text(size=12),
        axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=12)) 

library(cowplot)
plot_grid(scale.location, residuals.leverage, 
          ncol=2, nrow=1)

bptest(model2)
dwtest(model2)

# Regression table
library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(model1, model2, model3, auto.label=FALSE, show.ci=FALSE,
          dv.labels = c("Model 1", "Model 2", "Model 3"))

# Prediction
pred <- model2 %>% predict(df.test)
RMSE(pred, df.test$views)
R2(pred, df.test$views)
0.6212399/mean(df.test$views)
