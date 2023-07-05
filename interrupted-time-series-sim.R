library(tidyverse)

month <- rep(1:12, times = 12)
year <- rep(2011:2022, each = 12, times = 1)
time <- bind_cols(year = year,month = month)



x <- rpois(12, 1.5*(2))
y<-0

data <- data.frame(x,y)

for(i in 2:6){
  
  x <- rpois(12, 1.5*(i))
  y <- rep(0, times = 12)
  
  data <- bind_rows(data, data.frame(x,y))
  
}

y <- rpois(12, 100*2^2)
x <- rpois(12, 1.5*(i))- (y/3000)


data1 <- data.frame(x,y)


for(i in 7:11){
  
  y <- rpois(12, 100*i^2)
  
  x <- rpois(12, 1.5*(i)) - (y/3000)
  
  data1 <- bind_rows(data1, data.frame(x,y))
  
}


df <- bind_rows(data, data1)

df <- bind_cols(time,df)

df <- df %>% 
  mutate(intervention = ifelse(y > 0, 1, 0))

df$time <- 1:nrow(df)

df$post.intervention.time <- c(rep(0,72),1:72)

df

library(nlme)

formula <- x ~ time + intervention + post.intervention.time + y

model <- gls(formula, method = "ML", data = df)

fx = function(pval,qval){
  summary(
    gls(formula, data = df, 
        correlation= corARMA(p=pval,q=qval, form = ~ Time),
        method="ML"))$AIC
  }

p = summary(gls(formula, data = df,method="ML"))$AIC
message(str_c ("AIC Uncorrelated model = ",p))

autocorrel = expand.grid(pval = 0:2, qval = 0:2)

for(i in 2:nrow(autocorrel)){
  p[i] = try(summary(gls(formula, data = df, 
                         correlation= corARMA(p=autocorrel$pval[i],
                                              q=autocorrel$qval[i], 
                                              form = ~ time),
                         method="ML"))$AIC)
  }

autocorrel<- autocorrel %>%
  mutate(AIC = as.numeric(p)) %>%
  arrange(AIC)
}


autocorrel

model <- gls(formula, method = "ML", data = df, 
             correlation= corARMA(p=2,
                                  q=2, form = ~ time))

summary(model)

library(marginaleffects)

a <- predictions(model)

df<-df %>% mutate(
  model.predictions = a$estimate,
  model.se = a$std.error
)


ggplot(df,aes(time,x))+
  geom_ribbon(aes(ymin = model.predictions - (1.96*model.se), 
                  ymax = model.predictions + (1.96*model.se)), 
              fill = "lightblue")+
  geom_line(aes(time,
                model.predictions),color="black",lty=1)+
  geom_point(alpha=0.3) +
  geom_vline(xintercept = 72.5, lty =2) + 
  labs(
    y = "Opioid Overdose Deaths",
    x = "Time in Months",
    title = "Simulated Interrupted Timeseries"
  )

performance::performance(model)
performance::check_model(model)
























