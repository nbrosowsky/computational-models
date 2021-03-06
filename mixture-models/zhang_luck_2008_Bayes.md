---
title: "zhang_luck_2008"
author: "Nicholaus Brosowsky"
date: "4/8/2019"
output:
  html_document:
    keep_md: yes
---


## Re-analysis of Zhang and Luck, 2008 using Bayesian estimation



## Bayesian mixture model


```r
fit_mixture_model <- function(data, start_values) {
  
  # placeholder for monte-carlo markov chain values
  chain <- matrix(0,5000,2) 
  # put starting  values in first
  chain[1,] <- start_values
  
  # number of "burn-in" values 
  # aka. throw away the first N values
  burnin <- 500

  # proposed sd values
  propsd <- start_values*.05
  
  # upper and lower bounds
  # c(prop in memory, degree error)
  lb <- c(0,4)
  ub <- c(1,360)
  
  # number of iterations
  N_chain <- dim(chain)[1]
  
  for (i in c(2:N_chain)) { 
    cur <- chain[i-1,]
    
    # Run loop until proposed values fall within upper and lower bounds
    doitagain <- TRUE
    while (doitagain) {
      # proposed values + random noise
      propl <- cur + rnorm(2,0,propsd) 
      doitagain <- any(propl<lb) || any(propl>ub)
    }
    
    # The use of logarithms guards against numerical issue that arise when very small (or large) numbers are multiplied
    # one consequence: multiplication turns into addition
    # likelihood of proposed values
    lpropval <- logmixturepdf(data, propl[1], propl[2]) + logprior(propl[1], propl[2])
    
    # likelihood of current values
    lcurval  <- logmixturepdf(data,cur[1],cur[2]) + logprior(cur[1],cur[2])
    
    # likelihood ratio
    # Likewise, the ratio of the two values for the target distribution is computed by subtraction rather than division
    # Because that ratio must be in the range 0-1 to permit comparison against a random uniform number, 
    # the difference operation on logarithms is exponentiated in the same line in order to return to the original untransformed space. 
    # (The exponentiated difference between two logs is the same as the ratio between the original numbers).
    llratio  <- exp(lpropval-lcurval)   
    
    # if the value for the proposal is greater than for the current sample, then the ratio of values is necessarily greater than one
    # thus the random uniform number can never be greater than the ratio and thus the proposal will always be accepted. 
    # When the ratio is less than 1, 
    if (runif(1) < llratio) { 
      chain[i,] <- propl
    } else {
      chain[i,] <- cur
    }
    
  } 
  
  # get final parameters
  finparm <- apply(chain[-c(1:burnin),],2,mean) 
  
  #print(finparm)
  
  td <- c(-180:180)
  
  pred <- (1-finparm[1]) * dvonmises(mkcirc(td),mkcirc(0),sd2k(finparm[2])) + finparm[1]*rep(1,length(data))/(2*pi)
  
  posterior<-chain[-c(1:burnin),]
  
  return(list(preds=pred,posteriors=posterior))
} 
```



## Load data


```r
data <- c()
for (n in 1:8){
  temp <- readMat(con = paste0("data/E2_subject_", n, ".mat"))
  temp <- cbind(unlist(temp$data[[1]][1,]),
                   unlist(temp$data[[3]][1,]),
                   rep(n, 500))
  data <- rbind(data, temp)
}

data <- data.frame(
  errors  = data[,1],
  setsize = data[,2],
  subject = data[,3]
)

data$setsize <- as.factor(data$setsize)
data$subject <- as.factor(data$subject)

raw_data <- data

data <- raw_data %>%
  group_by(subject, setsize) %>%
  mutate(errors = errors*(180/pi)) %>%
  mutate(error_discrete = cut(errors,breaks=seq(from=-180,to=180,by=20),labels=FALSE))

summary <- data %>%
  group_by(subject, setsize, error_discrete) %>%
  summarise (n = n()) %>%
  mutate(errors = n / sum(n)) %>%
  select(-n)
```

## Fit mixture model for Subject #1


```r
sub_errors <- data %>% 
  filter(subject == 1,
         setsize%in%c(3,6))
sub_summary  <- summary %>%
  filter(subject == 1,
         setsize%in%c(3,6))

#get predictions
start_values     <- c(0.5,20)
preds<-posteriors<- vector("list",2)
ssz              <- c(3, 6)

for (s in 1:length(ssz)) {
  cp<-fit_mixture_model(subset(sub_errors,setsize==ssz[s])$errors,start_values)
  preds[[s]] <- cp$preds
  posteriors[[s]] <- cp$posterior
  preds[[s]] <- preds[[s]]/sum(preds[[s]])  #normalize
}

sub_summary$setsize<-factor(sub_summary$setsize)
sub_summary$error_discrete<-factor(sub_summary$error_discrete)
levels(sub_summary$error_discrete)<-seq(from=-180, to=180, by=20)
sub_summary$error_discrete<-as.numeric(as.character(sub_summary$error_discrete))


# estimated distributions
set6 <- data.frame(x6 = seq(from=-180,to=180,length.out=length(preds[[2]])),
                   y6 = preds[[2]]*(length(preds[[2]])/length(sub_summary[sub_summary$setsize == 6,]$error_discrete))
                   )

set3 <- data.frame(x3 = seq(from=-180,to=180,length.out=length(preds[[1]])),
                   y3 = preds[[1]]*(length(preds[[1]])/length(sub_summary[sub_summary$setsize == 3,]$error_discrete))
                   )


ggplot(sub_summary[sub_summary$setsize == 6,], aes(x = error_discrete, y = errors)) + 
  geom_line(aes(x = x3, y = y3), data = set3, lwd= 1) +
  geom_line(aes(x = x6, y = y6), data = set6, lwd= 1, linetype = "dashed") +
  geom_point(data = sub_summary[sub_summary$setsize == 6,], size = 4, pch=21, cex=1.5, col="black",bg="white") +
  geom_point(data = sub_summary[sub_summary$setsize == 3,], size = 4, pch=21, cex=1.5, col="black",bg="grey") +
  theme_bw() +
  labs(title = "Zhang and Luck, 2008; Exp 2; Subject 1", x = "Difference in color value (degrees)", y = "Proportion of responses" ) +
  scale_x_continuous(breaks=seq(-180, 180, 20))  # Ticks from 0-10, every .25
```

![](zhang_luck_2008_Bayes_files/figure-html/unnamed-chunk-3-1.png)<!-- -->



## Fit mixture model for Subject #8


```r
sub_errors <- data %>% 
  filter(subject == 8,
         setsize%in%c(3,6))
sub_summary  <- summary %>%
  filter(subject == 8,
         setsize%in%c(3,6))

#get predictions
start_values     <- c(0.5,20)
preds<-posteriors<- vector("list",2)
ssz              <- c(3, 6)

for (s in 1:length(ssz)) {
  cp<-fit_mixture_model(subset(sub_errors,setsize==ssz[s])$errors,start_values)
  preds[[s]] <- cp$preds
  posteriors[[s]] <- cp$posterior
  preds[[s]] <- preds[[s]]/sum(preds[[s]])  #normalize
}

sub_summary$setsize<-factor(sub_summary$setsize)
sub_summary$error_discrete<-factor(sub_summary$error_discrete)
levels(sub_summary$error_discrete)<-seq(from=-180, to=180, by=20)
sub_summary$error_discrete<-as.numeric(as.character(sub_summary$error_discrete))


# estimated distributions
set6 <- data.frame(x6 = seq(from=-180,to=180,length.out=length(preds[[2]])),
                   y6 = preds[[2]]*(length(preds[[2]])/length(sub_summary[sub_summary$setsize == 6,]$error_discrete))
                   )

set3 <- data.frame(x3 = seq(from=-180,to=180,length.out=length(preds[[1]])),
                   y3 = preds[[1]]*(length(preds[[1]])/length(sub_summary[sub_summary$setsize == 3,]$error_discrete))
                   )


ggplot(sub_summary[sub_summary$setsize == 6,], aes(x = error_discrete, y = errors)) + 
  geom_line(aes(x = x3, y = y3), data = set3, lwd= 1) +
  geom_line(aes(x = x6, y = y6), data = set6, lwd= 1, linetype = "dashed") +
  geom_point(data = sub_summary[sub_summary$setsize == 6,], size = 4, pch=21, cex=1.5, col="black",bg="white") +
  geom_point(data = sub_summary[sub_summary$setsize == 3,], size = 4, pch=21, cex=1.5, col="black",bg="grey") +
  theme_bw() +
  labs(title = "Zhang and Luck, 2008; Exp 2; Subject 8", x = "Difference in color value (degrees)", y = "Proportion of responses" ) +
  scale_x_continuous(breaks=seq(-180, 180, 20))  # Ticks from 0-10, every .25
```

![](zhang_luck_2008_Bayes_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Fit mixture model for all subs


```r
#get predictions
start_values     <- c(0.5, 20)
set_sizes        <- c(1, 2, 3, 6)

# placeholder
mm_data <- c()


for (s in set_sizes) {
  for (n in 1:2) {
    d <- data %>% filter(subject == n, setsize == s)
    m <- fit_mixture_model(d$errors, start_values)
    
    mm_data <- rbind(mm_data, c(n, s, mean(m$posteriors[,1]), mean(m$posteriors[,2]) ))
                     
  }
}
```


## Plot Probability and SD


```r
mm_data <- data.frame(
  subject = mm_data[,1],
  setsize = mm_data[,2],
  prob_g  = mm_data[,3],
  sd      = mm_data[,4]
)

mm_summary <- mm_data %>%
  group_by(setsize) %>%
  summarize(prob_memory = 1-(mean(prob_g)),
            mean_sd = mean(sd)
  )

mm_summary
```

```
## # A tibble: 4 x 3
##   setsize prob_memory mean_sd
##     <dbl>       <dbl>   <dbl>
## 1       1       0.987    13.7
## 2       2       0.870    21.9
## 3       3       0.780    25.4
## 4       6       0.362    29.1
```

```r
ggplot(mm_summary, aes(x = setsize, y = prob_memory)) + 
  geom_point(size = 3.5)+
  theme_bw() +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(breaks = c(1,2,3,6)) +
  labs(x = "Set Size", y = "Probability in Memory", title = "Zhang and Luck, 2008; Exp 2")
```

![](zhang_luck_2008_Bayes_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
ggplot(mm_summary, aes(x = setsize, y = mean_sd)) + 
  geom_point(size = 3.5)+
  theme_bw() +
  #scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(breaks = c(1,2,3,6)) +
  labs(x = "Set Size", y = "Standard Deviation", title = "Zhang and Luck, 2008; Exp 2")
```

![](zhang_luck_2008_Bayes_files/figure-html/unnamed-chunk-6-2.png)<!-- -->
