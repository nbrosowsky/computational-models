---
title: "Jones_and_Mewhort_2007"
author: "Nicholaus Brosowsky"
date: "4/12/2019"
output:
  html_document:
    keep_md: yes
---



## Jones & Mewhort Context table


```r
## large file (~750mb)
load(url('https://www.dropbox.com/s/ft4r9sxyajoo597/BeagleHalfMil.RData?dl=1'))

get_similar_words<-function(in_word,memory,termlist,number){
  word_index<-which(termlist==in_word)
  correlations<-cosine(memory[word_index,],t(memory))
  names(correlations)<-termlist
  sorted_cors<-sort(correlations,decreasing=T)
  return(sorted_cors[1:number])
}
```


```r
list_of_words<-c("computer","hospital","heart","cognitive","beer")
all_words<-list()
tcnt<-0
for(i in list_of_words){
  tcnt<-tcnt+1
  testtable<-get_similar_words(i,word_mem,word_names,20)
  df<-data.frame(Word=names(testtable),Cosines=as.numeric(testtable))
  names(df)[1]<-i
  names(df)[2]<-paste(c("Sim",tcnt),collapse="")
  all_words<-c(all_words,df)
}


kable(as.data.frame(all_words))
```



computer             Sim1  hospital          Sim2  heart               Sim3  cognitive             Sim4  beer            Sim5
-------------  ----------  ----------  ----------  ------------  ----------  --------------  ----------  --------  ----------
computer        1.0000000  hospital     1.0000000  heart          1.0000000  cognitive        1.0000000  beer       1.0000000
data            0.6957629  medical      0.5265011  artery         0.5533114  intellectual     0.3615345  wine       0.3936900
computers       0.6006278  doctor       0.5119070  blood          0.5504594  development      0.3547062  drank      0.3263409
program         0.5668555  patients     0.4910803  arteries       0.5468190  learning         0.3496427  whiskey    0.2554123
processing      0.5319692  home         0.4828140  vessels        0.5152069  personality      0.3405463  bottles    0.2545389
instructions    0.5119955  patient      0.4671755  veins          0.5001740  psychologists    0.3344057  glass      0.2097927
processed       0.4984589  care         0.4415309  pumps          0.4633628  behavior         0.3320757  rum        0.2095522
memory          0.4940510  nursing      0.4229302  circulation    0.4468405  stages           0.3305449  bottle     0.2038008
storage         0.4871722  nurse        0.4196902  lungs          0.4266910  psychological    0.3220300  hairs      0.2007455
information     0.4870815  nurses       0.4176436  body           0.4263028  childs           0.3201006  milk       0.1954855
input           0.4807896  doctors      0.4171020  capillaries    0.4224739  research         0.3110344  alcohol    0.1944093
keyboard        0.4721857  hospitals    0.4082151  clot           0.4151822  social           0.2870772  cup        0.1939493
software        0.4415241  told         0.4043110  hemoglobin     0.4048366  approach         0.2788162  ate        0.1830159
records         0.4221911  leave        0.3960645  liver          0.3999857  historical       0.2776711  drink      0.1771401
electronic      0.4146075  knew         0.3905388  clotting       0.3955872  stage            0.2770444  tea        0.1750268
programmer      0.4068391  job          0.3839366  vessel         0.3896259  physical         0.2761436  root       0.1726693
operation       0.4026819  sick         0.3834835  cells          0.3884692  cultural         0.2756693  bring      0.1686045
record          0.4007232  thought      0.3813073  ventricles     0.3875274  terms            0.2726191  big        0.1620238
type            0.3978228  family       0.3809905  pumping        0.3816258  emotional        0.2714970  drinks     0.1619587
machine         0.3890179  mother       0.3806714  muscles        0.3772036  theory           0.2707568  perfume    0.1613206

```r
#t_all<-as.data.frame(all_words)
```


