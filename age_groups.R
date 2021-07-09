data <- read.table("Weekly_counts_of_death_by_age_data.csv",sep=",",header=TRUE)
pop_perc <- read.table("age_group_perc.csv",sep=",",header=TRUE)
labels <- c("Under 25 years","25-44 years","45-64 years",
          "65-74 years","75-84 years","85 years and older")

us_init_pre_pop <- 327840942
us_init_post_pop <- 329301587

########################################################

pre_deaths <- matrix(NA, 52, 6)
colnames(pre_deaths) <- labels
for(i in 1:6)
  pre_deaths[,i] <- data[
      ((data$Year==2019 & data$Week>=13)  | (data$Year==2020 & data$Week<13)) &
      data$ï..Age.Group==labels[i],7]

post_deaths <- matrix(NA, 52, 6)
colnames(post_deaths) <- labels
for(i in 1:6)
  post_deaths[,i] <- data[
    ((data$Year==2020 & data$Week>=13)  | (data$Year==2021 & data$Week<12)) &
    data$ï..Age.Group==labels[i],7]

########################################################

par(mfrow=c(1,2))

plot(c(),c(),xlim=c(1,52),ylim=c(0,max(post_deaths)),
  main="March 2019 to March 2020",xlab="Week", ylab="Deaths")
points(pre_deaths[,1],col="red",type="l")
points(pre_deaths[,2],col="orange",type="l")
points(pre_deaths[,3],col="yellow",type="l")
points(pre_deaths[,4],col="green",type="l")
points(pre_deaths[,5],col="blue",type="l")
points(pre_deaths[,6],col="violet",type="l")

plot(c(),c(),xlim=c(1,52),ylim=c(0,max(post_deaths)),
  main="March 2020 to March 2021",xlab="Week", ylab="Deaths")
points(post_deaths[,1],col="red",type="l")
points(post_deaths[,2],col="orange",type="l")
points(post_deaths[,3],col="yellow",type="l")
points(post_deaths[,4],col="green",type="l")
points(post_deaths[,5],col="blue",type="l")
points(post_deaths[,6],col="violet",type="l")

########################################################

pre_pop <- matrix(NA, 52, 6)
colnames(pre_pop) <- labels
pre_pop[1,] <- pop_perc[,2] * us_init_pre_pop
for(i in 2:52)
  pre_pop[i,] <- pre_pop[i-1,] - pre_deaths[i-1,]

post_pop <- matrix(NA, 52, 6)
colnames(post_pop) <- labels
post_pop[1,] <- pop_perc[,2] * us_init_post_pop
for(i in 2:52)
  post_pop[i,] <- post_pop[i-1,] - post_deaths[i-1,]

########################################################

par(mfrow=c(1,2))

plot(c(),c(),xlim=c(1,52),ylim=c(0,max(post_pop)),
  main="March 2019 to March 2020",xlab="Week", ylab="Population")
points(pre_pop[,1],col="red",type="l")
points(pre_pop[,2],col="orange",type="l")
points(pre_pop[,3],col="yellow",type="l")
points(pre_pop[,4],col="green",type="l")
points(pre_pop[,5],col="blue",type="l")
points(pre_pop[,6],col="violet",type="l")

plot(c(),c(),xlim=c(1,52),ylim=c(0,max(post_pop)),
  main="March 2020 to March 2021",xlab="Week", ylab="Population")
points(post_pop[,1],col="red",type="l")
points(post_pop[,2],col="orange",type="l")
points(post_pop[,3],col="yellow",type="l")
points(post_pop[,4],col="green",type="l")
points(post_pop[,5],col="blue",type="l")
points(post_pop[,6],col="violet",type="l")

########################################################

pre_deathrate <- pre_deaths / pre_pop
post_deathrate <- post_deaths / post_pop

ratio <- post_deathrate / pre_deathrate
var_ratio <- ratio^2 / post_deaths
upper_ratio <- ratio + 1.96 * sqrt(var_ratio)
lower_ratio <- ratio - 1.96 * sqrt(var_ratio)

diff <- post_deathrate - pre_deathrate
var_diff <- post_deaths / post_pop^2
upper_diff <- diff + 1.96 * sqrt(var_diff)
lower_diff <- diff - 1.96 * sqrt(var_diff)

########################################################

par(mfrow=c(1,2))

plot(c(),c(),main="March 2019 to March 2020",xlim=c(1,52),
  ylim=c(0,max(post_deathrate)),xlab="Week",ylab="Weekly Deathrate")
points(pre_deathrate[,1],col="red",type="l")
points(pre_deathrate[,2],col="orange",type="l")
points(pre_deathrate[,3],col="yellow",type="l")
points(pre_deathrate[,4],col="green",type="l")
points(pre_deathrate[,5],col="blue",type="l")
points(pre_deathrate[,6],col="violet",type="l")

plot(c(),c(),main="March 2020 to March 2021",xlim=c(1,52),
  ylim=c(0,max(post_deathrate)),xlab="Week",ylab="Weekly Deathrate")
points(post_deathrate[,1],col="red",type="l")
points(post_deathrate[,2],col="orange",type="l")
points(post_deathrate[,3],col="yellow",type="l")
points(post_deathrate[,4],col="green",type="l")
points(post_deathrate[,5],col="blue",type="l")
points(post_deathrate[,6],col="violet",type="l")

########################################################

par(mfrow=c(3,2))
par(mar=c(4,4,2,1))

for(i in 1:6)
{
  plot(rep(1,times=52),ylim=c(0,2),main=labels[i],
    xlab="Week",ylab="Ratio",type="l")
  points(upper_ratio[,i],type="l",col="pink")
  points(lower_ratio[,i],type="l",col="pink")
  points(ratio[,i],type="l",col="red")
}

########################################################

par(mfrow=c(3,2))
par(mar=c(4,4,2,1))

for(i in 1:6)
{
  plot(rep(0,times=52),ylim=range(diff),main=labels[i],
    xlab="Week",ylab="Difference",type="l")
  points(upper_diff[,i],type="l",col="pink")
  points(lower_diff[,i],type="l",col="pink")
  points(diff[,i],type="l",col="red")

}

########################################################

cum_ratio <- matrix(NA, 52, 6)
cum_ratio[1,] <- ratio[1,]

cum_upper_ratio <- matrix(NA, 52, 6)
cum_upper_ratio[1,] <- upper_ratio[1,]

cum_lower_ratio <- matrix(NA, 52, 6)
cum_lower_ratio[1,] <- lower_ratio[1,]

for(i in 2:52)
{
  cum_ratio[i,] <- ratio[i,] + cum_ratio[i-1,]
  cum_upper_ratio[i,] <- upper_ratio[i,] + cum_upper_ratio[i-1,]
  cum_lower_ratio[i,] <- lower_ratio[i,] + cum_lower_ratio[i-1,]
}

par(mfrow=c(3,2))
par(mar=c(4,4,2,1))

for(i in 1:6)
{
  plot(c(),xlim=c(1,52),ylim=c(0,max(cum_ratio)),main=labels[i],
    xlab="Week",ylab="Cumulative Relative Hazard",type="l",col="red")
  points(cum_upper_ratio[,i],type="l",col="pink")
  points(cum_lower_ratio[,i],type="l",col="pink")
  points(cum_ratio[,i],type="l",col="red")
}

########################################################

cum_diff <- matrix(NA, 52, 6)
cum_diff[1,] <- diff[1,]

cum_upper_diff <- matrix(NA, 52, 6)
cum_upper_diff[1,] <- upper_diff[1,]

cum_lower_diff <- matrix(NA, 52, 6)
cum_lower_diff[1,] <- lower_diff[1,]

for(i in 2:52)
{
  cum_diff[i,] <- diff[i,] + cum_diff[i-1,]
  cum_upper_diff[i,] <- upper_diff[i,] + cum_upper_diff[i-1,]
  cum_lower_diff[i,] <- lower_diff[i,] + cum_lower_diff[i-1,]
}

par(mfrow=c(3,2))
par(mar=c(4,4,2,1))

for(i in 1:6)
{
  plot(c(),xlim=c(1,52),ylim=c(0,max(cum_diff)),main=labels[i],
    xlab="Week",ylab="Cumulative Excess Hazard",type="l",col="red")
  points(cum_upper_diff[,i],type="l",col="pink")
  points(cum_lower_diff[,i],type="l",col="pink")
  points(cum_diff[,i],type="l",col="red")
}
