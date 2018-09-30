product <- 1

for(i in 1:23){
  product <- product*(365-i+1)
}

p <- 1-(product/365^23)

print(10*(1-p)/p)

##########
rm(list=ls(all=T))

exp <- 100000
toss <- 20

a_count <- matrix(rep(0,exp*toss),exp,toss)
b_count <- matrix(rep(0,exp*toss),exp,toss)

for(count in 1:exp){
  for(coin in 1:toss){
    a <- runif(1,0,1)
    if(a >= 0.5){
      a_count[count,coin] <- 1
    }else{
      b_count[count,coin] <- 1
    }
  }
}

acumsum <- t(apply(a_count,1,cumsum))
bcumsum <- t(apply(b_count,1,cumsum))

diffcumsum <- acumsum - bcumsum

final_count_1 <- ifelse(diffcumsum >= 0,1,0)

first_whole <- apply(final_count_1,1,sum)

count_first_whole <- ifelse(first_whole == 20,1,0)

sum(count_first_whole)/exp  ## 17.5%

count_first_whole_b <- ifelse(first_whole == 0,1,0)

sum(count_first_whole_b)/exp  ## 8.8%

final_count_mark <- ifelse(final_count_1 == 1, "A","B")

###############

rm(list=ls(all=T))

exp <- 100000
toss <- 20

a_count <- matrix(rep(0,exp*toss),exp,toss)
b_count <- matrix(rep(0,exp*toss),exp,toss)
winner <- matrix(rep(0,exp*toss),exp,toss)

for(count in 1:exp){
  for(coin in 1:toss){
    a <- runif(1,0,1)
    if(coin == 1){
      if(a >= 0.5){
        a_count[count,coin] <- 1
      }else{
        b_count[count,coin] <-1
      }
    }else{
      if(a >= 0.5){
        a_count[count,coin] <- 1 + a_count[count,coin-1]
        b_count[count,coin] <- b_count[count,coin-1]
      }else{
        b_count[count,coin] <- 1 + b_count[count,coin-1]
        a_count[count,coin] <- a_count[count,coin-1]
      }
    }
    if(a_count[count,coin] > b_count[count,coin]){
      winner[count,coin] <- 1
    }else if(b_count[count,coin] > a_count[count,coin]){
      winner[count,coin] <- 0
    }else{
      winner[count,coin] <- ifelse(a_count[count,coin-1] > b_count[count,coin-1],1,0)
    }
  }
}

count_A <- apply(winner,1,sum)

count_B <- apply(winner_B,1,function(x){sum(x==0)})

win_A <- ifelse(count_A == 20,1,0)

sum(win_A)/exp

half_win <- ifelse(count_A == 10,1,0)

sum(half_win)/exp

win_B <- ifelse(count_A == 0,1,0)

sum(win_A)/exp

cum_winner <- t(apply(winner,1,cumsum))

probability <- rep(0,11)

p <- 0

for(i in seq(0,toss,2)){
  p <- p+1
  probability[p] <- sum(ifelse(count_A == i,1,0))/exp
}

plot(probability,type='l')

