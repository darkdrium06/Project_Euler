problem_1 <- function(){
  sum <- 0
  for(i in 1:999){
    if(i%%3==0 || i%%5==0){
      sum <- sum + i
    }
  }
  sum
}

problem_2 <- function(){
  fib <- c()
  fib[1] <- 1
  fib[2] <- 2
  i <- 3
  sum <- 2
  while(fib[i-1] < 4000000){
    fib[i] <- fib[i-1] + fib[i-2]
    if(fib[i]%%2==0){
      sum <- sum + fib[i]
    }
    i <- i+1
  }
  sum
}

sieve <- function(n){
  a <- !logical(length(2:n))
  if(n < 2){
    NA
  }
  else if(n %in% 2:3){
    2
  }
  else
  {
    for(i in 2:floor(sqrt(n))){
      if(a[i]){
        for(j in seq(i^2, n, by = i)){
          a[j] <- FALSE
        }
      }
    }
    which(a==TRUE)[-1]
  }
}

problem_3 <- function(){
  primes <- sieve(floor(sqrt(600851475143)))
  primes[max(which(600851475143%%primes==0))]
}

problem_4 <- function(){
  max <- 0
  for(i in 999:100){
    for(j in 999:100){
      prod <- as.character(i*j)
      if(prod==paste(rev(strsplit(prod,"")[[1]]),collapse="") && i*j > max){
        max <- i*j
      }
    }
  }
  max
}

problem_5 <- function(){
  i <- prod(sieve(20))
  while(sum(i%%1:20) != 0){
    i <- i + prod(sieve(20))
  }
  i
}

problem_6 <- function(){
  sum(1:100)^2 - sum((1:100)*(1:100))
}

problem_7 <- function(){
  i <- 1
  while(length(sieve(i)) < 10001){
    i <- i + 10000
  }
  sieve(i)[10001]
}

problem_8 <- function(){
  number = "73167176531330624919225119674426574742355349194934
            96983520312774506326239578318016984801869478851843
            85861560789112949495459501737958331952853208805511
            12540698747158523863050715693290963295227443043557
            66896648950445244523161731856403098711121722383113
            62229893423380308135336276614282806444486645238749
            30358907296290491560440772390713810515859307960866
            70172427121883998797908792274921901699720888093776
            65727333001053367881220235421809751254540594752243
            52584907711670556013604839586446706324415722155397
            53697817977846174064955149290862569321978468622482
            83972241375657056057490261407972968652414535100474
            82166370484403199890008895243450658541227588666881
            16427171479924442928230863465674813919123162824586
            17866458359124566529476545682848912883142607690042
            24219022671055626321111109370544217506941658960408
            07198403850962455444362981230987879927244284909188
            84580156166097919133875499200524063689912560717606
            05886116467109405077541002256983155200055935729725
            71636269561882670428252483600823257530420752963450"
  number <- gsub("[^0-9]", "", number)
  max <- 0
  for(i in 1:nchar(number)){
    thirteen_gram <- substr(number, i, i + 12)
    if(grepl("0", thirteen_gram)){
      i <- i+max(unlist(gregexpr("0", thirteen_gram)))
      next
    }
    else {
      nums <- as.integer(unlist(strsplit(thirteen_gram, "")))
      if(prod(nums) > max){
        max <- prod(nums)
      }
    }
  }
  max
}

problem_9 <- function(){
  sum <- 0
  i <- 3
  repeat{
    if(i%%2 == 0){
      a <- i
      b <- (i/2)^2 - 1
      c <- (i/2)^2 + 1
    }
    else{
      a <- i
      b <- (i^2 - 1) / 2
      c <- (i^2 + 1) / 2
    }
    sum <- a + b + c
    if(1000%%sum == 0){
      prod <- a * b * c * (1000/sum)^3
      break
    }
    i <- i + 1
  }
  prod
}

problem_10 <- function(){
  sum(as.numeric(sieve(2000000)))
}

problem_11 <- function(){
  setwd("C:/Users/06sha_000/Documents/R/Project Euler")
  grid <- readLines("problem11.txt", warn = FALSE)
  grid <- as.numeric(unlist(lapply(grid, function(x){strsplit(x, " ")})))
  grid <- matrix(grid, ncol=20)
  
  vertical <- grid[1:17, ] * grid[2:18, ] * grid[3:19, ] * grid[4:20, ]
  horizontal <- grid[,1:17] * grid[,2:18] * grid[,3:19] * grid[,4:20]
  diagonal_one <- grid[1:17, 1:17] * grid[2:18, 2:18] * grid[3:19, 3:19] * grid[4:20, 4:20]
  diagonal_two <- grid[4:20, 1:17] * grid[3:19, 2:18] * grid[2:18, 3:19] * grid[1:17, 4:20]
  
  max(vertical,horizontal,diagonal_one,diagonal_two)
}

all_divisors <- function(n){
  possible_divisors <- 1:n
  proper_divisors <- possible_divisors[n%%possible_divisors == 0]
  proper_divisors
}

#try to rewrite without using numbers package
problem_12 <- function(){
  require(numbers)
  triangle <- 1
  i <- 1
  while(length(divisors(triangle)) < 500){
    i <- i + 1
    triangle <- triangle + i
  }
  triangle
}

problem_13 <- function(){
  setwd("C:/Users/06sha_000/Documents/R/Project Euler")
  options(scipen=999)
  numbers <- as.numeric(readLines("problem13.txt", warn = FALSE))
  substr(as.character(sum(numbers)),1,10)
}

#need to optimize
problem_14 <- function(){
  require(hashmap)
  length_of_chains <- hashmap(1,1)
  for(i in 1000000:1){
    n <- i
    count <- 1
    chains <- c()
    chains[count] <- n
    while(n != 1){
      n <- ifelse(n%%2 == 0, n/2, 3*n + 1)
      if(!is.na(length_of_chains[[n]])){
        chains <- rev(chains)
        for(j in 1:count){
          length_of_chains[[chains[j]]] <- length_of_chains[[n]] + j
        }
        break
      }
      else{
        count <- count + 1
        chains[count] <- n
      }
    }
  }
  as.integer(names(which.max(length_of_chains$data())))
}

problem_15 <- function(){
  choose(40,20)
}

problem_16 <- function(){
  require(gmp)
  number <- as.character(as.bigz(2^1000))
  sum <- 0 
  for(i in 1:nchar(number)){
    sum <- sum + as.integer(substr(number,i,i))
  }
  sum
}

digit_to_word <- function(n){
  numbers <- c(1:20, seq(30, 90, by = 10))
  letters <- c("one", "two", "three", "four", "five", "six", "seven", "eight", 
                                                 "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", 
                                                 "sixteen", "seventeen", "eighteen", "nineteen", "twenty", "thirty", 
                                                 "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
  basic_numbers <- data.frame("Numbers" = numbers, "Letters" = letters, stringsAsFactors = FALSE)
  
  str <- as.character(n)
  
  if(nchar(str) == 1){
    return(basic_numbers[basic_numbers$Number==n,]$Letters)
  }
  else if(nchar(str) == 2){
    if(n < 20 || n%%10 == 0){
      return(basic_numbers[basic_numbers$Number==n,]$Letters)
    }
    else{
      return(paste0(digit_to_word(n%%100-n%%10),digit_to_word(n%%10)))
    }
  }
  else if(nchar(str) == 3){
    if(n%%100 == 0){
      return(paste0(basic_numbers[basic_numbers$Number==as.integer(substr(str,1,1)),]$Letters, 
                   "hundred"))
    }
    else{
      return(paste0(digit_to_word(n%%1000-n%%100), "and", digit_to_word(n%%100)))
    }
  }
  else{
    "onethousand"
  }
}

problem_17 <- function(){
  sum(nchar(unlist(lapply(1:1000, digit_to_word))))
}

problem_18 <- function(){
  setwd("C:/Users/06sha_000/Documents/R/Project Euler")
  triangle <- readLines("problem18.txt", warn = FALSE)
  triangle_size <- length(triangle)
  nums <- list()
  for(i in 1:triangle_size){
    nums <- c(nums, list(as.numeric(unlist(strsplit(triangle[i], " ")))))
  }

  for(i in (length(nums)-1):1){
    for(j in 1:length(nums[[i]])){
      nums[[i]][j] <- max(nums[[i+1]][j]+nums[[i]][j],nums[[i+1]][j+1]+nums[[i]][j])
    }
  }
  nums[[1]][1]
}

problem_19 <- function(){
  day_start <- 1
  num_sundays <- 0
  for(i in 1900:2000){
    if(i%%4 == 0){
      days_in_months <- c(31,29,31,30,31,30,31,31,30,31,30,31)
    }
    else{
      days_in_months <- c(31,28,31,30,31,30,31,31,30,31,30,31)
    }
    for(j in 1:length(days_in_months)){
      day_start <- day_start+days_in_months[j]
      if(day_start%%7 == 0 && i != 1900){
        num_sundays <- num_sundays + 1
      }
    }
  }
  num_sundays
}

problem_20 <- function(){
  require(gmp)
  options(scipen=999)
  number <- as.character(factorialZ(100))
  sum <- 0 
  for(i in 1:nchar(number)){
    sum <- sum + as.integer(substr(number,i,i))
  }
  sum
}

problem_21 <- function(){
  sums <- NULL
  for (i in 1:10000) {
    possible_divisors <- 1:(i-1)
    proper_divisors <- possible_divisors[i%%possible_divisors == 0]
    sums[length(sums) + 1] <- sum(proper_divisors)
  }
  
  sum <- 0
  for(i in 1:length(sums)){
    if(length(i == sums[sums[i]]) == 1 && !is.na(sums[sums[i]])){
      if(i == sums[sums[i]] && i != sums[i]){
        sum <- sum + i + sums[i]
      }
    }
  }
  sum/2
}

problem_22 <- function(){
  setwd("C:/Users/06sha_000/Documents/R/Project Euler")
  names <- readLines("problem22.txt", warn = FALSE)
  names <- sort(unlist(strsplit(gsub("\"", "", names), ",")))
  
  numbers <- 1:26
  letters <- toupper(letters)
  letter_values <- data.frame("Numbers" = numbers, "Letters" = letters, stringsAsFactors = FALSE)
  
  sum <- 0
  for(i in 1:length(names)){
    str <- names[i]
    name_score <- 0
    for(j in 1:nchar(str)){
      name_score <- name_score + letter_values[letter_values$Letters==substring(str,j,j),]$Numbers
    }
    sum <- sum + (name_score * i)
  }
  sum
}

problem_23 <- function(){
  abundants <- NULL
  for (i in 2:28123) {
    possible_divisors <- 1:(i-1)
    proper_divisors <- possible_divisors[i%%possible_divisors == 0]
    if (sum(proper_divisors) > i) {
      abundants[length(abundants) + 1] <- i
    }
  }
  
  sums <- list()
  for(i in seq(abundants)){
    sums[[i]] <- abundants[i] + abundants
  }
  
  sums <- unique(unlist(sums))
  sum(1:28123) - sum(sums[sums <= 28123])
}

permutations <- function(n){
  if(n==1){
    return(matrix(1))
  } else {
    sp <- permutations(n-1)
    p <- nrow(sp)
    A <- matrix(nrow=n*p,ncol=n)
    for(i in 1:n){
      A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
    }
    return(A)
  }
}

problem_24 <- function(){
  nums <- 0:9
  paste0(as.character(matrix(nums[permutations(10)],ncol=10)[1000000,]), collapse="")
}

fib <- function(n){
  require(gmp)
  first <- as.bigz(0)
  second <- as.bigz(1)
  third <- as.bigz(0)
  for(i in seq_len(n)){
    third <- add.bigz(first, second)
    first <- second
    second <- third
  }
  first
}

problem_25 <- function(){
  i <- 0
  term <- ""
  while(nchar(term) < 1000){
    i <- i + 1
    term <- as.character(fibnum(i))
  }
  i
}

problem_26 <- function(){
  max_length <- 0
  for(i in 2:1000){
    rest <- 1
    for(j in 0:i){
      rest <- (rest * 10)%%i
    }
    r0 <- rest
    len <- 0
    repeat{
      rest <- (rest * 10)%%i
      len <- len + 1
      if(rest == r0){
        break
      }
    }
    if(len > max_length){
      maxi <- i
      max_length <- len
    }
  }
  maxi
}

is_prime <- function(n){
  if(n<=1){
    return(FALSE)
  }
  for(i in 2:ceiling(sqrt(n))){
    if(n%%i == 0){
      return(FALSE)
    }
    
  }
  return(TRUE)
}

problem_27 <- function(){
  max <- 0
  for(a in -999:999){
    for(b in sieve(1000)){
      x <- 0
      repeat{
        formula <- x^2+a*x+b
        if(!is_prime(formula)){
          break
        }
        x <- x + 1
      }
      if(x > max){
        max <- x
        product <- a*b
      }
    }
  }
  product
}

problem_28 <- function(){
  sum <- 1
  for(i in 2:1001){
    if(i%%2 == 0){
      sum <- sum + (3*(i*i + 1) + 1)
    }
    else{
      sum <- sum + (i*i - 1)
    }
  }
  sum
}

problem_29 <- function(){
  combos <- as.bigz(NULL)
  for(i in 2:100){
    for(j in 2:100){
      combos <- c(combos, pow.bigz(i,j))
    }
  }
  length(unique(combos))
}

problem_30 <- function(){
  total <- 0
  for(i in 2:(6*9^5)){
    str = as.character(i)
    sum <- 0
    for(j in 1:nchar(str)){
      sum <- sum + as.integer(substr(str,j,j))^5
    }
    
    if(sum == i){
      total <- total + i
    }
  }
  total
}

problem_31 <- function(){
  coins <- c(1, 2, 5, 10, 20, 50, 100, 200)
  amount <- 200
  v <- c(1, rep(0, amount))
  coins <- coins + 1
  for(i in 1:length(coins)){
    for(j in coins[i]:(amount+1)){
      v[j] <- v[j] + v[j-(coins[i]-1)]
    }
  }
  v[amount+1]
  
}

problem_32 <- function(){
  nums <- NULL
  for(i in 1:100){
    for(j in 1:10000){
      str <- as.character(c(i,j,i*j))
      unique_str <- unique(unlist(strsplit(str, "")))
      if(sum(nchar(str)) != 9 || "0" %in% unique_str){
        next
      }
      
      if(sum(nchar(str)) != 9){
        break
      }
      
      if(length(unique_str) == 9){
        nums <- c(nums, i*j)
      }
    }
  }
  sum(unique(nums))
}

problem_33 <- function(){
  prod <- 1
  for(i in 10:98){
    for(j in i+1:99){
      if((i%%10 == j%/%10) || (i%/%10 == j%%10)){
        if((j/i) == (j%/%10)/(i%%10) || (j/i) == (j%%10)/(i%/%10)){
          prod <- prod * (i/j)
        }
      }
    }
  }
  1/prod
}

problem_34 <- function(){
  i <- 3:99999
  sum_facts <- unlist(lapply(strsplit(as.character(i), ""), function(x) { sum(factorial(as.integer(x)))}))
  sum(i[which(i == sum_facts)])
}

rotations <- function(n){
  str <- as.character(n)
  rot <- n
  for(i in 1:nchar(str)){
    str <- paste0(substr(str,2,nchar(str)), substr(str,1,1), collapse = "")
    rot <- c(rot, as.integer(str))
  }
  unique(rot)
}

#optimize
problem_35 <- function(){
  primes <- sieve(1000000)
  primes <- primes[-unlist(lapply(strsplit(as.character(primes), ""), function(x) {"0" %in% as.character(as.integer(x)%%2)}))]
  count <- 1 ##start at one to include 2
  for(i in 1:length(primes)){
    rot <- rotations(primes[i])
    if(sum(rot %in% primes) == length(rot)){
      count <- count + 1
    }
  }
  count
}

integer_to_binary <- function(n){
  isna <- is.na(n);
  y <- n[!isna];
  ans0 <- character(length(y));
  z <- NULL;
  while (any(y > 0) | is.null(z)) {
    z <- y%%2;
    y <- floor(y/2);
    ans0 <- paste(z, ans0, sep = "");
  }
  ans <- rep(as.character(NA), length(n));
  ans[!isna] <- ans0;
  ans;
}

problem_36 <- function(){
  x <- as.character(1:999999)
  decimal_palindromes <- x[which(x==sapply(lapply(strsplit(x, NULL), rev), paste, collapse=""))]
  binary_numbers <- unlist(lapply(as.integer(decimal_palindromes), integer_to_binary))
  binary_palindromes <- binary_numbers[which(binary_numbers==sapply(lapply(strsplit(binary_numbers, NULL), rev), paste, collapse=""))]
  sum(strtoi(binary_palindromes, base = 2))
}

problem_37 <- function(){
  n <- as.character(c(seq(0, 8, 2)))[-2]
  
  r <- integer(0)
  for(i in sieve(1e6)[-(1:4)]){
    f <- TRUE
    x <- unlist(strsplit(as.character(i), ""))
    if(any(n %in% x))
      next
    for(j in 1:length(x)) {
      x1 <- is_prime(as.integer(paste(x[1:j], collapse = "", sep = "")))
      x2 <- is_prime(as.integer(paste(x[j:length(x)], collapse = "", sep = "")))
      if(!(x1 & x2)) {
        f <- FALSE
        break
      }
    }
    if(f)
      r <- c(r, i)
  }
  sum(r)
}

problem_38 <- function(){
  max <- 0
  for(i in 2:1000000){
    j <- 1
    str <- ""
    repeat{
      str <- paste0(str,as.character(i*j), collapse = "")
      if(nchar(str) >= 9){
        break
      }
      j <- j + 1
    }
    chars <- unique(unlist(strsplit(str,"")))
    if("0" %in% chars){
      next
    }
    else if(nchar(str) == 9 && length(chars) == 9 && as.integer(str) > max){
      max <- as.integer(str)
    }
  }
  max
}

problem_39 <- function(){
  sides <- 1:999
  sides <- sides^2
  sums <- list()
  for(i in 1:length(sides)){
    for(j in i:length(sides)){
      sum <- sides[i] + sides[j]
      p <- sqrt(sides[i]) + sqrt(sides[j])
      if(sum %in% sides && p < 1000){
        
      }
    }
  }
}

#ptm <- proc.time()
#proc.time() - ptm






