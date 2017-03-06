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

# sieve <- function(n){
#   a <- !logical(length(2:n))
#   if(n < 2){
#     NA
#   }
#   else if(n %in% 2:3){
#     2
#   }
#   else
#   {
#     for(i in 2:floor(sqrt(n))){
#       if(a[i]){
#         for(j in seq(i^2, n, by = i)){
#           a[j] <- FALSE
#         }
#       }
#     }
#     which(a==TRUE)[-1]
#   }
# }

sieve <- function(n) {
  if (n < 2) return(NULL)
  a <- rep(T, n)
  a[1] <- F
  for(i in seq(n)) {
    if (a[i]) {
      j <- i * i
      if (j > n) return(which(a))
      a[seq(j, n, by=i)] <- F
    }
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
  setwd("C:/Users/mshay/Documents/R/Project_Euler")
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
  setwd("C:/Users/mshay/Documents/R/Project_Euler")
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
  setwd("C:/Users/mshay/Documents/R/Project_Euler")
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
  setwd("C:/Users/mshay/Documents/R/Project_Euler")
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

# is_prime <- function(n){
#   if(n <= 1){
#     return(FALSE)
#   }
#   for(i in 2:ceiling(sqrt(n))){
#     if(n%%i == 0){
#       return(FALSE)
#     }
#     
#   }
#   return(TRUE)
# }

is_prime <- function(n){
  if(n <= 1){
    return(FALSE)
  }
  else if(n == 2){
    return(TRUE)
  }
  else{
    return(all(n%%2:ceiling(sqrt(n))!=0))
  }
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
  sums <- c(matrix(0, 1000, 1))
  for(i in 1:length(sides)){
    for(j in i:length(sides)){
      sum <- sides[i] + sides[j]
      p <- sqrt(sides[i]) + sqrt(sides[j]) + sqrt(sum)
      if(sum %in% sides && p < 1000){
        sums[p] <- sums[p] + 1
      }
    }
  }
  which.max(sums)
}

problem_40 <- function(){
  irrational <- paste0(as.character(1:1000000), collapse = "")
  as.integer(substr(irrational,1,1))*as.integer(substr(irrational,10,10))*as.integer(substr(irrational,100,100))*as.integer(substr(irrational,1000,1000))*as.integer(substr(irrational,10000,10000))*as.integer(substr(irrational,100000,100000))*as.integer(substr(irrational,1000000,1000000))
}

problem_41 <- function(){
  max <- 0
  for(i in 4:9){
    nums <- 1:i
    perms <- as.integer(apply(matrix(nums[permutations(i)],ncol=i),1,paste,collapse=""))
    for(j in 1:length(perms)){
      if(is_prime(perms[j]) && perms[j] > max){
        max <- perms[j]
      }
    }
  }
  max
}

problem_42 <- function(){
  setwd("C:/Users/mshay/Documents/R/Project_Euler")
  words <- readLines("problem42.txt", warn = FALSE)
  
  words <- sort(unlist(strsplit(gsub("\"", "", words), ",")))
  
  numbers <- 1:26
  letters <- toupper(letters)
  letter_values <- data.frame("Numbers" = numbers, "Letters" = letters, stringsAsFactors = FALSE)
  
  all_words <- NULL
  for(i in 1:length(words)){
    str <- words[i]
    word_score <- 0
    for(j in 1:nchar(str)){
      word_score <- word_score + letter_values[letter_values$Letters==substring(str,j,j),]$Numbers
    }
    all_words <- c(all_words, word_score)
  }
  
  i <- 1
  n <- 1
  triangle_numbers <- NULL
  while(i < max(all_words)){
    i <- 1/2*n*(n+1)
    triangle_numbers <- c(triangle_numbers, i)
    n <- n+1
  }
  
  sum(all_words %in% triangle_numbers)
}

remove_doubles <- function(n){
  ind <- which(n%/%100 == (n%/%10)%%10 | n%/%100 == n%%10 | (n%/%10)%%10 == n%%10)
 if(length(ind) == 0){
   return(n)
 }
  else{
   return(n[-ind])
  }
}

create_numbers <- function(x, prime){
  nums <- NULL
  if(prime != 0){
    substr_len <- max(nchar(x))
    n <- unique(substr(x, substr_len-1, substr_len))
    for(i in 1:length(n)){
      for(j in 0:9){
        nums <- c(nums, paste0(n[i], as.character(j), collapse = ""))
      }
    }
    nums <- as.integer(nums)
    new_digits <- as.character(remove_doubles(nums[nums%%prime == 0]))
    new_digits <- unlist(lapply(as.character(new_digits), function(x) {if(nchar(x) == 2) paste0("0",x) else x}))
    
    current_nums <- NULL
    for(i in 1:length(x)){
      for(j in 1:length(new_digits)){
        if(substr(x[i], substr_len-1, substr_len) == substr(new_digits[j], 1, 2)){
          current_nums <- c(current_nums, paste0(x[i], substr(new_digits[j], 3, 3), collapse = ""))
        }
      }
    }
  }
  else{
    n <- x
    for(i in 1:length(n)){
      for(j in 0:9){
        nums <- c(nums, paste0(as.character(j), n[i], collapse = ""))
      }
    }
    current_nums <- nums
  }
  
  nums_to_remove <- NULL
  for(i in 1:length(current_nums)){
    str <- current_nums[i]
    counts <- rep(0,10)

    for(j in 1:nchar(str)){
      digit <- as.integer(substr(str,j,j))+1
      counts[digit] <- counts[digit] + 1
    }
    if(length(which(counts>1)) > 0){
      nums_to_remove <- c(nums_to_remove, i)
    }
  }

  current_nums[-nums_to_remove]
}

problem_43 <- function(){
  require(gmp)
  nums <- remove_doubles(seq(0, 999, by = 2))
  nums <- unlist(lapply(as.character(nums), function(x) {if(nchar(x) == 2) paste0("0",x) else x}))
  primes <- sieve(18)[-1]
  for(i in 1:length(primes)){
    nums <- create_numbers(nums, primes[i])
  }
  
  nums <- create_numbers(nums, 0)
  
  sum(as.bigz(nums))
}

is_pentagonal <- function(n){
  x <- floor((1 + sqrt( 1 + 24*n))/6)
  x*(3*x-1)/2 == n
}

problem_44 <- function(){
  n <- 1:3000
  pents <- n*(3*n-1)/2
  combos <- expand.grid(pents, pents)
  combos <- combos[which(combos$Var1 != combos$Var2),]
  combos <- combos[which(is_pentagonal(combos$Var1+combos$Var2) & is_pentagonal(abs(combos$Var1-combos$Var2))),]
  min(abs(combos$Var1-combos$Var2))
}

is_hexagonal <- function(n){
  x <- floor((1 + sqrt( 1 + 8*n))/4)
  x*(2*x-1) == n
}

problem_45 <- function(){
  i <- 286
  triangle <- i*(i+1)/2
  while(!is_pentagonal(triangle) || !is_hexagonal(triangle)){
    i <- i + 1
    triangle <- i*(i+1)/2
  }
  triangle
}

problem_46 <- function(){
  n <- 35
  result <- 0
  while(TRUE){
    if(!is_prime(n)){
      i <- 1
      while(2*i*i < n){
        if(is_prime(n-2*i*i)){
          break
        }
        i <- i + 1
      }
      if(!is_prime(n-2*i*i)){
        result <- n
        break
      }
    }
    n <- n + 2
  }
  result
}

problem_47 <- function(){
  limit <- 1000000
  factors <- rep(0,limit)
  count <- 0
  result <- 0
  for(i in 2:limit){
    if(factors[i] == 0){
      count <- 0
      num <- i
      while(num < limit){
        factors[num] <- factors[num] + 1
        num <- num + i
      }
    }
    else if(factors[i] == 4){
      count <- count + 1
      if(count == 4){
        result <- i-3
        break
      }
    }
    else{
      count <- 0
    }
  }
  result
}

problem_48 <- function(){
  require(gmp)
  i <- 1:1000
  str <- as.character(sum(pow.bigz(i,i)))
  substr(str, nchar(str)-9, nchar(str))
}

problem_49 <- function(){
  primes <- sieve(10000)
  primes <- primes[which(primes > 999 & !(primes %in% c(1487, 4817, 8147)))]
  seq_num <- NULL
  i <- 1
  while(TRUE){
    digits <- as.integer(unlist(strsplit(as.character(primes[i]), "")))
    nums <- as.integer(apply(matrix(digits[permutations(4)],ncol=4),1,paste,collapse=""))
    sequence <- c(primes[i], primes[i]+3330, primes[i]+2*3330)
    if(sum(sequence %in% nums) == 3 && sum(sequence %in% primes) == 3){
      seq_num <- paste0(as.character(sequence), collapse = "")
      break
    }
    i <- i + 1
  }
  seq_num
}

problem_50 <- function(){
  primes <- sieve(1000000)
  primes_to_sum <- primes[1:700]
  consecutive_sums <- NULL
  for(i in 1:length(primes_to_sum)){
    sums <- cumsum(primes_to_sum[i:length(primes_to_sum)])
    filtered_sums <- sums[sums %in% primes]
    filtered_sums <- max(filtered_sums)
    consecutive_sums <- rbind(consecutive_sums, c((which(sums==filtered_sums)-i+1), filtered_sums))
  }
  consecutive_sums[which.max(consecutive_sums[,1]),2]
}

problem_51 <- function(){
  primes <- sieve(1000000)
  primes <- as.character(primes[primes > 10000])
  min <- 0
  for(i in primes){
    counts <- rep(0,10)
    for(j in 1:nchar(i)){
      digit <- as.integer(substr(i,j,j))+1
      counts[digit] <- counts[digit] + 1
    }
    if(max(counts) < 3){
      next
    }
    else{
      digits <- as.character(0:9)
      num_to_replace <- digits[which.max(counts)]
      composite_count <- 0
      prime_count <- 0
      first_family <- TRUE
      for(k in digits){
        new_num <- gsub(num_to_replace, k, i)
        if(composite_count > 2 || prime_count == 8){
          break
        }
        if(new_num %in% primes){
          prime_count <- prime_count + 1
          if(first_family){
            min <- new_num
            first_family <- FALSE
          }
        }
        else{
          composite_count <- composite_count + 1
        }
      }
      if(prime_count == 8){
        break
      }
    }
  }
  min
}

is_digit_anagram <- function(a,b){
  x <- as.character(a)
  y <- as.character(b)
  
  counts <- rep(0,10)
  for(j in 1:nchar(x)){
    digit <- as.integer(substr(x,j,j))+1
    counts[digit] <- counts[digit] + 1
  }
  
  for(j in 1:nchar(y)){
    digit <- as.integer(substr(y,j,j))+1
    counts[digit] <- counts[digit] - 1
    if(counts[digit] < 0){
      return(FALSE)
    }
  }

  sum(counts) == 0
}

problem_52 <- function(){
  for(i in 100000:1000000){
    j <- 2
    count <- 1
    while(is_digit_anagram(i,i*j)){
      count <- count + 1
      j <- j + 1
    }
    if(count == 6){
      break;
    }
  }
  i
}

problem_53 <- function(){
  total <- 0
  for(i in 1:100){
    for(j in 1:100){
      if(choose(i,j) > 1000000){
        total <- total + 1
      }
    }
  }
  total
}

parse_hand <- function(hand){
  values <- NULL
  suits <- NULL
  for(i in 1:10){
    if(i%%2 == 0){
      suits <- c(suits, substr(hand,i,i))
    }
    else{
      number_value <- NULL
      switch(substr(hand,i,i),
             "T" = {number_value <- "10"},
             "J" = {number_value <- "11"},
             "Q" = {number_value <- "12"},
             "K" = {number_value <- "13"},
             "A" = {number_value <- "14"},
             number_value <- substr(hand,i,i))
      values <- c(values, number_value)
    }
  }
  list(suits,as.integer(values))
}

rank_hand <- function(parsed_hand){
  high_card <- max(parsed_hand[[2]])
  #flushes
  if(length(unique(parsed_hand[[1]])) == 1){
    #royal flush
    if(sum(parsed_hand[[2]]) == 60){
      return(10)
    }
    else{
      #straight flush
      if(length(unique(parsed_hand[[2]])) == 5 && max(parsed_hand[[2]])-min(parsed_hand[[2]]) == 4){
        return(c(9,high_card))
      }
      #flush
      else{
        return(c(6,high_card))
      }
    }
  }
  #n of a kind
  else if(length(unique(parsed_hand[[2]])) < 5){
    counts <- rep(0,14)
    for(i in 1:5){
      digit <- parsed_hand[[2]][i]
      counts[digit] <- counts[digit] + 1
    }
    max_count <- max(counts)
    num_counts <- length(counts[counts>0])
    #four of a kind
    if(max_count == 4){
      return(c(8,which(counts==4),which(counts==1))) 
    }
    else{
      if(max_count == 3){
        #full house
        if(num_counts == 2){
          return(c(7,which(counts==3),which(counts==2)))
        }
        #three of a kind
        else{
          return(c(4,which(counts==3),max(which(counts==1)),min(which(counts==1))))
        }
      }
      else{ #max_count == 2
        #two pair
        if(num_counts == 3){
          return(c(3,max(which(counts==2)),min(which(counts==2)),which(counts==1)))
        }
        #one pair
        else{ #num_counts == 4
          return(c(2,which(counts==2),rev(which(counts==1))))
        }
      }
    }
  }
  #straight
  else if(length(unique(parsed_hand[[2]])) == 5 && max(parsed_hand[[2]])-min(parsed_hand[[2]]) == 4){
    return(c(5,high_card)) 
  }
  #high card
  else{
    return(c(1,rev(sort(parsed_hand[[2]]))))
  }
}

problem_54 <- function(){
  setwd("C:/Users/mshay/Documents/R/Project_Euler")
  hands <- gsub(" ", "", readLines("problem54.txt", warn = FALSE))
  player_one <- substr(hands,1,10)
  player_two <- substr(hands,11,20)
  player_one_ranks <- lapply(player_one, function(x) {rank_hand(parse_hand(x))})
  player_two_ranks <- lapply(player_two, function(x) {rank_hand(parse_hand(x))})
  num_wins <- 0
  for(i in 1:length(player_one_ranks)){
    player_one_hand <- player_one_ranks[[i]]
    player_two_hand <- player_two_ranks[[i]]
    j <- 1
    if(player_one_hand[j] > player_two_hand[j]){
      num_wins <- num_wins + 1
    }
    else if(player_one_hand[j] < player_two_hand[j]){
      next
    }
    else{
      while(player_one_hand[j] == player_two_hand[j]){
        j <- j + 1
      }
      if(player_one_hand[j] > player_two_hand[j]){
        num_wins <- num_wins + 1
      }
    }
  }
  num_wins
}

reverse_number <- function(n){
  require(gmp)
  str <- paste0(rev(unlist(strsplit(as.character(n),""))), collapse = "")
  as.bigz(substr(str,regexpr("[^0]",str),nchar(str)))
}

problem_55 <- function(){
  require(gmp)
  count <- 0
  for(i in 80:9999){
    i <- as.bigz(i)
    sum <- add.bigz(i, reverse_number(i))
    for(j in 1:50){
      if(sum == reverse_number(sum)){
        break
      }
      else{
        sum <- add.bigz(sum, reverse_number(sum))
      }
    }
    if(j == 50){
      count <- count + 1
    }
  }
  count
}

problem_56 <- function(){
  require(gmp)
  max <- 0
  for(i in 2:99){
    for(j in 2:99){
      power <- pow.bigz(i,j)
      power <- as.character(power)
      sum <- 0
      for(k in 1:nchar(power)){
        sum <- sum + as.integer(substr(power,k,k))
      }
      if(sum > max){
        max <- sum
      }
    }
  }
  max
}

problem_57 <- function(){
  require(gmp)
  numerator <- as.bigz(1)
  denominator <- as.bigz(1)
  count <- 0
  for(i in 1:1000){
    old_denominator <- denominator
    denominator <- add.bigz(numerator, denominator)
    numerator <- add.bigz(denominator, old_denominator)
    if(nchar(as.character(numerator)) > nchar(as.character(denominator))){
      count <- count + 1
    }
  }
  count
}

problem_58 <- function(){
  ratio <- 1
  n <- 1
  num_primes <- 0
  num_diag <- 1
  while(ratio > .1){
    n <- n + 2
    num_primes <- num_primes + sum(unlist(lapply(c(n^2,n^2-n+1,n^2-2*n+2,n^2-3*n+3), is_prime)))
    num_diag <- num_diag + 4
    ratio <- num_primes/num_diag
  }
  n
}

problem_59 <- function(){
  setwd("C:/Users/mshay/Documents/R/Project_Euler")
  codes <- readLines("problem59.txt", warn = FALSE)
  codes <- as.integer(unlist(strsplit(codes, ",")))
  ascii <- utf8ToInt("a"):utf8ToInt("z")
  encryption_keys <- expand.grid(ascii, ascii, ascii)
  decoded_messages <- vector(mode = "character", length = nrow(encryption_keys))
  for(i in 1:nrow(encryption_keys)){
    repeated_encryption <- c(rep(unname(unlist(encryption_keys[i,])), length(codes)%/%3),encryption_keys[i,1])
    decoded_messages[i] <- intToUtf8(bitwXor(codes,repeated_encryption))
  }
  sum(utf8ToInt(decoded_messages[grep(" the ", decoded_messages)]))
}

find_min_set <- function(primes){
  len <- length(primes)
  prime_pairs <- matrix(data = FALSE, ncol = len, nrow = len)
  for(i in 1:(len-1)){
    for(j in (i+1):len){
      first_prime <- primes[i]*10^(nchar(primes[j]))+primes[j]
      second_prime <- primes[j]*10^(nchar(primes[i]))+primes[i]
      concat_primes_are_prime <- is_prime(first_prime) && is_prime(second_prime)
      prime_pairs[i,j] = concat_primes_are_prime
      prime_pairs[j,i] = concat_primes_are_prime
    }
  }
  prime_pairs
  sums <- NULL
  for(i in 1:(len-4)){
    for(j in (i+1):(len-3)){
      if(prime_pairs[i,j]){
        for(k in (j+1):(len-2)){
          if(prime_pairs[i,k] && prime_pairs[j,k]){
            for(l in (k+1):(len-1)){
              if(prime_pairs[i,l] && prime_pairs[j,l] && prime_pairs[k,l]){
                for(m in (l+1):len){
                  if(prime_pairs[i,m] && prime_pairs[j,m] && prime_pairs[k,m] && prime_pairs[l,m]){
                    sums <- c(sums, sum(primes[c(i,j,k,l,m)]))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  ifelse(length(sums) != 0, min(sums), Inf)
}

#need to optimize
problem_60 <- function(){
  primes <- sieve(10000)[-c(1,3)]
  primes_1 <- c(3,primes[primes%%3 == 1])
  primes_2 <- c(3,primes[primes%%3 == 2])
  min(find_min_set(primes_1),find_min_set(primes_2))
}

find_cycles <- function(set_one, set_two){
  potential_cycles <- list()
  for(i in set_one){
    first_set_one <- substr(i, 1, 2)
    last_set_one <- substr(i, nchar(i)-1, nchar(i)) 
    for(j in set_two){
      first_set_two <- substr(j, 1, 2)
      last_set_two <- substr(j, nchar(j)-1, nchar(j))
      if(first_set_one %in% last_set_two){
        potential_cycles[[length(potential_cycles)+1]] <- paste0(j, i, collapse = "")
      }
      else if(last_set_one %in% first_set_two){
        potential_cycles[[length(potential_cycles)+1]] <- paste0(i, j, collapse = "")
      }
    }
  }
  potential_cycles[-which(substr(potential_cycles,nchar(potential_cycles),nchar(potential_cycles))=="0")]
}

problem_61 <- function(){
  formulas <- c("n*(n+1)/2", "n*n", "n*(3*n-1)/2", "n*(2*n-1)", "n*(5*n-3)/2", "n*(3*n-2)")
  all_sets <- list()
  for(i in 1:6){
    n <- 1:200
    set <- eval(parse(text=formulas[i]))
    set <- as.character(set[nchar(set) == 4])
    all_sets[[i]] <- set
  }
  
  n <- 1:6
  perms <- matrix(n[permutations(6)], ncol = 6)
  perms <- perms[perms[,1] == 6,]
  for(i in 1:nrow(perms)){
    iteration <- perms[i,]
    cycle <- all_sets[[iteration[1]]]
    for(j in 2:length(iteration)){
      cycle <- find_cycles(cycle,all_sets[[iteration[j]]])
    }
    if(length(cycle) != 0){
      for(i in 1:length(cycle)){
        str <- cycle[[i]]
        if(substr(str,1,2) == substr(str,nchar(str)-1,nchar(str))){
          sum <- 0
          for(i in seq(1, nchar(str), by=4)){
            sum <- sum + as.integer(substr(str,i,i+3))
          }
          return(sum);
        }
      }
    }
  }
}

problem_62 <- function(){
  n <- (1:10000)^3
  sorted_cubes <- unlist(lapply(lapply(strsplit(as.character(n), ""), sort), paste, sep="", collapse=""))
  num_perms <- table(sorted_cubes)
  min(which(sorted_cubes %in% names(num_perms[num_perms == 5])))^3
}

problem_63 <- function(){
  n <- 1:9
  sum <- 0
  i <- 1
  while(sum(nchar(as.character(pow.bigz(n,i)))==i) != 0){
    sum <- sum + sum(nchar(as.character(pow.bigz(n,i)))==i)
    i <- i + 1
  }
  sum
}

continued_fraction_expansion <- function(n){
  m <- 0
  d <- 1
  a0 <- floor(sqrt(n))
  a <- a0
  seq <- a
  while(a != 2*a0){
    m <- d*a-m
    d <- (n-m^2)/d
    a <- floor((a0+m)/d)
    seq <- c(seq,a)
  }
  seq
}

problem_64 <- function(){
  n <- 1:10000
  squares <- (1:100)^2
  n <- n[-squares]
  sum <- 0
  for(i in n){
    if((length(continued_fraction_expansion(i))-1)%%2 == 1){
      sum <- sum + 1
    }
  }
  sum
}

problem_65 <- function(){
  require(gmp)
  nums <- rep(as.bigz(0),100)
  nums[0] <- as.bigz("2");
  nums[1] <- as.bigz("3");
  nums[2] <- as.bigz("8");
  k <- as.bigz("4");
  for(i in 3:99){
    if(i%%3 == 2){
      nums[i] <- add.bigz(prod.bigz(nums[i-1],k),nums[i-2])
      k <- add.bigz(k, as.bigz("2"))
    }
    else{
      nums[i] <- add.bigz(nums[i-1], nums[i-2])
    }
  }
  
  sum(as.numeric(unlist(strsplit(as.character(nums[99]),"")))) 
}

minx <- function(n){
  d <- continued_fraction_expansion(n)
  if(length(d) == 2 | length(d) == 3){
    return(d[1]*d[2]+1)
  }
  else if(length(d)%%2==1){
    d <- d[-length(d)]  
  }
  else{
    d <- c(d,d[-c(1,length(d))])
  }
  
  conv <- c(d[length(d)-1]*d[length(d)]+1, d[length(d)])
  for(i in (length(d)-2):1){
    conv <- c(d[i]*conv[1]+conv[2], conv[1])
  }
  return(conv[1])
}

#rewrite
problem_66 <- function(){
  d <- 1:1000
  squares <- (1:floor(sqrt(length(d))))^2
  d <- d[-squares]
  
  x <- c()
  for(i in d){
    x <- c(x,minx(i))
  }
  
  d[which(x==max(x))]
}

problem_67 <- function(){
  setwd("C:/Users/mshay/Documents/R/Project_Euler")
  triangle <- readLines("problem67.txt", warn = FALSE)
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

generate_ring_sums <- function(magic_ring){
  c(sum(magic_ring[1], magic_ring[6], magic_ring[7])
  ,sum(magic_ring[2], magic_ring[7], magic_ring[8])
  ,sum(magic_ring[3], magic_ring[8], magic_ring[9])
  ,sum(magic_ring[4], magic_ring[9], magic_ring[10])
  ,sum(magic_ring[5], magic_ring[10], magic_ring[6]))
}

create_ring_string <- function(magic_ring){
  start <- which.min(magic_ring[1:5])
  if(start != 1){
    outer_order <- c(seq(start, 5, by = 1), seq(1, start-1, by = 1))
    first_inner_order <- c(seq(start+5, 10, by = 1), seq(6, (start+5)-1, by = 1))
    second_inner_order <- first_inner_order+1
    second_inner_order[which(second_inner_order==11)] <- 6
  }
  else{
    outer_order <- 1:5
    first_inner_order <- 6:10
    second_inner_order <- c(7,8,9,10,6)
  }
  magic_ring <- as.character(magic_ring)
  str <- NULL
  for(i in 1:5){
    str <- paste0(str, magic_ring[outer_order[i]], magic_ring[first_inner_order[i]], magic_ring[second_inner_order[i]])
  }
  str
}

problem_68 <- function(){
  nums <- 1:10
  m <- matrix(nums[permutations(10)],ncol=10)
  m <- m[which(m[,1] == 10 | m[,2] == 10 | m[,3] == 10 | m[,4] == 10 | m[,5] == 10),]
  valid_rings <- m[which(apply(m, 1, function(x) {length(unique(generate_ring_sums(x)))==1})),]
  max(apply(valid_rings, 1, create_ring_string))
}

phi_sieve <- function(n){
  phi <- rep(0,n)
  
  for(i in 1:n){ 
    phi[i] <- i
  }
  
  for(i in 2:n){
    if(phi[i] == i){
      for(j in seq(i, n, by = i)){
        phi[j] <- (phi[j]/i) * (i-1)
      }
    }
  }
  
  phi
}

problem_69 <- function(){
  i <- 1:1000000
  phi <- phi_sieve(1000000)
  which.max(i/phi[i])
}

problem_70 <- function(){
  i <- 2:10000000
  phi <- phi_sieve(10000000)[-1]
  df <- as.data.frame(phi)
  df$i <- i
  df$difference <- i - phi
  df$ratio <- i/phi
  df <- df[df$difference > 1,]
  df <- df[order(df$ratio),]
  
  result <- 0
  for(j in 2:nrow(df)){
    if(is_digit_anagram(df[j,1],df[j,2])){
      result <- df[j,2]
      break;
    }
  }
  result
}

gcds <- function(x,y){
  if(y == 0){
    return(x)
  }
  else{
    gcds(y, x%%y)
  }
}

problem_71 <- function(){
  numerator <- 3
  denominator <- 7
  numer <- 0.1
  limd <- 1e6+1
  while(numer != floor(numer)){
    limd <- limd - 1
    numer <- ((numerator * limd) - 1) / denominator
  }
  c(numer, limd) / gcds(numer, limd)
}

problem_72 <- function(){
  phi <- phi_sieve(1000000)[-1]
  sum <- as.bigz(0)
  for(i in 1:length(phi)){
    sum <- add.bigz(sum,phi[i])
  }
  sum
}

problem_73 <- function(){
  fractions <- sapply(5:12000, function(i) (ceiling(i/3):floor(i/2)) / i)
  length(unique(unlist(fractions))) - 2
}

make_chain <- function(n){
  str <- as.character(n)
  sum <- n
  chain <- 0
  chains <- NULL
  while(!(sum %in% chains)){
    chains <- c(chains, sum)
    sum <- sum(factorial(as.integer(unlist(strsplit(str, "")))))
    str <- as.character(sum)
    chain <- chain + 1
  }
  chains
}

problem_74 <- function(){
  #sum(unlist(lapply(1:9999, make_chain))==60)
  all_chain_sizes <- NULL
  for(i in 999999:1){
    if(!is.null(all_chain_sizes[i]) && !is.na(all_chain_sizes[i])){
      next;
    }
    str <- as.character(i)
    sum <- i
    if(sum(factorial(as.integer(unlist(strsplit(str, ""))))) == sum){
      all_chain_sizes[sum] <- 1
      next;
    }
    chain <- 0
    chains <- NULL
    while(!(sum %in% chains)){
      if(!is.null(all_chain_sizes[sum]) && !is.na(all_chain_sizes[sum])){
        break;
      }
      chains <- c(chains, sum)
      sum <- sum(factorial(as.integer(unlist(strsplit(str, "")))))
      str <- as.character(sum)
      chain <- chain + 1
    }
    
    chain_sizes <- length(chains):1
    if(length(which(chains==sum)) != 0){
      chain_sizes <- chain_sizes[-(length(chain_sizes):(which(chains==sum)+1))]
      for(j in 1:length(chain_sizes)){
        all_chain_sizes[chains[j]] <- chain_sizes[j]
      }
    }
    else{
      for(j in 1:length(chain_sizes)){
        all_chain_sizes[chains[j]] <- chain_sizes[j] + all_chain_sizes[sum] 
      }
    }
  }
  
  all_chain_sizes <- all_chain_sizes[1:999999]
  sum(all_chain_sizes==60)
}


problem_75 <- function(){
  values = NULL
  for(i in 1:ceiling(sqrt(1500000/2))){
    for(j in (i+1):ceiling(sqrt(150000/2))){
      if(gcds(i,j) == 1 && (j-i)%%2 == 1){
        a <- j*j - i*i
        b <- 2*i*j
        c <- j*j + i*i
        if(a+b+c < 1500001){
          k <- 1
          sum <- 0
          while(sum < 1500001){
            sum <- k*a+k*b+k*c
            values <- c(values, sum)
            k <- k + 1
          }
        }
      }
    }
  }
  values <- table(values)
  length(names(values[values==1]))
}
#ptm <- proc.time()
#proc.time() - ptm






