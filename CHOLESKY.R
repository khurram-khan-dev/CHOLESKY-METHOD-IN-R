A <- matrix(c(6,15,55,15,55,225,55,225,979),3,3,byrow=T)
B <- matrix(c(152.6,585.6,2488.8),3,1,byrow=T)

l <- matrix(0, nrow=3 , ncol = 3)


n <- nrow(A)

## calculation L11

l[1,1] <- sqrt(A[1,1])

## calculating L21 L31

for(i in 2:n)
{
  l[i,1] <- A[i,1]/l[1,1]
}


## calculating L22 , L32

for(j in 2:n)
{
  l[j,j] <- sqrt(A[j,j] - (l[j,j-1])^2 )
  
  l[(j:n),j] <- (A[j:n,j] - (l[j,j-1]*l[j:n,j-1]))/l[j,j]
  
}

## calculating L33 

l[n,n] <- sqrt(A[n,n] - (l[n,n-2])^2 - (l[n,n-1])^2 )

u <- t(l)
print('lower triangle')
print(l)

print('upper triangle')
print(u)

y = solve(l,B)
print(y)

x = solve(u,y)
print(x)
