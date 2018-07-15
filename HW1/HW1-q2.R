# First we will define our vectors and check the second condition:
v <- c(1:13)
Q = permutations(n=13,r=6,v=v,repeats.allowed = FALSE) # only checking condition 2
G = length(Q)/6
y = integer(G)

for ( i in 1:G)
{
  A = Q[i,2]/(Q[i,2]+Q[i,3])
  B = (Q[i,2]+Q[i,3])/(Q[i,3]+Q[i,4]+Q[i,5])
  C = (Q[i,3]+Q[i,4]+Q[i,5])/(Q[i,1]+Q[i,5]+Q[i,6])
  # Q[i,1] = a5 , Q[i,6] = a10
  
  if( A == B && A ==C && B==C && A<1)
  {
    y[i] = i
  }
}  
#---------------------------------

# all the i s that give our condition:
y <- y[y!=0]

# our new matrix:
check2 = Q[y,]
rows = nrow(check2)

#---------------------------------

# now we will use these to check other conditions:
m <- c(1:13)
H = permutations(7)
rows2 = nrow(H)
for(i in 1:rows)
{
  check = m[ !m %in%  check2[i,]]
  for (j in 1:rows2)
  {
    C = c( (check[H[j,1:4]]), check2[i,] , check[H[j,5:7]] )
    A1 = C[1]/(C[2]+C[3])
    B1 = (C[2]+C[3])/(C[3]+C[4]+C[5])
    
    A3 = (C[11]+C[12])/(C[12]+C[13])
    B3 = (C[12]+C[13])/(C[13]+C[10])
    
    if (A1 == B1 && A1<1 && A3 == B3 && A3<1)
    {
      print(C)
      break
    
    }
    
  }
  
}
# [1]  6  1 11  3 10  8  4  9  5 12  7  2 13
#---------------------------------
