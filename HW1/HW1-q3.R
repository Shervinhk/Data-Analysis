# First Condition with 16 people:


c = integer(16)
# 0 : liar , 1 : frank

j = integer(1)
while(1)
{
  check = c
  for(i in 1:16)
  {
if( c[i] == 0 && c[i-1]+c[i+1] == 0 && i !=1 && i !=16 ) # if liar was frank
{
  c[i] = 1
}
  
if ( c[i] == 0 && c[i-1]+c[i+1] > 0 && i !=1 && i !=16) # if liar was liar
{
  c[i] = 0
}

if (  c[i] == 1 && c[i-1]+c[i+1] == 0 && i !=1 && i !=16) # if frank was frank
{
  c[i] = 1
}

if(  c[i] == 1 && c[i-1]+c[i+1] > 0 && i !=1 && i !=16) # if frank was liar
{
  c[i] = 0
}

    if( i ==1 )
    {
      if( c[1] ==0 && c[16] + c[2] == 0)
      {
        c[1] = 1
      }
      if( c[1] ==0 && c[16] + c[2] > 0)
      {
        c[1] = 0
      }
      if( c[1] ==1 && c[16] + c[2] == 0)
      {
        c[1] = 1
      }
      if( c[1] ==1 && c[16] + c[2] > 0)
      {
        c[1] = 0
      }
            
    }
    
    
    if( i ==16 )
    {
      if( c[16] ==0 && c[15] + c[1] == 0)
      {
        c[16] = 1
      }
      if( c[16] ==0 && c[15] + c[1] > 0)
      {
        c[16] = 0
      }
      if( c[16] ==1 && c[15] + c[1] == 0)
      {
        c[16] = 1
      }
      if( c[16] ==1 && c[15] + c[1] > 0)
      {
        c[16] = 0
      }
      
    }
    
    
  }
  if(c-check || 0)
  {
    Answer1 = c
    frank = sum(Answer1)
    break
  }
}




check = integer(12)
c = integer(12)
j = integer(1)

while(j<10000)
{
  check = c
  for(i in 1:12)
  {
    if( c[i] == 0 && c[i-1]+c[i+1]  == 1 && i !=1 && i !=12 ) # if liar was frank
    {
      c[i] = 1
    }
    
    if ( c[i] == 0 && (c[i-1]+c[i+1] !=1) && i !=1 && i !=12) # if liar was liar
    {
      c[i] = 0
    }
    
    if (  c[i] == 1 && c[i-1]+c[i+1] == 1 && i !=1 && i !=12) # if frank was frank
    {
      c[i] = 1
    }
    
    if(  c[i] == 1 && c[i-1]+c[i+1] != 1 && i !=1 && i !=12) # if frank was liar
    {
      c[i] = 0
    }
    
    if( i ==1 )
    {
      if( c[1] ==0 && c[12] + c[2] == 1)
      {
        c[1] = 1
      }
      if( c[1] ==0 && c[12] + c[2] != 1)
      {
        c[1] = 0
      }
      if( c[1] ==1 && c[12] + c[2] == 1)
      {
        c[1] = 1
      }
      if( c[1] ==1 && c[12] + c[2] != 1)
      {
        c[1] = 0
      }
    }
      
      if( i ==12 )
      {
        if( c[12] ==0 && c[11] + c[1] == 1)
        {
          c[12] = 1
        }
        if( c[12] ==0 && c[11] + c[1] != 1)
        {
          c[12] = 0
        }
        if( c[12] ==1 && c[11] + c[1] == 1)
        {
          c[12] = 1
        }
        if( c[12] ==1 && c[11] + c[1] != 1)
        {
          c[12] = 0
        }
      
    }
    
    
  }

  j = j+1
}
Answer2 = c
frank2 = sum(Answer2)


liars = integer(1)

for (i in 1:2^8){
  for(k in 1:2^8){
    if (sum(x[k,]) == 4)
    {
      check2 = x[k,]
      check = x[i,]
      add = 0
      check3 = which(check == 1)
      if (add == 0)
      {
        if (length(check3) != 0)
        {
          for (j in 1:length(check3))
          {
            number.ofcheck = ( check3[j]-2 ):check3[j]
            
            if (sum(check[number.ofcheck%%n+1]) == 2 & check2[check3[j]] == 1 )
            {
              add = add + 1
              break
            }
            if (sum(check[number.ofcheck%%n+1]) == 3 & check2[check3[j]] == 0 ) {
              add = add + 1
              break
            }
          }
        }
      }
      
      check3 = which(check == 0)
      
      if (length(check3) != 0)
      {
        for (j in 1:length(check3))
        {
          number.ofcheck = (check3[j]-2):check3[j]
          if (sum(check[number.ofcheck%%n+1]) != 1 & check2[check3[j]] == 1 ) 
            
          {
            add = add + 1
            break
          }
          if (sum(check[number.ofcheck%%n+1]) != 2 & check2[check3[j]] == 0 )
          {
            add = add + 1
            break
          }
        }
      }
      
      if (add == 0)
      {
        liars = c(liars,sum(check))
        Answer3 = liars
      }
    }
  }
}


