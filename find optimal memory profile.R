#Nature as an Architect: the design of dominance hierarchy
#

rm(list=ls())

# Fix randomize result
set.seed(1) 

# Number of Agents
nAgents  <- 5

# Memory ability of Agents
mAbility <- 1

# Create a nAgents-by-nAgents null matrix
null_matrix = matrix(0,nAgents,nAgents)

# Create the memory profile matrix base
mProfile = null_matrix

# Create the strategy profile matrix base
sProfile = null_matrix

# Digitize the strategies
hawk <- 3
dove <- 1
mix <- 2
strategy_space <- c(hawk,mix)
winner_space <- c(hawk)
clash_counting <- matrix(0,1,nAgents*(nAgents-1)*0.5+1)
#
p <- 1
q <- 1
Count <- 0
supported_count = 0
# loop over all possible memory profiles
# i=j=k=l=m=3
for (i in c(2,3,4,5))
{for (j in c(1,3,4,5)) 
 {for (k in c(1,2,4,5))                 
  {for (l in c(1,2,3,5))
   {for (m in c(1,2,3,4))
    {mProfile[1,i] = 1
     mProfile[2,j] = 1
     mProfile[3,k] = 1
     mProfile[4,l] = 1
     mProfile[5,m] = 1
     # if (!=0 & sum(matches[,mate])==0)
     # print(mProfile)                     # Print memory profile
     # loop over all possible equilibrium strategy profiles
     #space = strategy_space # change strategy_space into winner space to make sure social rule works
     space = winner_space
     
     if (mProfile[1,2] == 1 | mProfile[2,1] == 1){s1space = winner_space} else{s1space = strategy_space}
     if (mProfile[1,3] == 1 | mProfile[3,1] == 1){s2space = winner_space} else{s2space = strategy_space}
     if (mProfile[1,4] == 1 | mProfile[4,1] == 1){s3space = winner_space} else{s3space = strategy_space}
     if (mProfile[1,5] == 1 | mProfile[5,1] == 1){s4space = winner_space} else{s4space = strategy_space}
     if (mProfile[2,3] == 1 | mProfile[3,2] == 1){s5space = winner_space} else{s5space = strategy_space}
     if (mProfile[2,4] == 1 | mProfile[4,2] == 1){s6space = winner_space} else{s6space = strategy_space}
     if (mProfile[2,5] == 1 | mProfile[5,2] == 1){s7space = winner_space} else{s7space = strategy_space}
     if (mProfile[3,4] == 1 | mProfile[4,3] == 1){s8space = winner_space} else{s8space = strategy_space}
     if (mProfile[3,5] == 1 | mProfile[5,3] == 1){s9space = winner_space} else{s9space = strategy_space}
     if (mProfile[4,5] == 1 | mProfile[5,4] == 1){s10space = winner_space} else{s10space = strategy_space}
     
    #  s1space = space
    #  s2space = space
    #  s3space = space
    #  s4space = space
    #  s5space = space
    #  s6space = space
    #  s7space = space
    #  s8space = space
    #  s9space = space
    #  s10space = space
     
     
     for (s1 in s1space) # change strategy_space into winner space to make sure social rule works
     {sProfile[1,2] = s1
     sProfile[2,1] = 4-s1
     for (s2 in s2space)
     {sProfile[1,3] = s2
     sProfile[3,1] = 4-s2
     for (s3 in s3space)
     {sProfile[1,4] = s3
     sProfile[4,1] = 4-s3
     for (s4 in s4space)
     {sProfile[1,5] = s4
     sProfile[5,1] = 4-s4
     for (s5 in s5space)
     {sProfile[2,3] = s5
     sProfile[3,2] = 4-s5
     for (s6 in s6space)
     {sProfile[2,4] = s6
     sProfile[4,2] = 4-s6
     for (s7 in s7space)
     {sProfile[2,5] = s7
     sProfile[5,2] = 4-s7
     for (s8 in s8space)
     {sProfile[3,4] = s8
     sProfile[4,3] = 4-s8
     for (s9 in s9space)
     {sProfile[3,5] = s9
     sProfile[5,3] = 4-s9
     for (s10 in s10space)
     {sProfile[4,5] = s10
     sProfile[5,4] = 4-s10
     Count = Count + 1
     # Testing if the strategy profile can be supported by the memory profile
     check1 = 0
     check2 = 0
     check3 = 0
     check4 = 0
     check5 = 0
     checkall = check1*check2*check3*check4*check5
     # There are 2 situations when the strategy profile is supported by the memory profile
     strategy_list1 = c(sProfile[1,2], sProfile[1,3], sProfile[1,4], sProfile[1,5])
     strategy_list2 = c(sProfile[2,1], sProfile[2,3], sProfile[2,4], sProfile[2,5])
     strategy_list3 = c(sProfile[3,1], sProfile[3,2], sProfile[3,4], sProfile[3,5])
     strategy_list4 = c(sProfile[4,1], sProfile[4,2], sProfile[4,3], sProfile[4,5])
     strategy_list5 = c(sProfile[5,1], sProfile[5,2], sProfile[5,3], sProfile[5,4])
     memory_list1 = c(mProfile[1,2], mProfile[1,3], mProfile[1,4], mProfile[1,5])
     memory_list2 = c(mProfile[2,1], mProfile[2,3], mProfile[2,4], mProfile[2,5])
     memory_list3 = c(mProfile[3,1], mProfile[3,2], mProfile[3,4], mProfile[3,5])
     memory_list4 = c(mProfile[4,1], mProfile[4,2], mProfile[4,3], mProfile[4,5])
     memory_list5 = c(mProfile[5,1], mProfile[5,2], mProfile[5,3], mProfile[5,4])
     #Universal action against all opponents
     if (max(strategy_list1) == min(strategy_list1))
     {check1 = 1} 
     #Universal action against all non-memorized opponents
     if (max(memory_list1[2],memory_list1[3],memory_list1[4]) 
         == min(memory_list1[2],memory_list1[3],memory_list1[4]) &
         max(strategy_list1[2],strategy_list1[3],strategy_list1[4]) 
         == min(strategy_list1[2],strategy_list1[3],strategy_list1[4])
     )
     {check1 = 1} 
     if (max(memory_list1[1],memory_list1[3],memory_list1[4]) 
         == min(memory_list1[1],memory_list1[3],memory_list1[4]) &
         max(strategy_list1[1],strategy_list1[3],strategy_list1[4]) 
         == min(strategy_list1[1],strategy_list1[3],strategy_list1[4])
     )
     {check1 = 1}
     if (max(memory_list1[1],memory_list1[2],memory_list1[4]) 
         == min(memory_list1[1],memory_list1[2],memory_list1[4]) &
         max(strategy_list1[1],strategy_list1[2],strategy_list1[4]) 
         == min(strategy_list1[1],strategy_list1[2],strategy_list1[4])
     )
     {check1 = 1}
     if (max(memory_list1[1],memory_list1[2],memory_list1[3]) 
         == min(memory_list1[1],memory_list1[2],memory_list1[3]) &
         max(strategy_list1[1],strategy_list1[2],strategy_list1[3]) 
         == min(strategy_list1[1],strategy_list1[2],strategy_list1[3])
     )
     {check1 = 1}
     #Repeat the testing for other 4 agents:
     #check2
     if (max(strategy_list2) == min(strategy_list2))
     {check2 = 1} 
     #Universal action against all non-memorized opponents
     if (max(memory_list2[2],memory_list2[3],memory_list2[4]) 
         == min(memory_list2[2],memory_list2[3],memory_list2[4]) &
         max(strategy_list2[2],strategy_list2[3],strategy_list2[4]) 
         == min(strategy_list2[2],strategy_list2[3],strategy_list2[4])
     )
     {check2 = 1} 
     if (max(memory_list2[1],memory_list2[3],memory_list2[4]) 
         == min(memory_list2[1],memory_list2[3],memory_list2[4]) &
         max(strategy_list2[1],strategy_list2[3],strategy_list2[4]) 
         == min(strategy_list2[1],strategy_list2[3],strategy_list2[4])
     )
     {check2 = 1}
     if (max(memory_list2[1],memory_list2[2],memory_list2[4]) 
         == min(memory_list2[1],memory_list2[2],memory_list2[4]) &
         max(strategy_list2[1],strategy_list2[2],strategy_list2[4]) 
         == min(strategy_list2[1],strategy_list2[2],strategy_list2[4])
     )
     {check2 = 1}
     if (max(memory_list2[1],memory_list2[2],memory_list2[3]) 
         == min(memory_list2[1],memory_list2[2],memory_list2[3]) &
         max(strategy_list2[1],strategy_list2[2],strategy_list2[3]) 
         == min(strategy_list2[1],strategy_list2[2],strategy_list2[3])
     )
     {check2 = 1}
     
     #check3
     if (max(strategy_list3) == min(strategy_list3))
     {check3 = 1} 
     #Universal action against all non-memorized opponents
     if (max(memory_list3[2],memory_list3[3],memory_list3[4]) 
         == min(memory_list3[2],memory_list3[3],memory_list3[4]) &
         max(strategy_list3[2],strategy_list3[3],strategy_list3[4]) 
         == min(strategy_list3[2],strategy_list3[3],strategy_list3[4])
     )
     {check3 = 1} 
     if (max(memory_list3[1],memory_list3[3],memory_list3[4]) 
         == min(memory_list3[1],memory_list3[3],memory_list3[4]) &
         max(strategy_list3[1],strategy_list3[3],strategy_list3[4]) 
         == min(strategy_list3[1],strategy_list3[3],strategy_list3[4])
     )
     {check3 = 1}
     if (max(memory_list3[1],memory_list3[2],memory_list3[4]) 
         == min(memory_list3[1],memory_list3[2],memory_list3[4]) &
         max(strategy_list3[1],strategy_list3[2],strategy_list3[4]) 
         == min(strategy_list3[1],strategy_list3[2],strategy_list3[4])
     )
     {check3 = 1}
     if (max(memory_list3[1],memory_list3[2],memory_list3[3]) 
         == min(memory_list3[1],memory_list3[2],memory_list3[3]) &
         max(strategy_list3[1],strategy_list3[2],strategy_list3[3]) 
         == min(strategy_list3[1],strategy_list3[2],strategy_list3[3])
     )
     {check3 = 1}
     
     #check4
     if (max(strategy_list4) == min(strategy_list4))
     {check4 = 1} 
     #Universal action against all non-memorized opponents
     if (max(memory_list4[2],memory_list4[3],memory_list4[4]) 
         == min(memory_list4[2],memory_list4[3],memory_list4[4]) &
         max(strategy_list4[2],strategy_list4[3],strategy_list4[4]) 
         == min(strategy_list4[2],strategy_list4[3],strategy_list4[4])
     )
     {check4 = 1} 
     if (max(memory_list4[1],memory_list4[3],memory_list4[4]) 
         == min(memory_list4[1],memory_list4[3],memory_list4[4]) &
         max(strategy_list4[1],strategy_list4[3],strategy_list4[4]) 
         == min(strategy_list4[1],strategy_list4[3],strategy_list4[4])
     )
     {check4 = 1}
     if (max(memory_list4[1],memory_list4[2],memory_list4[4]) 
         == min(memory_list4[1],memory_list4[2],memory_list4[4]) &
         max(strategy_list4[1],strategy_list4[2],strategy_list4[4]) 
         == min(strategy_list4[1],strategy_list4[2],strategy_list4[4])
     )
     {check4 = 1}
     if (max(memory_list4[1],memory_list4[2],memory_list4[3]) 
         == min(memory_list4[1],memory_list4[2],memory_list4[3]) &
         max(strategy_list4[1],strategy_list4[2],strategy_list4[3]) 
         == min(strategy_list4[1],strategy_list4[2],strategy_list4[3])
     )
     {check4 = 1}
     
     #check5
     if (max(strategy_list5) == min(strategy_list5))
     {check5 = 1} 
     #Universal action against all non-memorized opponents
     if (max(memory_list5[2],memory_list5[3],memory_list5[4]) 
         == min(memory_list5[2],memory_list5[3],memory_list5[4]) &
         max(strategy_list5[2],strategy_list5[3],strategy_list5[4]) 
         == min(strategy_list5[2],strategy_list5[3],strategy_list5[4])
     )
     {check5 = 1} 
     if (max(memory_list5[1],memory_list5[3],memory_list5[4]) 
         == min(memory_list5[1],memory_list5[3],memory_list5[4]) &
         max(strategy_list5[1],strategy_list5[3],strategy_list5[4]) 
         == min(strategy_list5[1],strategy_list5[3],strategy_list5[4])
     )
     {check5 = 1}
     if (max(memory_list5[1],memory_list5[2],memory_list5[4]) 
         == min(memory_list5[1],memory_list5[2],memory_list5[4]) &
         max(strategy_list5[1],strategy_list5[2],strategy_list5[4]) 
         == min(strategy_list5[1],strategy_list5[2],strategy_list5[4])
     )
     {check5 = 1}
     if (max(memory_list5[1],memory_list5[2],memory_list5[3]) 
         == min(memory_list5[1],memory_list5[2],memory_list5[3]) &
         max(strategy_list5[1],strategy_list5[2],strategy_list5[3]) 
         == min(strategy_list5[1],strategy_list5[2],strategy_list5[3])
     )
     {check5 = 1}
     checkall = check1*check2*check3*check4*check5
     if (checkall == 1)
     {supported_count = supported_count +1
     # print(supported_count)
     nclash = sum(sProfile == 2)/2
     for (happened_clash in 0:10) {
       if (nclash == happened_clash)
       {clash_counting[happened_clash+1]=clash_counting[happened_clash+1]+1}
     }
     if (nclash ==6){message(sProfile, " is supported by ", mProfile, " with ", nclash, " clashes ")}
     # message(sProfile, " is supported by ", mProfile, " with ", nclash, " clashes ")
     }
     
     }}}}}}}}}}
     mProfile = null_matrix # Reset memory profile 
     #sProfile = null_matrix # Reset stategy profile 
     Count = Count + 1
    }
   }
  }
 }
}
#####################################################################################
#####################################################################################
#####################################################################################
sum <- 0
for (i in col(clash_counting)) {sum = sum + (i-1)*clash_counting[i] 
}
mean = sum/sum(clash_counting)
mean #Expected number of clash

#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#Nature as an Architect: the design of dominance hierarchy
#

rm(list=ls())

# Fix randomize result
set.seed(1) 

# Number of Agents
nAgents  <- 5

# Memory ability of Agents
mAbility <- 2

# Create a nAgents-by-nAgents null matrix

null_matrix = matrix(0,nAgents,nAgents)

# Create the memory profile matrix base
mProfile = null_matrix

# Create the strategy profile matrix base
sProfile = null_matrix

# Digitize the strategies
hawk <- 3
dove <- 1
mix <- 2
strategy_space <- c(hawk,mix,dove)
winner_space <- c(hawk,mix)
only_space <- c(hawk)
clash_counting <- matrix(0,1,nAgents*(nAgents-1)*0.5+1)
#
p <- 1
q <- 1
Count <- 0
supported_count = 0
# loop over all possible memory profiles
# i=j=k=l=m=3
for (i1 in c(2,3,4,5)) for (i2 in c(2,3,4,5))
{for (j1 in c(1,3,4,5)) for (j2 in c(1,3,4,5)) 
{for (k1 in c(1,2,4,5)) for (k2 in c(1,2,4,5))               
{for (l1 in c(1,2,3,5)) for (l2 in c(1,2,3,5))
{for (m1 in c(1,2,3,4)) for (m2 in c(1,2,3,4))
{if (i1<i2){mProfile[1,i1] = 1
            mProfile[1,i2] = 1}
 if (j1<j2){mProfile[2,j1] = 1
            mProfile[2,j2] = 1}
 if (k1<k2){mProfile[3,k1] = 1
            mProfile[3,k2] = 1}
 if (l1<l2){mProfile[4,l1] = 1
            mProfile[4,l2] = 1}
 if (m1<m2){mProfile[5,m1] = 1
            mProfile[5,m2] = 1}
  if (sum(mProfile)==10){
# if (!=0 & sum(matches[,mate])==0)
# print(mProfile)                     # Print memory profile
# loop over all possible equilibrium strategy profiles
# space = strategy_space # change strategy_space into winner space to make sure social rule works
space = winner_space
for (s1 in space) # change strategy_space into winner space to make sure social rule works
{sProfile[1,2] = s1
sProfile[2,1] = 4-s1
for (s2 in space)
{sProfile[1,3] = s2
sProfile[3,1] = 4-s2
for (s3 in space)
{sProfile[1,4] = s3
sProfile[4,1] = 4-s3
for (s4 in space)
{sProfile[1,5] = s4
sProfile[5,1] = 4-s4
for (s5 in space)
{sProfile[2,3] = s5
sProfile[3,2] = 4-s5
for (s6 in space)
{sProfile[2,4] = s6
sProfile[4,2] = 4-s6
for (s7 in space)
{sProfile[2,5] = s7
sProfile[5,2] = 4-s7
for (s8 in space)
{sProfile[3,4] = s8
sProfile[4,3] = 4-s8
for (s9 in space)
{sProfile[3,5] = s9
sProfile[5,3] = 4-s9
for (s10 in space)
{sProfile[4,5] = s10
sProfile[5,4] = 4-s10
Count = Count + 1
# Testing if the strategy profile can be supported by the memory profile
check1 = 0
check2 = 0
check3 = 0
check4 = 0
check5 = 0
checkall = check1*check2*check3*check4*check5
# There are 2 situations when the strategy profile is supported by the memory profile
strategy_list1 = c(sProfile[1,2], sProfile[1,3], sProfile[1,4], sProfile[1,5])
strategy_list2 = c(sProfile[2,1], sProfile[2,3], sProfile[2,4], sProfile[2,5])
strategy_list3 = c(sProfile[3,1], sProfile[3,2], sProfile[3,4], sProfile[3,5])
strategy_list4 = c(sProfile[4,1], sProfile[4,2], sProfile[4,3], sProfile[4,5])
strategy_list5 = c(sProfile[5,1], sProfile[5,2], sProfile[5,3], sProfile[5,4])
memory_list1 = c(mProfile[1,2], mProfile[1,3], mProfile[1,4], mProfile[1,5])
memory_list2 = c(mProfile[2,1], mProfile[2,3], mProfile[2,4], mProfile[2,5])
memory_list3 = c(mProfile[3,1], mProfile[3,2], mProfile[3,4], mProfile[3,5])
memory_list4 = c(mProfile[4,1], mProfile[4,2], mProfile[4,3], mProfile[4,5])
memory_list5 = c(mProfile[5,1], mProfile[5,2], mProfile[5,3], mProfile[5,4])
#Universal action against all opponents
if (max(strategy_list1) == min(strategy_list1))
{check1 = 1} 
#Universal action against all non-memorized opponents
if (memory_list1[3] == memory_list1[4] &
    strategy_list1[3] == strategy_list1[4] 
)
{check1 = 1} 
if (memory_list1[2] == memory_list1[4] &
    strategy_list1[2] == strategy_list1[4] 
)
{check1 = 1}
if (memory_list1[2] == memory_list1[3] &
    strategy_list1[2] == strategy_list1[3] 
)
{check1 = 1}
if (memory_list1[1] == memory_list1[4] &
     strategy_list1[1] == strategy_list1[4]
)
{check1 = 1}
if (memory_list1[1] == memory_list1[3] &
     strategy_list1[1] == strategy_list1[3]
)
{check1 = 1}
if (memory_list1[1] == memory_list1[2] &
     strategy_list1[1] == strategy_list1[2]
)
{check1 = 1}
#Repeat the testing for other 4 agents:
#check2
if (max(strategy_list2) == min(strategy_list2))
{check2 = 1} 
#Universal action against all non-memorized opponents
if (memory_list2[3] == memory_list2[4] &
    strategy_list2[3] == strategy_list2[4] 
)
{check2 = 1} 
if (memory_list2[2] == memory_list2[4] &
    strategy_list2[2] == strategy_list2[4] 
)
{check2 = 1}
if (memory_list2[2] == memory_list2[3] &
    strategy_list2[2] == strategy_list2[3] 
)
{check2 = 1}
if (memory_list2[1] == memory_list2[4] &
     strategy_list2[1] == strategy_list2[4]
)
{check2 = 1}
if (memory_list2[1] == memory_list2[3] &
     strategy_list2[1] == strategy_list2[3]
)
{check2 = 1}
if (memory_list2[1] == memory_list2[2] &
     strategy_list2[1] == strategy_list2[2]
)
{check2 = 1}
#check3
if (max(strategy_list3) == min(strategy_list3))
{check3 = 1} 
#Universal action against all non-memorized opponents
if (memory_list3[3] == memory_list3[4] &
    strategy_list3[3] == strategy_list3[4] 
)
{check3 = 1} 
if (memory_list3[2] == memory_list3[4] &
    strategy_list3[2] == strategy_list3[4] 
)
{check3 = 1}
if (memory_list3[2] == memory_list3[3] &
    strategy_list3[2] == strategy_list3[3] 
)
{check3 = 1}
if (memory_list3[1] == memory_list3[4] &
    strategy_list3[1] == strategy_list3[4]
)
{check3 = 1}
if (memory_list3[1] == memory_list3[3] &
    strategy_list3[1] == strategy_list3[3]
)
{check3 = 1}
if (memory_list3[1] == memory_list3[2] &
    strategy_list3[1] == strategy_list3[2]
)
{check3 = 1}

#check4
if (max(strategy_list4) == min(strategy_list4))
{check4 = 1} 
#Universal action against all non-memorized opponents
if (memory_list4[3] == memory_list4[4] &
    strategy_list4[3] == strategy_list4[4] 
)
{check4 = 1} 
if (memory_list4[2] == memory_list4[4] &
    strategy_list4[2] == strategy_list4[4] 
)
{check4 = 1}
if (memory_list4[2] == memory_list4[3] &
    strategy_list4[2] == strategy_list4[3] 
)
{check4 = 1}
if (memory_list4[1] == memory_list4[4] &
    strategy_list4[1] == strategy_list4[4]
)
{check4 = 1}
if (memory_list4[1] == memory_list4[3] &
    strategy_list4[1] == strategy_list4[3]
)
{check4 = 1}
if (memory_list4[1] == memory_list4[2] &
    strategy_list4[1] == strategy_list4[2]
)
{check4 = 1}


#check5
if (max(strategy_list5) == min(strategy_list5))
{check5 = 1} 
#Universal action against all non-memorized opponents
if (memory_list5[3] == memory_list5[4] &
    strategy_list5[3] == strategy_list5[4] 
)
{check5 = 1} 
if (memory_list5[2] == memory_list5[4] &
    strategy_list5[2] == strategy_list5[4] 
)
{check5 = 1}
if (memory_list5[2] == memory_list5[3] &
    strategy_list5[2] == strategy_list5[3] 
)
{check5 = 1}
if (memory_list5[1] == memory_list5[4] &
    strategy_list5[1] == strategy_list5[4]
)
{check5 = 1}
if (memory_list5[1] == memory_list5[3] &
    strategy_list5[1] == strategy_list5[3]
)
{check5 = 1}
if (memory_list5[1] == memory_list5[2] &
    strategy_list5[1] == strategy_list5[2]
)
{check5 = 1}
checkall = check1*check2*check3*check4*check5
if (checkall == 1)
{supported_count = supported_count +1
# print(supported_count)
nclash = sum(sProfile == 2)/2
for (happened_clash in 0:10) {
  if (nclash == happened_clash)
  {clash_counting[happened_clash+1]=clash_counting[happened_clash+1]+1}
}
# if (nclash ==0){message(sProfile, " is supported by ", mProfile, " with ", nclash, " clashes ")}
# message(sProfile, " is supported by ", mProfile, " with ", nclash, " clashes ")
}

}}}}}}}}}}}
mProfile = null_matrix # Reset memory profile 
#sProfile = null_matrix # Reset stategy profile 
Count = Count + 1
}
}
}
}
}