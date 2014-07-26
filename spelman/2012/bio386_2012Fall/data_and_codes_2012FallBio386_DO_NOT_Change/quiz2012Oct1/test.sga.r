# You are given the genetic interaction data in pairwise format, provided by Costanzo 2009 Science.
# Please use the following codes to load this genetic interaction network into R. 

geneticPairsTb = read.delim("sgadata_costanzo2009_stringentCutoff_101120.txt", header=F,)
names(geneticPairsTb) = c("ORF1", "name1", "ORF2", "name2", NA, NA, NA)

# Now, please calculate the number of interactions per gene.


# Please feel free to carry out more analysis as in our inclass analysis of Fraser02 Science paper. 
# 

