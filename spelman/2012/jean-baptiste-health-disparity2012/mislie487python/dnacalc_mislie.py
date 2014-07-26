#! /usr/bin/env python

# 2012 Sep 26, updated to python 3.2 by Hong Qin

DNASeq = "ATGAAC"
print ('Sequence:', DNASeq )

SeqLength = float(len(DNASeq))

print ("Sequence Length:", SeqLength)

Base = "ACGT"
Number = {}

#count A, C, G, T in the DNA sequence
for Base in DNASeq:
    Number[Base]= DNASeq.count(Base) / SeqLength
for Base in Number:
    print (Base, ": ", format(Number[Base], "4.1f"))

