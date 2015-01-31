#! /usr/bin/env python

# 2012 Sep 26, updated to python 3.2 by Hong Qin

DNASeq = "ATGAAC"
print ('Sequence:', DNASeq )

SeqLength = float(len(DNASeq))

print ("Sequence Length:", SeqLength)

NumberA = DNASeq.count('A')
NumberC = DNASeq.count('C')
NumberG = DNASeq.count('G')
NumberT = DNASeq.count('T')

print ("A:", NumberA/SeqLength)
print ("C:", NumberC/SeqLength)
print ("G:", NumberG/SeqLength)
print ("T:", NumberT/SeqLength)
