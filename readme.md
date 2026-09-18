# Rozhodovací stromy
Name: Adam Kučík  
Login: xkucik00  

Everything from the project assigment should be working - basic tests passed. 

Project was done with ghc 9.4.8. The compilation requires the Split library, which is allowed by the assigment. The project was tested even on merlin, but the path to the library needs to be set.  

## First task
Decision tree and classification. Execution:  
`flp-fun -1 <soubor obsahujici strom> <soubor obsahujici nove data>`

For example tree file on input:
```
Node: 0, 5.5
  Leaf: TridaA
  Node: 1, 3.0
    Leaf: TridaB
    Leaf: TridaC
```

And example data file on input:
```
2.4, 1.3
6.1, 0.3
6.3, 4.4
```

The output is:
```
TridaA
TridaB
TridaC
```

## Second task
Creation of decision tree. Execution:  
`flp-fun -2  <soubor obsahujici trenovaci data>`

For example training data file on input:  
```
2.4, 1.3, TridaA
6.1, 0.3, TridaB
6.3, 4.4, TridaC
2.9, 4.4, TridaA
3.1, 2.9, TridaB
```

The output is:
```
Node: 0, 3.0
  Leaf: TridaA
  Node: 0, 6.199999999999999
    Leaf: TridaB
    Leaf: TridaC
```