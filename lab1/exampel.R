library(bnlearn)

e <- empty.graph(LETTERS[1:6], num = 1) # num specifies number of objects
class(e)

e

arc.set <- matrix(c("A", "C", "B", "F", "C", "F"), 
                 ncol = 2, byrow = TRUE,
                 dimnames = list(NULL, c("from", "to")))
arc.set

arcs(e) <- arc.set # Node labels must be present in graph and loops A -> A are illegal
e

#arcs(e, ignore.cycles = TRUE) # cycles cannot be introduced to the network unless this set to true

acyclic(e) #Checks whether graph has any cycles

# Undirected graphs can be introduced 




