print("----------------- START ------------------")

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
# Undirected arcs (edges) can be introduced by including both directions (A -> B & B <- A)

edges = matrix(c("A", "B", "B", "A", "C", "D"),
               ncol = 2, byrow = TRUE,
               dimnames = list(NULL, c("from", "to")))

arcs(e) = edges

e

# Arcs kan ocks?? skapas med en bin??r matris d??r 

adj = matrix(0L, ncol = 6, nrow = 6,
             dimnames = list(LETTERS[1:6], LETTERS[1:6]))

adj["A", "C"] = 1L
adj["B", "F"] = 1L
adj["C", "F"] = 1L
adj["D", "E"] = 1L
adj["A", "E"] = 1L
adj

amat(e) = adj

e
  

