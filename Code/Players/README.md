## Players and Strategies 

- `player.rkt` implements the mechanical part of a player, the
  "intelligent" part goes into strategies 

- `strategies.rkt` defines a strategy contract and a base strategy (with a
  natural initial placement tactic) 

- `first-s,rkt` picks a deterministic possibly illegal action for turns 

- `second-s,rkt` picks a deterministic, ideally legal action for turns,
  uses an illegal action if required 



```
+------------------------+                  +-----------------------------+
| strategies: strategy/c |<-----------------| strategies: base-strategy/c |
+------------------------+                  +-----------------------------+
                                                          ^
                                                          |
                                               -----------*-----------
                                               |                     |
                                   +--------------------------+    +----------------------------+
                                   | first-s : first-strategy |    | second-s : second-strategy |
                                   +--------------------------+    +----------------------------+
```
