# ring-of-fire

A parallelized genetic program for evolving rules that describe a cellular automaton-based wildfire simulation using Clojure and based on the [propel framework](https://github.com/lspector/propel) designed by [Lee Spector](https://github.com/lspector). Created by [Isaac Caruso](https://github.com/icaruso21), [Oliver Baldwin Edwards](https://github.com/Oliver-BE), and [Martin Glusker](https://github.com/mglusker). Special thanks to Lee Spector for his invaluable assistance and advice.

## Usage

**To run:**
 * Clone  repository
 * Navigate to  `src/ring-of-fire` and run command: `lein run`

    * Note that `lein run` will run the genetic program with the following default arguments:

    ```clojure
    {:instructions            fire-instructions
     :error-function          fire-error-function
     :max-generations         5000
     :population-size         10
     :max-initial-plushy-size 40
     :parent-selection        :lexicase
     :tournament-size         5
     :fire-selection          2
     :time-step               3
     :step-limit              100}
    ```

 * To run the genetic program with user-defined arguments, add key-value pairs as arguments after `lein run` 

 * Example usage: `lein run :population-size 20 :parent-selection :tournament :max-generations 1000` 



## Data

--- 
test: asdf
poop: stinky 
---
## Example Outputs
*include screenshots here*
*also include how to take a rule generated from ring-of-fire and use that to run our quil program and output the grid*

## Dependencies

Requires Java SDK 1.8 and Leiningen. 

## License

Copyright © 2020 


