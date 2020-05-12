# ring-of-fire

A genetic program for evolving rules that describe a cellular automaton-based wildfire simulation using Clojure and based on the [propel framework](https://github.com/lspector/propel) designed by Lee Spector. Created by [Isaac Caruso](https://github.com/icaruso21), [Oliver Baldwin Edwards](https://github.com/Oliver-BE), and [Martin Glusker](https://github.com/mglusker). Special thanks to [Lee Spector](https://github.com/lspector) for his invaluable assistance and advice.

## Usage

To run, clone the repository, navigate to the `src/ring-of-fire` directory and run command: <br />
`lein run`<br />
To interact with a repl: <br />
`lein repl`

Note that `lein run` will run our genetic program with the following default arguments:

---
 :instructions            fire-instructions
 :error-function          fire-error-function
 :max-generations         5000
 :population-size         10
 :max-initial-plushy-size 40
 :parent-selection        :lexicase
 :tournament-size         5
 :fire-selection          2
 :time-step               3
 :step-limit              100
---

To copy all of the folders in a directory to another directory but not the files (useful for creating a filesystem for condor job outputs): 
`rsync -a -f"+ */" -f"- *" /directory-to-copy/* /new-directory-name/`

## Necessary input data
*we need to add what kind of new data input could be placed in here, i.e. what the structure of the data that we used is*

## Example Outputs
*include screenshots here*
*also include how to take a rule generated from ring-of-fire and use that to run our quil program and output the grid*

## Dependencies

Requires Java SDK 1.8 and Leiningen. 

## License

Copyright © 2020 


