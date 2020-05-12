# ring-of-fire  <img src="logo.png" width="60" height="60"/>



A parallelized genetic program for evolving rules that describe a cellular automaton-based wildfire simulation using Clojure and based on the [propel framework](https://github.com/lspector/propel) designed by [Lee Spector](https://github.com/lspector). 

Created by [Isaac Caruso](https://github.com/icaruso21), [Oliver Baldwin Edwards](https://github.com/Oliver-BE), and [Martin Glusker](https://github.com/mglusker). 
Special thanks to Lee Spector for his invaluable assistance and advice. 
Project inspired by and data sourced from [Cell2Fire](https://github.com/cell2fire/Cell2Fire).

## Usage

**To run:**
 * Clone repository
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

We sourced our data from [Cell2Fire](https://github.com/cell2fire/Cell2Fire). 
The data was used to both specify the parameters for instructions in our genetic program (such as the weather data) and to 
calculate the error in our results (by using the actual final fire scar).
Ten fires were provided, each with data of the following type:

| Data File                | Description   |
| ------------------------ | ------------- |
| `Forest.asc`             | A grid composed of numeric values that identify what type terrain occupies each cell. Use `fbp_lookup_table` as a lookup for each value  |     
| `fbp_lookup_table.csv`   | A lookup table to take cell values from `Forest.asc` and translate them into fuel types (includes descriptive names such as "Boreal Sprice" as well)             |
| `Weather.csv`            | A table including hourly data from start to finish with variables such as wind speed (`WS`) and fire weather index (`FWI`)             |
| `FinalScarGrid.csv`      | A grid displaying the final state of the fully-burned fire, where cells marked 1 were those that burned and cells marked 0 were those that did not            |
| `elevation.asc`          | A grid containing elevation values (altitude in meters of the current cell with respect to the sea level)            |
| `slope.asc`              | A grid containing slope values (the slope percentage in terms of the vertical rise over the horizontal run and adjacent cells)         |
| `IgnitionPoints.csv`     | Contains the value of the ignition cell (note that this cell value iis 1-indexed)         |

## Example Outputs
*include screenshots here*
*also include how to take a rule generated from ring-of-fire and use that to run our quil program and output the grid*

## Dependencies

Requires Java SDK 1.8 and Leiningen. 

## License

* This software is intended for research use. 
* <div>Icons made by <a href="https://www.flaticon.com/authors/smashicons" title="Smashicons">Smashicons</a> from <a href="https://www.flaticon.com/" title="Flaticon">www.flaticon.com</a></div>


