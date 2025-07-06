# wolfram

Wolfram is a Haskell project that implements cellular automata based on Wolfram's rules.

## Installation

To install the project, clone the repository and build it using Cabal:

```sh
git clone https://github.com/yourusername/wolfram.git
cd wolfram
make
```

## Testing

To test the program, you can run it with different arguments and verify the output. For example:

```sh
./wolfram --rule 30 --lines 10 --window 80 --move 0
```

This command will run the automaton with rule 30, for 10 lines, with a window width of 80, and no movement.

```sh
                                        #                                       
                                       ###                                      
                                      ##  #                                     
                                     ## ####                                    
                                    ##  #   #                                   
                                   ## #### ###                                  
                                  ##  #    #  #                                 
                                 ## ####  ######                                
                                ##  #   ###     #                               
                               ## #### ##  #   ### 
```