# Quick repo to test large dependency trees with spago@next

## To run

`spago run -p build` will generate 100 packages, where the first 10 packages have no local dependencies, and the other 90 depend on all 10 of the first.

The total number of packages can be changed by upping `max` in `build/src/Main.purs`.
The dependency setup for each package can be swapped by swapping the commented block in `dependencies` in the same file.
