# All sorts of permutations

*This repository gathers most of the ideas and case studies presented in a great paper [All Sorts of Permutations (Functional Pearl)](https://www.informatik.uni-kiel.de/~sad/icfp2016-preprint.pdf).*

The code is split into two directories: `src` and `notebooks`.

### `src`
1. `Sorters.hs` contains default implementations for 5 standard sorting algorithms, namely: *insertion sort, selection sort, bubble sort, quick sort, merge sort*
2. `SortersM.hs` contains monadic counterparts
3. `Predicates.hs` contains standard nondeterministic binary boolean predicate and its consistent version

### `notebooks`
1. `RunSorters.ipynb` presents usage of the sorters and a way for testing them
2. `SimpleND.ipynb` presents main the fundamental concept behind the paper
3. `SelectSort.ipynb`, `Quicksort.ipynb` and `BubbleSort.ipynb` present detailed case studies on corresponding algorithms


The notebooks were created thanks to [IHaskell](https://github.com/gibiansky/IHaskell). The `Set` monad was taken from [set-monad](https://hackage.haskell.org/package/set-monad#:~:text=The%20set%2Dmonad%20library%20provides,with%20a%20constrained%20run%20function.) package.