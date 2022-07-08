# broccoli

Broccoli is a simple, malleable programming language designed to be broken! This language is meant to be used as a base to test and play with implementing programming language features in a consistent environment.

To achieve this, Broccoli is easy to manipulate, and is designed to be as broad as possible!

Some ideas for features to implement into Broccoli are:

- Time complexity prediction
- Different forms of asynchronous code
- Dynamic optimisations based off marked functional and non-functional code
- Impurity tracking and verbose reporting



## Getting started with Broccoli
You will be required to have cabal installed. Check out https://www.haskell.org/ghcup/ for more info.

Simply clone the repo and run cabal install:
```
git clone https://github.com/Sam-Hobson/broccoli.git
cabal install
```

To execute a file, use:
```
cabal run :all "*FILENAME*"
```



## TODO:
- Implement hooks in language functionality
- Create compiler
