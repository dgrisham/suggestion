Suggestion
==========

A Haskell program whose ultimate goal is to choose a movie to watch.

Implemented functionality:

1.  Add a movie and its metadata (found using a slightly-modified imdbtool) to
    the 'movies' file, e.g.
    
    ```sh
    suggestion-exe add 'the truman show'
    ```

2.  Get a random movie suggestion with optional filters on metadata. Examples:
    
    ```sh
    # random movie from all in file
    suggestion-exe suggestion
    # random Jim Carrey comedy OR action movie
    suggestion-exe s 'genre: comedy; actors: jim carrey' 'genre: action'
    ```

