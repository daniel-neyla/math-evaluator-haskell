# Use the official Haskell image
FROM haskell:8.10

# Set the working directory inside the container
WORKDIR /opt/math-evaluator-haskell

# Update cabal package list
RUN cabal update

# Copy the .cabal file first to install dependencies without the source code
COPY ./math-evaluator-haskell.cabal /opt/math-evaluator-haskell/math-evaluator-haskell.cabal

# Install the dependencies (this will be cached unless the .cabal file changes)
RUN cabal build --only-dependencies -j4

# Copy the source code
COPY . /opt/math-evaluator-haskell

# Install the application (this builds the project and installs it)
RUN cabal install

# Set the default command to run your application
CMD ["math-evaluator-haskell"]


# Run this for interactive docker session: docker run -it -v "$(pwd):/opt/math-evaluator-haskell" haskell-parser bash
