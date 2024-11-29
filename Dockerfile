# Step 1: Use an official Haskell image as the base image
FROM haskell:latest

# Step 2: Set the working directory inside the container
WORKDIR /app

# Step 3: Install Stack (if not already available in the image)
RUN apt-get update && apt-get install -y curl && curl -sSL https://get.haskellstack.org/ | sh

# Step 4: Copy the project files (including stack.yaml and source code)
COPY . /app

# Step 5: Install dependencies and build the project
RUN stack setup && stack build

# Step 6: Set the default command to run the application
CMD ["stack", "exec", "haskell-parser-exe"]
