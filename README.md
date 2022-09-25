# Tokie

tokie is a simple webapp written in F# and compiled to JavaScript (using Fable) for decoding and validating JWT tokens.

For now tokie is in a somewhat alpha state.

## Setup dev environment

tokie can be developed using every F# IDE, but for Visual Studio Code a devcontainer has been provided. To develop tokie locally first run `npm install` or `npm ci && npm run postinstall`.

To run tokie locally the command `npm run start`. To build tokie run the command `npm run build`. To build the Docker container run the command `npm run build-container`. After that tokie can be run using Docker via the command `npm run run-container.
