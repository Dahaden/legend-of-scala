## ![Logo](/tools/art/splash.png) The Legend of Scala

_A (sort of) educational game for learning Scala._

# Overview

_Legend of Scala_ is a mildly educational video game for learning Scala. The
code is mildly terrible (it's my first time using Scala so please be gentle).

## Running the Server

Play Framework needs to be installed.

1. Run `play run` in the server directory.

2. Go to the website address given in the console and follow the instructions
   to evolve the database.

3. Stop `play run`, and open `play console` to run
   `:load scripts/generate_feature.scala`.

4. `play run`

## Running the Client

SBT is bundled with the repo.

1. Set the environment variable `LOS_HOST` to the server's address.

2. Run `./start.sh` in the client directory.
