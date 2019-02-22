### Development Highlights

* **Test First** – This project was written predominantly with a test-first approach. Write a failing test, write minimal code to make that test pass, and then do any desired refactorings because the tests provide confidence.

* **Functional Core, Imperative Shell** – Keep the parts on the "inside" of the program as pure and immutable as possible and only use impure, side-effecting procedures at the "boundaries".

* **Live Coding** – At least part of this project was streamed on [twitch](https://www.twitch.tv/rainocodes) with archives uploaded to [youtube](https://www.youtube.com/channel/UCx02LJT0RTblmsGnDoNGxKw).

* **Proof-of-Work** (HashCash)

* **Account model of state** (as opposed to Bitcoin's UTXO)

### Dependencies

* `scala`
* `sbt`
* `docker`
* `docker-compose`

### Testing

* `sbt`
    * `resourceGeneration/run`
    * `testOnly -- -l DockerComposeTag` (Run non-docker tests)
    * `dockerComposeTest`

### Work in Progress and full of vulnerabilities!

* Docker testing is very new and some tests intermittently fail. I'm currently making it more robust in the `peer-discovery` branch
* There are many security flaws in the program that I have discovered over time. One fun one is that you can make someone else's node save or load the blockchain on demand over the network!
