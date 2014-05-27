Shakespeare
===========

Shakespeare is a web app that turns an input text into a poem. For example, an input text of:

> It all seems rather silly now, how she took every little thing he ever said and did, every tiny bit of attention he ever paid her, and molded it into a great unspoken love, a bright splendid future filled with road trips and hotel rooms and dinner parties and kids and a sprawling garden and a pet pug and maybe even a bunny.

Might produce the following poem:

> Road kids and kids into a bright splendid,
>
> Every little thing he ever paid her and molded,
>
> Little thing he ever pug and did every tiny,
>
> It even a great unspoken love a bunny.

[You can see a demo here](trehans0.xvm.mit.edu:8000)

It works via a rhyme analyzer and a simple Markov model. First, the input text is stripped of everything but alphanumeric characters. Then, each word is converted to its phonetic equivalent using the [CMU Pronouncing Dictionary](http://www.speech.cs.cmu.edu/cgi-bin/cmudict), and each pair of words is characterized using a "rhyme coefficient". The rhyme coefficient is based on how many of their phonetic syllables, starting from the end of the word, match up. After the rhyme comparision, the two pairs of words with the highest rhyme coeffient are selected. These four words form the last words in each line.

To produce the rest of each line, Shakespeare uses a [Markov model](http://en.wikipedia.org/wiki/Markov_model). The model is constructed by looking at each pair of consecutive words: the state is a word, and a transition occurs from one word to another word if the second word immediately preceeds the first somewhere in the input text. With this model, each line is expanded backwards from the last word to the first word. Finally, the capitalization of each word is corrected and punctuation is inserted where appropriate.

The web page itself is written in [Elm](http://www.elm-lang.org), a new language based on [functional reactive programming](http://elm-lang.org/learn/What-is-FRP.elm) that compiles to HTML, CSS, and Javascript. It is served using a simple web application framework derived from my [Spin.hs](https://github.com/trehansiddharth/spinhs) project, which uses low-level socket programming. Shakespeare also uses my [Probabilities](https://github.com/trehansiddharth/probabilities) library for probabilistic simulation, modeling, and inference to construct the Markov model.

The project is divided into the following folders:

* `Probabilities`: Contains the Probabilities library. It has four modules:
	* `Probabilities`: The `Probabilities` monad and common probability distributions, such as `normal`, `geometric`, `uniform`, etc. It also contains sampling functions, such as `draw` and `sample`, and Bayesian inference functions, such as `given`. The `given` function is used to evaluate the Markov transition model.
    * `Probabilities.DistBuilder`: An abstraction for creating a distrbution from sample points collected as you go. This is used to build the transition model, where each sample point is a pair of consecutive words.
    * `Probabilities.Markov`: The `Markov` type, a combination of a state and a transition model. The progress of the Markov model can be thought of as the time-dependent evolution of a wave function. After each step, the state is observed using the `collapse` function.
    * `Probabilities.Lens`: A highly reduced version of the Haskell [`lens` library](https://hackage.haskell.org/package/lens) for use with the `State` monad. It is used to conveniently modify or extract only part of the state.
* `Phonetics`: The `Phonetics` modules contains functions for parsing the CMU Pronouncing Dictionary into a binary search tree and calculating the rhyme coefficient of two words.
* `Shakespeare`: The `Shakespeare` module contains the `shakespeare` function, which runs the rhyme analysis and Markov model within the `State` and `Probabilities` monads, and the `runShakespeare` function, which runs `shakespeare` on a `ByteString` using a random generator in the `IO` monad. This is where all of the poem generation happens.
* `Pipes`: Contains the `Pipes` library, a reduced version of Spin.hs. It has two modules:
	* `Pipes`: A system for communication between concurrent threads. The `Pipe` type is a pair of `MVar`s that allow threads to push data to or pull data from other threads (see Haskell's [`Control.Concurrent` library](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent.html).
    * `Pipes.Http`: Multithreaded low-level socket handling using pipes as the key abstraction to sending and receiving data over the network. It has very simple parsing of `GET` and `POST` requests, and is used to receive HTTP requests for HTML files and POST requests of input texts from the web client.
* `Server`: The `Server` module listens at port 8000 and, using the `Pipes.Http` module, sends HTML files and poems to clients. The `runServer` function starts the server.
* `Elm`: Contains the code for the web frontend to Shakespeare, written in the Elm programming language.
* `static`: Contains static content served by the server when the web client sends a `GET` request for something other than `/`. It contains the following files:
	* `elm-runtime.js`: The runtime javascript library for Elm files, which is requested as soon as an Elm-based page is loaded.
* `build`: Contains built versions of the server (as an executable file) and the Elm frontend (as an HTML file)
* `cache`: Contains intermediate flies produced by the Elm compliler for faster building.