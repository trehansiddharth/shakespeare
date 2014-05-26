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