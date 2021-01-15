# Competition Monads

This is an experimental project where I explore predicting and listing
possible tournament outcomes. The goal is to be as generic and
adaptable as possible in a two senses:

- it should be easy to adapt to new competition formats and sports
- it should be easy to use new prediction methods for all available
  competition formats

Right now this is using monad transformers in the "tagless-final"
style (see e.g. http://okmij.org/ftp/tagless-final/index.html and
http://felixmulder.com/writing/2020/08/08/Revisiting-application-structure.html).

All of this is currently a work-in-progress.
