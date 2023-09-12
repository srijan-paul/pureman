# Pureman

A Faithful clone of the 1980 Arcade game – [Pacman](https://en.wikipedia.org/wiki/Pac-Man).
This project tries to mimic the behavior of the maze and the ghosts as closely as possible,
and then introduce an AI Actor to play the game.

Why?
I wanted to ease my way into modelling video game entities and real time event dispatch in a purely functional setting.
There's very few projects I that do this well.
There's [Defect Process (Haskell)](https://github.com/incoherentsoftware/defect-process), [Lich (Scala)](http://michaelshaw.io/game_talk/game.html#/), [Carmack's keynote (Haskell)](https://www.youtube.com/watch?v=Uooh0Y9fC_M),and then... nothing?
(that I know of, at least).

Even half the links in the [Game-dev page on Haskell wiki](https://wiki.haskell.org/Game_Development) are dead!

At the time of me writing this, the core game is about 90% finished.
Once fully completed, I'll train a reinforcement learning model to play the game.
You know, to please our future overlords.

## Building locally

```
$ pnpm install
$ pnpm dev
```

That's it, now go check your browser.
