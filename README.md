# haarss

[![Build Status](https://travis-ci.org/stevana/haarss.svg?branch=master)](https://travis-ci.org/stevana/haarss)

`haarss` is a terminal based feed reader.

It strives to have the same look and feel as
[`snownews`](https://kiza.eu/software/snownews/), while not suffering
from some of its shortcomings.

In particular, `haarss` has the following advantages over `snownews`:

  * Native support for Atom feeds;
  * Can fetch feeds via TLS/SSL;
  * Feeds are fetched and parsed in parallel;
  * Character encodings seem to work better;
  * Crashes are hopefully less common.

Some things `snownews` has are still missing though:

  * OPML imports and exports;
  * Customisable keybindings;
  * Customisable colours;
  * Categories and tags;
  * Extensions, plugins, filters;
  * Help menus.

### Installation

Given a reasonably new version of the [Glasgow Haskell
Compiler](https://www.haskell.org/ghc/) (>= 7.8.1), and
[`cabal-install`](http://hackage.haskell.org/package/cabal-install),
installing `haarss` should be a matter of:

```
git clone https://github.com/stevana/haarss.git
cd haarss
cabal install
```

As usual when working with Haskell packages, make sure `~/.cabal/bin/`
in your `PATH`.

### Usage

`haarss` is controlled via the keyboard as follows:

  * Add and delete feeds with `a` and `D`;

  * Navigate down and up with `DOWN ARROW`, `UP ARROW`, `J`, `j`, `k`,
    and `K`;

  * Update the current feed with `R` or update all feeds with `r`;
  * Read feed/item with `l` or `RETURN`, go back with `q` (going back
    when viewing the feeds will quit the program);
  * Scroll the item's text with `SPACE`, `d`, `b`, and `u`;
  * Open the current item's url using an external browser with `o`;
  * Mark the current item as read with `M`, or mark all items as read
    with `m`;
  * Change the name of a feed with `c`;
  * Rearrange the order of feeds with `N` and `P`;
  * Search among feed/item titles with `/` or `TAB`.

These keybindings are currently hardcoded in `src/Haarss/Main.hs` (see
the `cmd` function).

### Files

`haarss` creates maintains two files; `~/.haarss/config` and
`~/.haarss/model`.

The config file is a humanly readable and editable list of feeds, their
aliases and a list of changes to ignore when making the list of new
items for a newly fetched feed. Here is an example entry:

    (Just "apa", "http://example.com/rss", [Date])

In this case the alias is `"apa"` (it could also have been `Nothing`),
and we ignore if the `Date` has changed -- i.e. if some item has
previously been marked as read, but the date of that item has changed at
the server, then keep it marked as read. This could be useful if a
server bumps the date, in order to try to make you reread their
posts. We can also ignore changes to the title or the description of the
item by adding `Title` or `Description` in the ignore list. Ignoring
changes to the description could be useful if the description changes
over time, e.g. it contains the amount of comments people have made.

The model file is a binary file which contains the current state of the
feeds -- which items feeds have, if they are read or not, etc.

Both files are read when `haarss` is started and written when `haarss`
is shut down, so to avoid losing your changes only edit the config
manually when `haarss` is not running.

### Also see

  * [`snownews`](https://kiza.eu/software/snownews/)
  * [`newsbeuter`](http://www.newsbeuter.org/)
  * [`canto`](http://codezen.org/canto-ng/)
  * [`raggle`](http://raggle.org/)
  * [`ureader`](http://hackage.haskell.org/package/ureader)

### License

ISC, see `LICENSE` file for details.
