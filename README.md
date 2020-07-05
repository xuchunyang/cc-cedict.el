# An Emacs interface for [CC-CEDICT](https://cc-cedict.org/wiki/) 
[![MELPA](https://melpa.org/packages/cc-cedict-badge.svg)](https://melpa.org/#/cc-cedict)

CC-CEDICT is a public-domain Chinese-English dictionary. `cc-cedict.el` is an Emacs interface for it.

## Setup

You need to manually download CC-CEDICT dictionary from https://cc-cedict.org/wiki/, extract the tarball then set `cc-cedict-file`. Something like the following:

``` shell
$ wget https://www.mdbg.net/chinese/export/cedict/cedict_1_0_ts_utf-8_mdbg.txt.gz
$ gunzip cedict_1_0_ts_utf-8_mdbg.txt.gz
```

``` emacs-lisp
(setq cc-cedict-file "/path/to/cedict_1_0_ts_utf-8_mdbg.txt")
```

## Usage

``` emacs-lisp
(cc-cedict "姊妹")
;; =>
#s(cc-cedict-entry "姊妹" "姊妹" "zi3 mei4" ("(older and younger) sisters" "sister (school, city etc)"))
```

## Dependencies

- Emacs 26.1 or higher
