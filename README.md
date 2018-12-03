# An Emacs interface for [CC-CEDICT](https://cc-cedict.org/wiki/)
[![Build Status](https://travis-ci.org/xuchunyang/cc-cedict.el.svg?branch=master)](https://travis-ci.org/xuchunyang/cc-cedict.el)

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
(:Traditional "姊妹"
 :Simplified "姊妹"
 :Pinyin "zi3 mei4"
 :English ("(older and younger) sisters" "sister (school, city etc)"))
```
