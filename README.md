About
=====

`frotate` stands for "functional rotate", whatever.
This is an evolution of the [log2rotate's ideas](http://jekor.com/log2rotate).
See also [pylog2rotate](https://github.com/avian2/pylog2rotate).

`frotate` is designed to rotate backups with any balance between retention
and space usage.  Instead of rotating backups using some familiar method such
as daily, weekly, monthly, and yearly periods, it rotates backups using any
periods. Thus "functional".

The idea is simple, the rotation schedule is determined by an integer function.
This function gives us a period (number) of days when we must encounter at
least one backup or whatever we are rotating. When we use an exponential
function, the scheme is similar to the radioactive decay law. When the
funtion is simply a constant 1, we don't rotate anything and retain all
the backups. If it is 2, we retain each second backup.  With some trivial
function we can achieve a well-known dayly-weekly-monthly-yearly scheme.

The `frotate` command line utility implements only exponential periods with
arbitrary base (ensure it is > 1, or have fun otherwise).


Usage
=====

Note that when neither `--keep` nor `--delete` option is given, the utility
prints all intervals with all days in them _to standard error_. In production
you will need to specify `--keep` or `--delete` explicitly.

```
Usage: frotate ([-k|--keep] | [-d|--delete]) [-b|--base BASE] DAY...

Available options:
  -k,--keep                Print days to keep
  -d,--delete              Print days to delete
  -b,--base BASE           Base of the exponent (default: 1.1)
  -h,--help                Show this help text

```


Example
=======

Different modes with the same days:

```
$ frotate --base 2 2019-08-31 2019-08-30 2019-08-29 2019-08-28 2019-08-27 2019-08-26 2019-08-25 2019-08-24
2019-08-31
2019-08-30 2019-08-29
2019-08-28 2019-08-27 2019-08-26 2019-08-25
2019-08-24

$ frotate --keep --base 2 2019-08-31 2019-08-30 2019-08-29 2019-08-28 2019-08-27 2019-08-26 2019-08-25 2019-08-24
2019-08-31 2019-08-30 2019-08-28 2019-08-24

$ frotate --delete --base 2 2019-08-31 2019-08-30 2019-08-29 2019-08-28 2019-08-27 2019-08-26 2019-08-25 2019-08-24
2019-08-29 2019-08-27 2019-08-26 2019-08-25
```

More or less realistic example when we keep some backups and get new ones, but not every day:

```
$ frotate --keep --base 2 2019-09-01 2019-08-31 2019-08-30 2019-08-28 2019-08-24
2019-09-01 2019-08-31 2019-08-28 2019-08-24

$ frotate --keep --base 2 2019-09-05 2019-09-01 2019-08-31 2019-08-28 2019-08-24
2019-09-05 2019-09-01 2019-08-28

$ frotate --keep --base 2 2019-09-06 2019-09-05 2019-09-01 2019-08-28
2019-09-06 2019-09-05 2019-09-01 2019-08-28

```

