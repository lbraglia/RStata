RStata [![Build Status](https://travis-ci.org/lbraglia/RStata.svg)](https://travis-ci.org/lbraglia/RStata) [![Build status](https://ci.appveyor.com/api/projects/status/hr28hr9hu7f80cpy)](https://ci.appveyor.com/project/lbraglia/rstata)
======
A simple interface for `R -> Stata`, allowing the user to:
- execute Stata commands (both inline and from a `.do` file) from R;
- pass a `data.frame` to Stata;
- return a `data.frame` to R;
- include Stata 'chunks' in knitr documents (however each chunk is
  independent from the others;  feature tested with LaTeX/Linux).

## Install
```
install.packages('RStata')
```

## Setup
In order to use this package you need to setup two `options`:
```
options("RStata.StataPath")
options("RStata.StataVersion")
```
Once done, the main function in the package is `stata` (see Examples below).


### RStata.StataPath
`RStata.StataPath` contains the path to Stata executable (Windows
user have to delete the extension `.exe` from the path ).

You can use

```
chooseStataBin()
```

to set this option the first time:
- in Linux it searches for the 4 (cli) Stata binaries (`stata-mp`,
  `stata-se`, `stata`, `stata-sm`) in the search path and prompt a menu to
  choose one;
- in Windows does a `file.choose` and a bit of manipulation to obtain a valid
  Windows path (no `.exe` extension).

`chooseStataBin` set up `RStata.StataPath` accordingly to user advice and
return the path; but you may consider to put the path returned in
`.Rprofile` options for the next time you start R.

### RStata.StataVersion
`RStata.StataVersion` is needed for right management of data export/import
to/from Stata.  Eg, for Stata 13:
```
options("RStata.StataVersion" = 13)
```

As above, you may consider to put this statement in your `.Rprofile`.

### Other options
See `?stata`.

## Examples

### One inline command
```
> stata("help regress")  #<- this won't work in Windows dued to needed
                         #   batch mode

. help regress

Title
-----

    [R] regress -- Linear regression


Syntax
------

        regress depvar [indepvars] [if] [in] [weight] [, options]

[...]
```

### Many inline commands
From version 0.4.0 you can do this (eg.):
```
> stata_src <- "

version 10
sysuse auto
reg mpg weight

"
> stata(stata_src)

[...]
```

### External .do file sourceing
```
> stata("foo.do")
```

### Data input to Stata
```
> x <- data.frame(a = rnorm(3), b = letters[1:3])
> stata("sum a", data.in = x)

. sum a

   Variable |       Obs        Mean    Std. Dev.       Min        Max
------------+--------------------------------------------------------
          a |         3   -.5603985    .3000552  -.7720861  -.2170166
```

### Data output from Stata
Eg in order to obtain `auto` dataset
```
> auto <- stata("sysuse auto", data.out = TRUE)

. sysuse auto
(1978 Automobile Data)
 
> head(auto)
           make price mpg rep78 headroom trunk weight length turn displacement
1   AMC Concord  4099  22     3      2.5    11   2930    186   40          121
2     AMC Pacer  4749  17     3      3.0    11   3350    173   40          258
3    AMC Spirit  3799  22    NA      3.0    12   2640    168   35          121
4 Buick Century  4816  20     3      4.5    16   3250    196   40          196
5 Buick Electra  7827  15     4      4.0    20   4080    222   43          350
6 Buick LeSabre  5788  18     3      4.0    21   3670    218   43          231
  gear_ratio  foreign
1       3.58 Domestic
2       2.53 Domestic
3       3.08 Domestic
4       2.93 Domestic
5       2.41 Domestic
6       2.73 Domestic
```

### Data input/output
```
> x <- data.frame(a = rnorm(3), b = letters[1:3])
> (y <- stata("replace a = 2", data.in = x, data.out = TRUE))

. replace a = 2
(3 real changes made)

  a b
1 2 a
2 2 b
3 2 c
```


## News
[Here](http://raw.githubusercontent.com/lbraglia/RStata/master/NEWS).

## Contribute
You are welcome to:
* submit suggestions and bug-reports at: <http://github.com/lbraglia/RStata/issues>
* fork and send a pull request on: <http://github.com/lbraglia/RStata/>
* send an e-mail to: <lbraglia@gmail.com>
