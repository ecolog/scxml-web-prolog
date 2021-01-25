# SCXML + Web Prolog

This is a proof-of-concept implemention showing how Prolog ([Web Prolog](https://gup.ub.gu.se/file/207827) to be more precise) can be used as a datamodel and scripting language in (a slightly modified dialect of) [State Chart XML](https://en.wikipedia.org/wiki/SCXML) (SCXML).

Although the implemention is fairly complete (see [web-client/scxml.pl](https://github.com/torbjornlager/scxml-web-prolog/tree/main/web-client/scxml.pl)), it certainly isn't ready for serious use. After all, it's just a PoC. Here is a list of issues:

- In implementation is written in a very imperative, non-Prologish style. Should be rewritten to avoid as many globals as possible.
- The predicate `remove_conflicting_transitions/2` is not yet implemented.
- Isolation is not perfect. One SCXML process might conflict with another.
- There's very little error handling.
- There are examples, but no real unit tests.

What you can do at this point is to look at the examples, run them, and inspect the traces. The PoC comes with more than a dozen examples exercising the most important aspects of SCXML + Web Prolog. Features such as hierarchy, history states, parallelism, broadcast communication, process invocation, inter-process communication, and (of course!) the use of Web Prolog as a datamodel and scripting language are all represented in one or more examples.

The examples can be found in [scxml-web-prolog/web-client/scxml/](https://github.com/torbjornlager/scxml-web-prolog/tree/main/web-client/scxml). Contributions of additional ones are of course most welcome!

## Installing and running

### Installing

#### 1. Get the latest SWI-Prolog

Install the latest SWI-Prolog _development version_ [here](https://www.swi-prolog.org/download/devel). 

Note that if you're running MacOSX, you may need to install [xquartz](http://xquartz.macosforge.org/) as well, in order for the trace mechanism to work as intended.


#### 2. Clone or download the repo

### Running

From the `scxml-web-prolog` directory, do:

```
$ cd web-client
$ swipl run.pl
```

The **Prolog debug monitor** should now appear. (It may take a while the first time.)

Let's run two example processes, with [scxml/pause-and-resume.scxml](https://github.com/torbjornlager/scxml-web-prolog/tree/main/web-client/scxml/pause-and-resume.scxml) and [scxml/pingpong.scxml](https://github.com/torbjornlager/scxml-web-prolog/tree/main/web-client/scxml/pingpong.scxml). At the Prolog prompter, do:

```prolog
?- run('scxml/pause-and-resume.scxml', Pid).
Pid = '10099674'.

?- $Pid ! e1.
Pid = '10099674'.

?- $Pid ! e2.
Pid = '10099674'.

?- $Pid ! e1.
Pid = '10099674'.

?- $Pid ! pause.
Pid = '10099674'.

?- $Pid ! resume.
Pid = '10099674'.

?- $Pid ! foo.
Pid = '10099674'.

?- $Pid ! terminate.
Pid = '10099674'.

?- flush.
% Got down('10099674'@'http://localhost:3060',exit)
true.

?- run('scxml/pingpong.scxml', Pid).
Pid = '20467645'.

?- exit($Pid, stop).
Pid = '20467645'.

?- flush.
% Got down('20467645'@'http://localhost:3060',stop)
true.

?-
```

Here's what the debug monitor should now show:

![The Prolog debug monitor](img/debug-monitor.jpg?raw=true "The Prolog debug monitor")

Success running the other examples!

And as I said, contributions of additional examples are of course most welcome!
