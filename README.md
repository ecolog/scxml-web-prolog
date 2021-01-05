# SCXML + Web Prolog

This is a proof-of-concept implemention showing how Prolog (Web Prolog to be more precise) can be used as a datamodel and scripting language in State Chart XML (SCXML).

Although the implemention is fairly complete, it certainly isn't ready for serious use. After all, it's just a PoC. Here is a list that things that needs to be done:

- The predicate `remove_conflicting_transitions/2` isn't implemented
- In implementation is written in a very imperative, non-Prologish style
- There's very little error handling
- There are no tests
- Isolation is not perfect. One SCXML process might conflict with another.

All you can do at this point is to look at the examples and the trace when running them. The PoC comes with more than a dozen examples exercising the most important aspects of SCXML. 

- [Spaghetti](https://github.com/torbjornlager/scxml-web-prolog/raw/main/web-client/scxml/spaghetti.scxml)

```xml
<scxml initial="process">
   <state id="process">
      <initial>
         <go to="s1" />
      </initial>
      <history id="h">
         <go to="s1" />
      </history>
      <state id="s1">
         <go to="s2" on="e1" />
      </state>
      <state id="s2">
         <go to="s1" on="e2" />
      </state>
      <go to="interrupted" on="pause" />
      <go to="terminated" on="terminate" />
   </state>
   <state id="interrupted">
      <go to="h" on="resume" />
      <go to="terminated" on="terminate" />
   </state>
   <final id="terminated" />
</scxml>
```


```text
*** Processing file 'scxml/pause-and-resume.scxml'
Configuration: [process,s1]
   Ext. event: e1
   Transition: s1 => [s2]
Configuration: [process,s2]
   Ext. event: e2
   Transition: s2 => [s1]
Configuration: [process,s1]
   Ext. event: e1
   Transition: s1 => [s2]
Configuration: [process,s2]
   Ext. event: pause
   Transition: process => [interrupted]
Configuration: [interrupted]
   Ext. event: resume
   Transition: interrupted => [h]
Configuration: [process,s2]
   Ext. event: terminate
   Transition: process => [terminated]
Configuration: [terminated]
*** End of processing (a down message was sent to parent)

```




## Installation


### Get the latest SWI-Prolog

Install the latest  [SWI-Prolog](http://www.swi-prolog.org) _development
version_. 

### Clone or download the repo

## Running Web Prolog

From the web-prolog directory, do:

```
$ cd web-client
$ swipl run.pl
```

Now direct your browser to http://localhost:3060/apps/swish/index.html .

A book manuscript describing the approach is available at

https://github.com/Web-Prolog/swi-web-prolog/raw/master/book/web-prolog.pdf

It is very much a draft lacking some of the planned chapters, but should be readable enough for those who want to know more about the ideas behind Web Prolog.

Success!


