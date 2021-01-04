# SCXML + Web Prolog

This is a proof-of-concept implemention showing how Prolog (Web Prolog to be more precise) can be used as a datamodel and scripting language in State Chart XML (SCXML).

The implemention is more or less complete but ... 

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


```xml
<scxml initial="s">
   <datamodel>
      p(a,b). p(b,c). p(c,d).
   </datamodel>
   <state id="s">
      <go if="p(X,Y), p(Y,Z), \+p(X,Z)">
          assert(p(X,Z))
      </go>
      <go if="findall(p(X,Y), p(X,Y), List)" to="f">
          log(List)
      </go>
   </state>
   <final id="f"/>
</scxml> 
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


