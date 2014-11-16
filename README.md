LogicProver
===========

A proposition validation system for verifying the validity of classical propositional statements. 

### Usage

The expected usage is as a command line executable that can take a proposition such as:

```
$ logic “p and ~p”
    Invalid:
        Contradiction: found both ‘p’ and ‘~p’
```

If used on a valid statement, the executable would respond with the valid cases:

```
$ logic “p -> p”
    Valid
```

Underlying this system is a basic proof-tree mechanism. See [analytic tableaux](http://en.wikipedia.org/wiki/Method_of_analytic_tableaux).

### TODO

* Improve output 
    * current only valid/invalid, but required information for specific success/fail cases is available -- see [Usage](Usage)
