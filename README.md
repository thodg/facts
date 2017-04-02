Facts
=====

> 1.1 The world is the totality of facts, not of things.
>
> --  Ludwig Wittgenstein, Tractatus Logico Philosophicus

Facts is a small in-memory graph database for Common Lisp.
It features :
* a triple store based on unlabelled skip lists
* a very simple query language (facts:with)
* transactions using rollback functions
* logging and replay of transactions to/from disk
* dumping and loading the database to/from disk


Requirements
------------

You will need :
* https://github.com/thodg/lessp
* https://github.com/thodg/rollback


Examples
--------

* https://github.com/RailsOnLisp/rol-server/blob/master/resource.lisp
* https://github.com/lowh/mentats
