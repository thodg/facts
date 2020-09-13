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
* https://github.com/cl-facts/lessp
* https://github.com/cl-facts/rollback


Usage
-----

### FACTS:ADD &rest SPECS
Adds facts (triples) to the database. Triples can be grouped by subject.

```common-lisp
(facts:add ("Blade Runner" :is-a :movie
                           :director "Ridley Scott"
                           :actor "Harison Ford"
                           :actor "Rutger Hauer")
           ("Snow White" :is-a :movie
                         :director "William Cottrell"
                         :director "David Hand"))
```

```common-lisp
(facts:add (?movie :is-a :movie
                   :title "Blade Runner"
                   :director "Ridley Scott"
                   :actor "Harrison Ford"
                   :actor "Rutger Hauer"))

(facts:add (?movie :is-a :movie
                   :title "Snow White and the Seven Dwarfs"
                   :director "William Cottrell"
                   :director "David Hand"))
```

The second version with `?movie` will generate an anonymous symbol prefixed with `movie-`.
It is considered a more clean and efficient way to abstract identifiers.

### FACTS:RM &rest SPECS
```common-lisp
(facts:rm ((?movie :actor "Harison Ford")))
```

### FACTS:WITH SPECS &body BODY

To follow Wittgenstein's view of the world, all queries get turned into
testing the presence or absence of triples (facts).

```common-lisp
(defun movie-title (movie)
  (facts:with ((movie :is-a :movie
                      :title ?title))
    (return ?title)))
```

is equivalent to

```common-lisp
(defun movie-title (movie)
  (facts:with ((movie :is-a :movie)
               (movie :title ?title))
    (return ?title)))
```

which is itself equivalent to

```common-lisp
(defun movie-title (movie)
  (facts:with ((movie :is-a :movie))
    (facts:with ((movie :title ?title))
      (return ?title))))
```

Multiple queries on the same subject can be grouped together easily :

```common-lisp
(facts:with ((?movie :is-a :movie
                     :title ?title
                     :director ?director))
  (format t "~A directed ~A~%" ?director ?title))
```

### FACTS:\*DB\*
The current facts database.

### FACTS:CLEAR-DB
Clears the database from every facts.

### FACTS:SAVE-DB &key INTO (READABLY T)
Dump the database facts into filespec INTO.

### FACTS:LOAD-DB SRC
Load the facts from SRC into \*db\*.

### FACTS:WITH-TRANSACTION &body BODY
Enclose BODY database operations into a transaction.

A transaction ensures that all database operations will succeed or be
reverted using their respective rollback functions.

Transactions can be nested safely.

Examples
--------

* https://github.com/RailsOnLisp/rol-server/blob/master/resource.lisp
* https://github.com/lowh/mentats
