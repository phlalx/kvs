# Key-Value store (WIP)

This is a primary/backup RPC-based key-value store written in OCaml using async concurrency library.

The design and some of the code is based on lab 2 of MIT's Distributed Systems class (6.824, Spring 2015). More details on the [class website](http://nil.csail.mit.edu/6.824/2015/labs/lab-2.html) and a previous [implementation in golang](https://github.com/phlalx/key-value-store). See also some [RPC examples](https://bitbucket.org/yminsky/core-hello-world). 

## The view service

The view service coordinates the replicas and decide which one is the primary. It maintains a sequence of numbered views of the form `viewnum, primary, backup`. 

Each replica pings the VS regularly with its current view. This allows the VS  to:
 * be informed of the replicas current view,
 * detect failure of the replicas.
