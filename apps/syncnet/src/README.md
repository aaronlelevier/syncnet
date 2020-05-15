# Synchronous Network Model (syncnet)

This details my implementation of the different components of the `syncnet` from *Distributed Algorithms* by Nancy Lynch.

## Vertex

This is a single Node in the Network

It has 3 states:

* idle
* running
* halted

A **Vertex** can transition from `idle` to `halted` if:

* it receives a `wakeup` message from the Environment Node
* it receives a non-null message

## Environment Node

A special Node, where the is only one that can do things like send `wakeup` messages to normal Nodes to create variable start times in the syncnet