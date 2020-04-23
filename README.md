# Customer database

## Building instructions

Assuming you have Stack already installed you should just be able to run

```
stack test
stack run
```

The tests/code assumes the existence of a MongoDB and will create and teardown test databases (except during failure).

The database details are all hard-coded for now.
