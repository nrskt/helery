# helery

# What' helery?

helery is inspired by "Celery" which python's distributed task queue library.
helery work with Redis as message broker for queuing message.(Not support RabbitMQ now.)


# Motivation
Celery workers are implemented in Python code.
Sometimes I am dissatisfied about execution speed and parallel / concurrency processing.
Some task should execute as fast as possible. But python is hard to implement it,
because python has GIL.

And python is dynamic type language. It is unuseful some case.
If some language or some system use the task queue, the request type should be strictly observed.

So I decide to use Haskell. (Other reason, I interested to learn Haskell too.)

# How to use

(Now not implement client code, so need to use python "Celery".)
Full sample code is `example/Sample.hs`.

```
| Client |  -- request -> | Redis | -> | helery |
```

1. Define Handler type.
    * Handler type must implement Handler type-class and FromJSON type-class.

    ```Haskell
    type AHandler = BaseHandler AHandlerArgs AHandlerKwargs
    ```

2. Define Router type.
    * Router type must implement Dispatchable type-classand FromJSON type-class.

    ```Haskell
    data MyRouter = AHandlerRoute AHandler
                  | BHandlerRoute BHandler
                   deriving (Eq, Show)
    ```

3. Implement main.
   Use `parseCommandLineOption` function.
   The function get the option which broker url, queue name
   and configuration file path.

   If you want to use custom configuration for your app,
   You can use `-c or --config` option. The Option represents
   the configuration file path.

   For example, You can write the DB connection info which use for your app.


    ```Haskell
    main :: IO ()
    options <- parseCommandLineOption
    main = runApp options (routing :: RoutingType IO MyRouter)
    ```

4. Client code(python).

    client.py
    ```python:client.py
    from celery import Celery

    app = Celery()
    app.config_from_object("config")

    app.send_task("AHandler", args=[1, 2], kwargs=dict(name="nrskt", age=20), queue="hsworker")
    ```

5. run

    ```
    # start worker
    ./example -q hsworker -b redis://:password@localhost:6379/0

    # send task
    python client.py
    ```

# Want to implement

* ~~Use configuration file~~
* Logging
* eta
* retry
* Support RabbitMQ
