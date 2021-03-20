elli-swagger
=====

A [swagger](https://swagger.io/)(😎) middleware for [elli](https://github.com/elli-lib/elli).

How To use?
---
This library was thought to be simple and easy to use.

**1.** Configure your application, which can be accomplisehd by adding

```erl
    [
        ...
        {elli_swagger, [{
            swagger_metadata, #{openapi => <<"3.0.0">>,
                                info => #{title => <<"Doc Tile">>}},
            {elli_swagger_documentation_callback, you_callback_module}
        }]}
        ...
    ].
```
to your configuration file.
**2.** Add `elli_swagger` to your `rebar.config` so it can be loaded. If you want it to be loaded on a release:
```erl
    {relx, [{release, {elli_swagger_example, vsn}},[your_application,
                                                    elli,
                                                    {elli_swagger, load}]]}
     ...
```
**3.** You must specify that you want to copy the `swagger` folder inside `_build/default/lib/elli_swagger` in your `rebar.config`. To do that you just need to 

**4.** Everything is ready now and you just need to call `elli_swagger:start/2`.
```erl
    elli_swagger:start([{your_module, your_args}], port_number)
```

How to document?
---
As you have probably noticed, one of the configuration variable is called `elli_swagger_documentation_callback`.

Setting the documentation in the same file as your code is a clever move, as it might boost readibility, however you might want to put the documentation callback on another module (which is fine) and in that case you can set that callback to be anything you want.

Please have in mind that this library is under development, and if you want for not setting a callback is not supported. Please always set the configuration properly (as in last section).

Contributions
---
Feel free to contribute to this project with opening an issue or opening a pull request!

License
---
Apache 2.0
