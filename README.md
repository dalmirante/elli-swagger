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
                                info => #{title => <<"Doc Tile">>}}
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
    elli_swagger:start([your_module], port_number)
```

**5.** Finally, in the module you have set in the previous step, just define a function named `elli_swagger_config`. This function should return a list containing your application path's, the module that handles that path, any arguments you want to send to that module and finally that path's documentation.

Contributions
---
Feel free to contribute to this project with opening an issue or opening a pull request!

License
---
Apache 2.0
