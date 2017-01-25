# dialyzeit
Weird dialyzer behaviour samples

---

## IT WAS FIXED!
This issue was fixed by [@aronisstav](https://github.com/aronisstav) [here](https://github.com/erlang/otp/pull/934)

---

## A bad behaviour in the PLT

### Symptoms
You implement a behaviour in your app and you see something like the following output from dialyzer:
```erlang
bad_child.erl:4: Callback info about the bad_behaviour behaviour is not available
```

### The problem
To fully replicate this situation you need a very very very specific scenario, as follows. To get there, follow these steps:

1. Create an app (like [first](first_app)).
2. Add a behaviour definition in it (like [bad_behaviour](first_app/src/bad_behaviour.erl)) with one or more callbacks.
3. For each callback, add a private function to that module with the same name and arity.
4. Now, create another app that has your first one as a dependency (like [second](second_app)).
5. In that app, implement the aforementioned behaviour (like I did in [bad_child](second_app/src/bad_child.erl)).
6. Generate a plt that includes the first app.
7. Use that plt to dialyze the second app.

I'm not 100% sure how this problem is generated or why, but the important things to trigger the issue are:
- the module defining the behaviour has to be accessible to dialyzer **only** through the plt, i.e. it doesn't fail if dialyzer can analyze both the behaviour definition and its implementation at the same time.
- the function with the same name as the callback must be **not exported**. If you export it, everything works fine.

### In Real-Life
If you want a taste of this in an actual _real-life_ project, try to implement `egithub_webhook` behaviour from [erlang-github](https://github.com/inaka/erlang-github) in your project using a release older than inaka/erlang-github#76 (i.e. older than 0.1.19).

### Watch it happen!
There are 2 ways to see this thing happening in this very repo:

#### With erlang.mk
I'm an [erlang.mk](http://ninenines.eu/docs/en/erlang.mk/1.0/) fan myself, so I used erlang.mk to build this project. I added some tiny hacks to Makefiles, but nothing too complex to understand. Anyway, these are the steps to reproduce the behaviour:

##### good_child is good
```bash
$ cd first_app
$ make app dialyze
 DEPEND first.d
 ERLC   bad_behaviour.erl good_behaviour.erl good_child.erl
 APP    first.app.src
  Compiling some key modules to native code... done in 0m0.23s
  Creating PLT /Users/elbrujohalcon/Projects/elbrujohalcon/dialyzeit/first_app/.first.plt ...
Unknown functions:
  compile:file/2
  compile:forms/2
  compile:noenv_forms/2
  compile:output_generated/1
  crypto:block_decrypt/4
  crypto:start/0
Unknown types:
  compile:option/0
 done in 0m38.29s
done (passed successfully)
  Checking whether the PLT /Users/elbrujohalcon/Projects/elbrujohalcon/dialyzeit/first_app/.first.plt is up-to-date... yes
  Proceeding with analysis...
good_child.erl:10: The inferred return type of bad/0 ('not_bad') has nothing in common with 'bad', which is the expected return type for the callback of bad_behaviour behaviour
 done in 0m0.56s
done (warnings were emitted)
make: *** [dialyze] Error 2
```
As you can see, the warning about not respecting the behaviour callback was emitted. That means dialyzer was capable of understanding and validating the behaviour. That's **good**!

##### bad_child is bad
```bash
$ cd ../second_app/
$ make app dialyze
 DEPEND second.d
 ERLC   bad_child.erl
 APP    second.app.src
  Compiling some key modules to native code... done in 0m0.24s
  Creating PLT /Users/elbrujohalcon/Projects/elbrujohalcon/dialyzeit/second_app/.second.plt ...
Unknown functions:
  compile:file/2
  compile:forms/2
  compile:noenv_forms/2
  compile:output_generated/1
  crypto:block_decrypt/4
  crypto:start/0
Unknown types:
  compile:option/0
 done in 0m37.46s
done (passed successfully)
  Checking whether the PLT /Users/elbrujohalcon/Projects/elbrujohalcon/dialyzeit/second_app/.second.plt is up-to-date... yes
  Proceeding with analysis...
bad_child.erl:4: Callback info about the bad_behaviour behaviour is not available
 done in 0m0.57s
done (warnings were emitted)
make: *** [dialyze] Error 2
```
Now, the info about `bad_behaviour` could _not_ be found. Note that the info about `good_behaviour` could totally be found, so it's not a matter of first_app/ebin missing in the path, the plt or whatever.

#### Without erlang.mk
Yeah, I know what you think, that has to be somehow bound to some erlang.mk shenannigans, right?
Well… Let's try _without_ erlang.mk. Let's use [a bash script](run) instead.

```bash
$ ./run
Compile and analyze the 'first' app ...
  Creating PLT dialyzeit.plt ...
Unknown functions:
  erlang:get_module_info/1
  erlang:get_module_info/2
 done in 0m0.12s
done (passed successfully)
  Checking whether the PLT dialyzeit.plt is up-to-date... yes
  Proceeding with analysis...
good_child.erl:10: The inferred return type of bad/0 ('not_bad') has nothing in common with 'bad', which is the expected return type for the callback of bad_behaviour behaviour
Unknown functions:
  erlang:get_module_info/1
  erlang:get_module_info/2
 done in 0m0.07s
done (warnings were emitted)

Above this line you should see a warning about the inferred return type of bad/0

Compile and analyze the 'second' app ...

  Checking whether the PLT dialyzeit.plt is up-to-date... yes
  Proceeding with analysis...
bad_child.erl:4: Callback info about the bad_behaviour behaviour is not available
Unknown functions:
  erlang:get_module_info/1
  erlang:get_module_info/2
 done in 0m0.07s
done (warnings were emitted)

Above this line you should see the same warning, but instead you get one about
bad_behaviour behaviour not being available
```
