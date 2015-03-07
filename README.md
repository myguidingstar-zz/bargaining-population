# bargaining-population

A simulation of automata playing bargaining games. Written in Clojurescript, running on modern browsers.

## Prepare

[Install boot][installboot].  Then, in a terminal:

```bash
boot -u
```

## Build

In a terminal do:

```bash
boot build
```

## Development

In a terminal do:

```bash
boot dev
```

You can view the generated content by opening
[http://localhost:3000/index.html](http://localhost:3000/index.html)
in your browser.

> *OutOfMemoryError Troubleshooting*
>
> boot provides a tool called _pods_ that make it possible for multiple
> independent Clojure classpaths to exist in the same JVM.  Task authors
> can use Maven dependencies without worrying about
> shadowing or otherwise interfering with the dependencies in other pods.
>
> One downside of pods is that their use results in higher-than-usual
> memory consumption by the JVM, particularly
> [PermGen](http://stackoverflow.com/questions/88235/dealing-with-java-lang-outofmemoryerror-permgen-space-error).
>
> If you are using Java 7, you may see errors related to PermGen.  You
> can consult the
> [JVM Options](https://github.com/boot-clj/boot/wiki/JVM-Options) wiki
> page for settings that can help.
>
> You may also consider upgrading to Java 8, as it
> [resolves many PermGen-related issues](http://www.infoq.com/news/2013/03/java-8-permgen-metaspace).

## License

Copyright © 2015 Hoang Minh Thang

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

[installboot]:      https://github.com/boot-clj/boot#install
