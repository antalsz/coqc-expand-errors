# coqc-expand-errors

Have you ever gotten frustrated while doing batch-mode Coq development because
you have no idea where the errors are coming from?  `coqc-expand-errors` aims to
address that.  If you replace `coqc` with `coqc-expand-errors coqc`, then [your
output will become][coqc-expand-errors output]

```coq
File "./triples.v", line 3, characters 30-36:
Error:
In environment
A : Type
B : Type
C : Type
triple : A * B * C
p : A * B
third : C
first : A
second : B
The term "second" has type "B" while it is expected to have type "A".

  1: Definition fst3 {A B C} (triple : A * B * C) : A :=
  2:   match triple with
> 3:   | (first, second, third) => second
  4:   end.

Error found in the definition "fst3"

```

Instead of [the more traditional][coqc output]

```coq
File "./test.v", line 3, characters 30-36:
Error:
In environment
A : Type
B : Type
C : Type
triple : A * B * C
p : A * B
third : C
first : A
second : B
The term "second" has type "B" while it is expected to have type "A".

```

And it even does this with color!
![The new coqc-expand-errors output compared to the old coqc output][coqc-expand-errors vs coqc]

[coqc-expand-errors vs coqc]: README/new-vs-old-coqc-expand-errors-vs-coqc-outputs.png
[coqc-expand-errors output]:  README/new-coqc-expand-errors-output.png
[coqc output]:                README/old-coqc-output.png

For use with a Coq-generated `Makefile`, run your Makefile as either

```shell
make COQC='coqc-expand-errors coqc'
```

or

```shell
COQC='coqc-expand-errors coqc' make
```

--------------------------------------------------------------------------------

Be warned: `coqc-expand-errors` is very much a best-effort tool.  It won’t
produce perfect output in all cases.  And I’m not devoting a lot of time to it.
But I hope you find it helpful!

--------------------------------------------------------------------------------

Installation is using [Stack](https://www.haskellstack.org/) – download Stack,
run `stack install`, and make sure the installation directory is on your `PATH`.
