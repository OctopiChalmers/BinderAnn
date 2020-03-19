# BinderAnn: Automated Reification of Source Annotations for Monadic EDSLs

BinderAnn is a simple GHC source-to-source plugin for enhancing monadic EDSLs with extra static information. It works by attaching *source annotations* to every `do` statement in the code. These annotations are generated automatically, and extend each existing `do` statement with:

* Its bound name (if any), e.g., "foo" in `foo <- bar`  
* Its location in the code (file, row, col), e.g., ("Main.hs, 42, 24).

Having access to this static information is especially desirable for code-generating EDSLs, and for providing better domain-specific error messages. Without any explicit support, this information is lost during compilation, so the EDSL code cannot easily take advantage of it. Until now!

The basic idea is to automatically transform `do` expressions of the form:

```
myExpr = do
  x <- foo
  y <- bar
  baz
```

Into:

```
myExpr = do
  x <- foo `annotateM` Info (Just "x") ("Main.hs", 2, 3)
  y <- bar `annotateM` Info (Just "y") ("Main.hs", 3, 3)
  baz      `annotateM` Info Nothing    ("Main.hs", 4, 3)
```

Where `annotateM` acts as the "gluing" function between the source annotations (`Info {..}`) and the EDSL implementation. By default, `annotateM` simply discards annotations altogether, but the programmer can simply override this behavior with the desired one by defining different type-class instances.

## TFP'20 Paper

BinderAnn was presented at [TFP'20](http://www.cse.chalmers.se/~rjmh/tfp/)! The paper has not yet been published, but you can  find a preprint [here](http://www.cse.chalmers.se/~mista/assets/pdf/tfp20.pdf). I'd suggest you to take a look at it first if you're planning to use this plugin.

## Working examples

* We adapted four existing real-world EDSLs to work with BinderAnn. You can find the code [here](https://github.com/OctopiChalmers/BinderAnn-examples).

* [PropProver](https://github.com/OctopiChalmers/PropProver): a cute EDSL for proving propositional logic formulas kinda interactively.

* The [test](./test) directory contains some extra working examples.

## Annotation styles

BinderAnn supports three *annotation styles*, i.e., (effect-free, effect-full, and generic). The first two are presented in detail in the TFP paper, while the third one is experimental and should be taken as such.

Each annotation plugin can be selected as follows:

### Effect-free (BinderAnn.Pure)

* Activate it using the `-fplugin=BinderAnn.Pure` compiler option.

* In your EDSL implementation, provide instances of the `Annotated` type class:

```
class AnnotatedM a
```

### Effect-full (BinderAnn.Monadic)

* Activate it using the `-fplugin=BinderAnn.Monadic` compiler option.

* In your EDSL implementation, provide instances of the `AnnotatedM` type class:

```
class Monad m => AnnotatedM m a
```

### Generic (BinderAnn.Generic)

* Activate it using the `-fplugin=BinderAnn.Generic` compiler option.

This annotation style relies on a monad transformer carrying a map of *stable pointers* to the return value of every `do` statement. Hence, it should be able to annotate `do` statements returning values of any type, without having to provide concrete `Annotated` or `AnnotatedM` instances. In this light, the user can lookup for annotations created by the plugin using the return value of an annotated `do` statement as the search key.

**NOTE:** this annotation style works for simple use cases, but it should be considered *very* experimental.


## Annotation scope

### Default (every `do` expression)

The default behavior is to transform *every* `do` expression in the module the plugin runs over. This is quite convinient for modules containing only EDSL code. However, it is also possible to reduce the annotation scope to particular `do` expressions (continue reading).

### Manual (specific `do` expressions)

To be able to select which `do` expressions are subject to be transformed by the plugin, you need to enable the "manual" mode by passing the compiler option `-fplugin-opt=BinderAnn.XXX:manual`, where `XXX` is the annotation style used.

The particular `do` expressions to be annotated can be specified in two different ways:

#### Annotation pragmas

If the `do` expression you want BinderAnn to annotate is bound to a top-level identifier, you can use `ANN` *annotation pragmas* to tell the plugin to consider it:

```
{-# ANN someExpr SrcInfo #-}
myExpr = do
  <annotated do statements>
```

#### Infix annotation operator

If the `do` expression you want BinderAnn to annotate is at the RHS of a `$` operator:

```
myExpr = runExpr $ do
  <do statements>
```

Then you can substitute `$` by an *infix annotation operator* (by default `|$|`). This will tell BinderAnn to annotate any `do` expression at the RHS of the annotation operator:

```
myExpr = runExpr |$| do
  <annotated do statements>
```

You can change the default annotation operator to be the one that looks better in your particular EDSL. For this, you need to pass the compiler option `-fplugin-opt=BinderAnn.XXX:infix=@@`, where `XXX` is the annotation style used.

**NOTE:** using `infix=XXX` implies `manual`!

## Other features

### Annotating tuple patterns
  
BinderAnn is also capable of annotating `do` statements matched against tuple patterns, e.g.:

```
  (foo, bar) <- baz
```

In this case, BinderAnn will produce **two** source annotations, one for `foo` and one for `bar`. Each annotation will be attached to its corresponding element of the resulting tuple. The annotated result will be equivalent to:

```
  (foo, bar) <- do
    (x, y) <- baz
    foo' <- return x `annotateM` Info (Just "foo") (Just ("Main.hs", 5, 3))
    bar' <- return y `annotateM` Info (Just "bar") (Just ("Main.hs", 5, 8))
    return (foo', bar')
```

**NOTE:** For now, this only works for tuples of length 2~5, and where the pattern of every tuple element is a variable. A more general solution is coming soon.

### Lifting annotations for single-field constructors

Thanks to Maciej Bendkowski, BinderAnn can also annotate `do` statements matched against single-field constructors:

```
  Foo x <- bar
```

In such case, the constructor name `Foo` will be ignored, and the annotation will be done as if the pattern were just `x`:

```
  Foo x <- bar `annotateM` Info (Just "x") (Just ("Main.hs", 3, 3))
```
