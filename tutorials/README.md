# Grisette Tutorials

The tutorials to Grisette is provided as jupyter notebooks with the
[IHaskell](https://github.com/IHaskell/IHaskell) kernel. You may run the
notebooks with the following command (assuming you've already installed the
IHaskell):

```bash
stack install --fast
ihaskell install --stack
stack exec jupyter -- notebook
```

Currently, there are three tutorials available:

1. [Solve-Aided Programming with Grisette](./1_symbolic_type.ipynb). This
   tutorial introduces the symbolic values in Grisette and how to use them to
   write solve-aided programs.
1. [UnionM and Custom Data Types](./2_union.ipynb). This tutorial introduces the
   `UnionM` monad, which is at the core of Grisette for handling multi-path
   execution, and how to use it to work with custom data types in Grisette.
1. [Using Monad Transformers with Grisette](./3_monad_transformer.ipynb). This
   tutorial introduces monad transformers and how to use them with Grisette to
   build advanced program reasoning tools with error and state handling.
