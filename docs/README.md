# TBL Documentation

This documentation is very incomplete, but will hopefully be improved in the future.

## Types

TBL supports primitive and aggregate types. Primitives are:

- Integers, signed and unsigned. 8-bit to 64-bit.
- Booleans
- Pointers (including task pointers)

For aggregate types, you can have structures and sized (stack-allocated) arrays.

The Any type is a special type used for limited dynamic typing (think `void *` in C).

## Tasks

Tasks can be used like functions in other languages, but can also be *scheduled* to
run repeatedly. When scheduled, the task's parameters become its state.