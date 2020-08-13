Series Deficiency
=====================

A concept I came up with that describes the multiplicative error accrued by
convolving series representations of integers.

Note that this is intimately connected with
[GenBase](https://github.com/queue-miscreant/GenBase),
particularly integral sequences.

Running
---------------------
If by some miracle you manage to compile, the syntax for running the output is:

```
$ ./deficient list size prime
```
* `list` is read in as `[Integer]`, but can be preceded with "r" to describe the
terms of a recurrence relation (i.e. "r[1,1]" generates the Fibonacci numbers)
* `size` dictates the width/height of the output image
* `prime` is a number. The numbers in the deficiency table modulo `prime` are those
used in writing to the image, in PNG format.
If preceded with "-", then a `prime` frame GIF of the deficiency table will be produced,
where the frame `n` is the deficiency table mod `n`.
If preceded with "--", the GIF will use the product table rather than the deficiency table

The output file will have filename that is something like `list` with only digits.

Examples
---------------------

```
# Generates a PNG of the deficiency table mod 2 for the Fibonacci numbers
# output file "1 1 mod 2.png"
$ ./deficient "r[1,1]" 512 2

# Generates a 10 frame GIF of the deficiency table for the Fibonacci numbers
# where the first frame is mod 2, the second is mod 3,...
# output file "1 1.gif"
$ ./deficient "r[1,1]" 512 -10

# Generates a 10 frame GIF of the product table for the Fibonacci numbers
# where the first frame is mod 2, the second is mod 3,...
# output file "1 1.gif"
$ ./deficient "r[1,1]" 512 --10
```
