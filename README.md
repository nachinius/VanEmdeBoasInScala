[![Build Status](https://travis-ci.org/nachinius/VanEmdeBoasInScala.svg?branch=master)](https://travis-ci.org/nachinius/VanEmdeBoasInScala)
[![codecov](https://codecov.io/gh/nachinius/VanEmdeBoasInScala/branch/master/graph/badge.svg)](https://codecov.io/gh/nachinius/VanEmdeBoasInScala)
[![Join the chat at https://gitter.im/nachinius/van_emde_boas_in_scala](https://badges.gitter.im/nachinius/van_emde_boas_in_scala.svg)](https://gitter.im/nachinius/van_emde_boas_in_scala?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

last release:[![Latest version](https://index.scala-lang.org/nachinius/vanemdeboasinscala/van-emde-boas-tree/latest.svg)](https://index.scala-lang.org/nachinius/vanemdeboasinscala/van-emde-boas-tree)

# modified van Emde Boas Tree 
#### the fastest successor/predecessor for integers set

A modified van Emde Boas data structure that achieves `O(lg w)` (with high probability), where `w` is the bits of the word used to store the integer. Space used is `O(n)`, where `n` is how many number are stored. The constrain is that `lg n <= w`.

If `w` is not too high, then this is optimal. In case `w` is too high, fusion trees are the best.

When `lg w >= sqrt(lg(n))` is better to use `fusion trees`.

For example, with 32-bit word `w = 32` (as the current implemenation that uses scala's Int which is 32-bit signed integer), van Emde Boas should be used if `n >~ 2^25 ~ 33 millon `. 
For 64-bit word, using Long (not implemented), `w = 64` and this structure is better than fusion trees if `n >~ 68 billon `. You're likely to have memory problems with such big numbers, and you may want to switch to a distributed system. Due to its recursive nature, I imagine this structure is easy to implement in a distributed way.

#### time complexity (with high probability)
- search: O(lg w) 
- successor: O(lg w) 
- predecessor: O(lg w)
- delete: O(lg w)
- insert: O(lg w)

where **n** is the maximum number capable of store.

### space complexity
- O(n)

###### may TODO
- [x] Create performance tests
- [x] Measure memory consumption againt data size (n) and bit (w)
- [x] Create a complete immutable version
- [x] Improve usage of generators for performance tests 
- [x] Do performance tests for Immutable Version
- [x] Compare performance between mutable and immutable 
- [x] Write predecessor  
- [x] Add delete
- [ ] Compare to fusion tree
- [ ] ? Augment the data structure
- [ ] Present performance results

#### features
- immutable or mutable
- fully tested
- with benchmarks code 

### References
[1] MIT 6.851 Advanced Data Structures, lecture 11


