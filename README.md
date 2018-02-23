[![Build Status](https://travis-ci.org/nachinius/VanEmdeBoasInScala.svg?branch=master)](https://travis-ci.org/nachinius/VanEmdeBoasInScala)
[![codecov](https://codecov.io/gh/nachinius/VanEmdeBoasInScala/branch/master/graph/badge.svg)](https://codecov.io/gh/nachinius/VanEmdeBoasInScala)
[![Join the chat at https://gitter.im/nachinius/van_emde_boas_in_scala](https://badges.gitter.im/nachinius/van_emde_boas_in_scala.svg)](https://gitter.im/nachinius/van_emde_boas_in_scala?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Latest version](https://index.scala-lang.org/nachinius/vanemdeboasinscala/van-emde-boas-tree/latest.svg)](https://index.scala-lang.org/nachinius/vanemdeboasinscala/van-emde-boas-tree)

# modified van Emde Boas Tree 
#### the fastest successor/predecessor for integers set

A modified van Emde Boas data structure that achieves time complexity for successor/predecessor/insert/search/delete in `O(lg w)` (with high probability), where `w` is the bits of the word used to store the integer. Space complexity is `O(n)`, where `n` is how many numbers are stored. Thus, the constrain is that `lg n <= w`, since any number must fit in a word.

If `w` is not too high, then this is optimal. In case `w` is too high, fusion trees are the best. However, dynamic versions of fusion tree only achieve expected `O(lg n/lg w)`.

When `lg w >= sqrt(lg(n))` is better to use `fusion trees`. However, for `dinamyc fusion trees` only **expected** `O(lg n/lg w)` has been achieved.

For example, in a static case, with a 32-bit word, `w = 32` (as this current implementation that uses scala's Int which is 32-bit signed integer), static van Emde Boas should be used if `n >~ 2^25 ~ 33 millon `. Otherwise, static `fusion trees` should be preferred. 
In the same scenario, static, for 64-bit word, `w = 64`, this structure is better than fusion trees if `n >~ 68 billon `. You're likely to have memory problems with such big numbers, and you may want to switch to a distributed system. Due to its recursive nature, I imagine this structure is easy to implement in a distributed way.

The current implementation, handles scala's Int which is a signed 32-bit number. However, th

#### time complexity (with high probability)
- search: O(lg w) 
- successor: O(lg w) 
- predecessor: O(lg w)
- delete: O(lg w)
- insert: O(lg w)

where **n = 2^w** is the maximum number that can be stored.

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
- [ ] Allow any ordering
- [ ] Augment the data structure
- [ ] Compare to fusion tree
- [ ] Compare to `O(lg n)` successor/predecessor of trees
- [ ] Present performance results

#### features
- immutable or mutable
- dynamic
- fully tested
- with benchmarks code 

### References
1. [MIT 6.851 Advanced Data Structures, Erik Demaine lecture 11](https://courses.csail.mit.edu/6.851/fall17/lectures/L11.html)


