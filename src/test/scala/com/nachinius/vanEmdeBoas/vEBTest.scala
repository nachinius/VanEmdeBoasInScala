package com.nachinius.vanEmdeBoas

import org.scalatest.FreeSpec

class vEBTest extends FreeSpec {

  "Methods tests" - {
    "insert" in {
      val a = vanEmdeBoas(8)
      assert(a.insert(3).member(3))
      assert(a.insert(4).member(3))
      assert(a.member(4))
      assert(a.insert(1 << 7  - 1).member( 1<<7-1))
    }


  }
}
