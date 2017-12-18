package com.nachinius.vanEmdeBoas.mutable

import com.nachinius.vanEmdeBoas.{vanEmdeBoas, vanEmdeBoasTest}

class mutableVanEmdeBoasTest extends vanEmdeBoasTest {
  override def constructorVanEmdeBoas: Int => vanEmdeBoas = {
    mutableVanEmdeBoas(_)
  }
}