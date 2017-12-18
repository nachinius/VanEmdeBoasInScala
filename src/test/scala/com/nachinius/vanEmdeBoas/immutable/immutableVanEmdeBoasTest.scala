package com.nachinius.vanEmdeBoas.immutable

import com.nachinius.vanEmdeBoas.{vanEmdeBoas, vanEmdeBoasTest}

class immutableVanEmdeBoasTest extends vanEmdeBoasTest {
  def constructorVanEmdeBoas: Int => vanEmdeBoas = {
    ImmutableVanEmdeBoas(_)
  }
}


