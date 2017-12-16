package com.nachinius.vanEmdeBoas.versionA

import com.nachinius.vanEmdeBoas.{vanEmdeBoas, vanEmdeBoasTest}

class versionATest extends vanEmdeBoasTest {
  override def constructorVanEmdeBoas: Int => vanEmdeBoas = {
    vanEmdeBoasA(_)
  }
}