class NewRepl extends ReplSuite {
  test("new macro annotations parse") {
    assert(repl("""
      |object main {
      |  inline def apply(x: Int)(defns: Any) = ???
      |}
    """.stripMargin.trim) === """
      |scala> object main {
      |  inline def apply(x: Int)(defns: Any) = ???
      |}
      |<console>:12: error: object meta is not a member of package scala
      |         inline def apply(x: Int)(defns: Any) = ???
      |         ^
    """.stripMargin.trim)
  }
}

