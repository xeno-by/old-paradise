package main

@identity
case class Test1()

@identity()
case class Test2()

@main.identity
case class Test3()

@main.identity()
case class Test4()

@main object TestMods {
  case class Message(msg: String)
  println(Message("hello world").msg)

  println(Test1().productPrefix)
  println(Test2().productPrefix)
  println(Test3().productPrefix)
  println(Test4().productPrefix)
}
