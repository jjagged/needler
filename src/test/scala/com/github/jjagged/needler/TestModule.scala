package com.github.jjagged.needler

import org.scalatest.WordSpec

class Test extends WordSpec {
  val module = new TestModule

  module.configure()

  "A module" when {
    "ever" should {
      "print toString" in {
        module.bindings.foreach(_.toString())
      }
    }
  }
}

class TestModule extends Module {

  override def configure(): Unit = {
    bind[YetAnother] as 'YetAnother
    bind[TestTrait]
      .as('MyTestClass)
      .to[TestClass]
      .args("String")
      .+[OtherClass]
      .+('YetAnother)

    bind[AutoInject] as 'AutoInject
  }
}

trait TestTrait

class TestClass(arg1: String, arg2: OtherClass, arg3: YetAnother) extends TestTrait

class OtherClass

class YetAnother

class AutoInject(arg1: OtherClass, arg2: YetAnother)