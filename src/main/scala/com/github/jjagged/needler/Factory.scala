package com.github.jjagged.needler

import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.universe._

trait Factory[T] {
  def create(dependencies: Seq[Any]): T
}

class ConstructorFactory[T, C <: T] extends Factory[T] {
  private[needler] val tpe = weakTypeOf[C]
  private[needler] val ctor = tpe.member(nme.CONSTRUCTOR).asMethod
  private[needler] val cm = ru.runtimeMirror(getClass.getClassLoader).reflectClass(tpe.typeSymbol.asClass)

  override def create(dependencies: Seq[_]): T = {
    cm.reflectConstructor(ctor).apply(dependencies).asInstanceOf[T]
  }
}

class FunctionFactory[T](f: => T) extends Factory[T] {
  override def create(dependencies: Seq[_]): T = {
    if (dependencies.size > 0) throw new ConfigurationException("You supplied dependencies to a factory method binding. Did you really mean to do that?")
    f
  }
}

class InstanceFactory[T](v: T) extends Factory[T] {
  override def create(dependencies: Seq[_]): T = {
    if (dependencies.size > 0) throw new ConfigurationException("You supplied dependencies to an instance binding. Did you really mean to do that?")
    v
  }
}

