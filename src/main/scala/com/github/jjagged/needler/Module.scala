package com.github.jjagged.needler

import scala.reflect.runtime.universe.TypeTag
import scala.reflect.runtime.universe._

trait Module {
  //private[needler] val providerRegistry = Map[Identifier, Instantiator]()
  private[needler] val bindings = Map[Identifier[_], Binding[_]]()

  def configure(): Unit

  protected def bind[T]: Binding[T] = {
    val binding: Binding[T] = new Binding[T](weakTypeTag[T])
    // Check that constructor args match dependencies
    bindings + (binding.identifier -> binding)
    binding
  }
}

trait Identifiable[T] {
  def as(name: scala.Symbol): BindTarget[T]
}

trait BindTarget[T] {
  def to[V <: T](factory: Factory[V]): BindingArg
  def to[V <: T](f: => V): BindingArg
  def to[V <: T](instance: V): BindingArg
  def to[V <: T]: BindingArg
}

trait BindingArg {
  def args[T]: BindingArgs
  def args(id: scala.Symbol): BindingArgs
  def args[T](value: T): BindingArgs
}

trait BindingArgs {
  def + [T]: BindingArgs
  def + (id: scala.Symbol): BindingArgs
  def + [T] (value: T): BindingArgs
}

class Binding[T](tpeTag: WeakTypeTag[T]) extends BindTarget[T] with Identifiable[T] with BindingArg with BindingArgs {
  var name: Option[String] = None
  var identifier: Identifier[T] =  new TypeIdentifier[T](tpeTag)
  var targetType: WeakTypeTag[_] =  tpeTag
  var factory: Factory[_] = new ConstructorFactory[T, T]
  var dependencies: Seq[Dependency] = Seq()

  override def as(nm: scala.Symbol): BindTarget[T] = {
    name = Some(nm.toString())
    identifier = new NamedIdentifier[T](nm.toString(), tpeTag)
    this
  }

  override def to[V <: T](fact: Factory[V]): BindingArg = {
    factory = fact
    targetType = weakTypeTag[V]
    this
  }

  override def to[V <: T](f: => V): BindingArg = {
    factory = new FunctionFactory[V](f)
    targetType = weakTypeTag[V]
    this
  }

  override def to[V <: T](instance: V): BindingArg = {
    factory = new InstanceFactory[T](instance)
    targetType = weakTypeTag[V]
    this
  }

  override def to[V <: T]: BindingArg = {
    factory = new ConstructorFactory[T, V]
    targetType = weakTypeTag[V]
    this
  }

  override def args[A]: BindingArgs = {
    dependencies :+ new TypeDependency(weakTypeTag[A])
    this
  }

  override def args(id: scala.Symbol): BindingArgs = {
    dependencies :+ new IdentifierDependency(id)
    this
  }

  override def args[A](value: A): BindingArgs = {
    dependencies :+ new ValueDependency(value)
    this
  }

  override def + [A]: BindingArgs = {
    args[A]
    this
  }

  override def + (id: scala.Symbol): BindingArgs = {
    args(id)
    this
  }

  override def + [A] (value: A): BindingArgs = {
    args[A](value)
    this
  }

  override def toString(): Unit = {
    println(name)
    println(identifier)
    println(factory)
    println(targetType)
    println(dependencies)
  }
}


abstract class Dependency
case class ValueDependency[T](value: T)
case class IdentifierDependency(name: scala.Symbol)
case class TypeDependency[T](tpeTag: WeakTypeTag[T])


trait Identifier[T <: AnyRef]
case class TypeIdentifier[T <: AnyRef](tpeTag: WeakTypeTag[T]) extends Identifier[T]
case class NamedIdentifier[T <: AnyRef](name: String, tpeTag: WeakTypeTag[T]) extends Identifier[T]

//case class Instantiator(val provider: Provider, val dependencies: Seq[Identifier])