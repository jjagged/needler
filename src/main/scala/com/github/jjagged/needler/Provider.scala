package com.github.jjagged.needler

trait Provider[T] {
  def get: T
}

class SingletonInstanceProvider[T](val instance: T) extends Provider[T] {
  override def get = instance
}

class SingletonFactoryProvider[T](private[needler] val injector: Injector,
                                  private[needler] val factory: Factory[T],
                                  private[needler] val dependencies: Seq[Identifier[_]]) extends Provider[T] {
  private lazy val instance = factory.create(dependencies.map(id => injector.get(id)))

  override def get = instance
}

class FactoryProvider[T](private[needler] val injector: Injector,
                         private[needler] val factory: Factory[T],
                         private[needler] val dependencies: Seq[Identifier[_]]) extends Provider[T] {
  override def get = factory.create(dependencies.map(id => injector.get(id)))
}
