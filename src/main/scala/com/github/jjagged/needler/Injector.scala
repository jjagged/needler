package com.github.jjagged.needler

abstract class Injector {

  def get[T]: T
  def get[T](id: scala.Symbol): T
  def get[T](id: Identifier[T]): T
}
