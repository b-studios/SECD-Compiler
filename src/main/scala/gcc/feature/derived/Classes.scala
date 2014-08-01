package gcc
package feature
package derived

import scala.collection.mutable
import scala.language.implicitConversions

trait Classes extends Syntax { self: Labeling with DerivedSyntax =>

  type ClassTag = Int
  private val classTags = mutable.Map.empty[Symbol, ClassTag]
  private val classMethods = mutable.Map.empty[ClassTag, Seq[Symbol]]

  def class_(name: Symbol)(fields: Symbol*)(members: (Symbol, Term)*) = {

    val memberNames = members.map { _._1 }
    val memberRefs = memberNames.map(m => term(m))

    val classTag = classTags.size
    classTags.update(name, classTag)
    classMethods.update(classTag, memberNames)

    // we return the constructor
    Symbol("new_" + name.name) -> lam(fields: _*){
      letrec( members :+ ('this := lam(){ tuple(classTag, memberRefs: _*) }) : _* )('this())
    }
  }

  implicit class MethodCallOps[T <% Term](term: T) {
    def call(className: Symbol, methodName: Symbol)(args: Term*) = {
      val classTag = classTags(className)
      val methodList = classMethods(classTag)
      val idx = methodList.indexOf(methodName)

      if (idx == -1) sys error s"Cannot call method $methodName on $className, available methods: ${methodList mkString ", "}"
      term.at(idx + 2, methodList.size + 1).apply(args: _*)
    }
  }

  def getter(field: Symbol) = fun(Symbol("get" + field.name.capitalize))() { field }
  def setter(field: Symbol) = fun(Symbol("set" + field.name.capitalize))('$value) { field <~ '$value }
}