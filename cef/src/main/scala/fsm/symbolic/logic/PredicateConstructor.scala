package fsm.symbolic.logic

import scala.reflect.runtime.{universe => ru}
import fsm.symbolic.sfa.Constants._

class PredicateConstructor {

}

object PredicateConstructor {

  /**
    * Creates a new predicate using reflection. There must be an implementation of the predicate with the given name
    * under fsm.symbolic.sfa.Constants$#predicatesClassPrefix()
    *
    * @param name The predicate's name. A class with exactly the same name must exist under
    *             fsm.symbolic.sfa.Constants$#predicatesClassPrefix()
    * @param arguments Any possible arguments that may need to be passed to the predicate.
    * @return If reflection can find a proper implementation class, an instance of this class is returned.
    */
  def getNewPredicateInstance(
                               name: String,
                               arguments: List[Any]
                             ): Predicate = {
    val fullName = predicatesClassPrefix + name
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val classSymbol = m.staticClass(fullName)
    val classMirror = m.reflectClass(classSymbol)
    //val ctor = ru.typeOf[EventTypePredicate].declaration(ru.nme.CONSTRUCTOR).asMethod
    //val ctorm = classMirror.reflectConstructor(ctor)
    //ctorm(arguments).asInstanceOf[Predicate]
    val constructorSymbol = classMirror.symbol.toType.members.find {
      item => item.isMethod && item.asMethod.isConstructor
    }
    val pred = classMirror.reflectConstructor(constructorSymbol.get.asMethod)
    pred(arguments).asInstanceOf[Predicate]

  }

  /*def getClassInstance(clsName: String): Any = {
    //val m = ru.runtimeMirror(getClass.getClassLoader)
    val mirror = ru.runtimeMirror(getClass.getClassLoader)
    val cls = mirror.classSymbol(Class.forName(clsName))
    val module = cls.companionSymbol.asModule
    mirror.reflectModule(module).instance
  }*/

  /**
    * Creates a new predicate that always evaluates to true, even without an event.
    * @return An epsilon predicate.
    */
  def getEpsilonPred: Predicate = getNewPredicateInstance(epsilonPredicate, List.empty)

  /**
    * Creates a new predicate that always evaluates to true, with any event.
    * @return A true predicate.
    */
  def getTruePred: Predicate = getNewPredicateInstance(truePredicate, List.empty)

  /**
    * Creates a predicate that evaluates to true if the event has a type that is equal to the given event type.
    * @param eventTypeSymbol The given event type.
    * @return A predicate that checks the event type.
    */
  def getEventTypePred(eventTypeSymbol: String): Predicate =
    getNewPredicateInstance(eventTypePredicate, List(eventTypeSymbol))

}
