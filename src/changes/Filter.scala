package changes

import objects._


abstract class Filter {
  def apply : PartialFunction[Effect, (Effect) => Effect]
}

object Filter {
  
  def identity : Effect => Effect = x => x
  
  def partial_identity : PartialFunction[Effect, (Effect) => Effect] = 
    { case _ => identity }

  def total (f : Filter) = f.apply orElse partial_identity

  def compose (f1 : Effect => Effect, f2 : Effect => Effect) 
  : Effect => Effect = { f1 compose f2 }

  def fun (c : Effect, f : Filter) : (Effect) => Effect = {
    total(f)(c)
  }
}
