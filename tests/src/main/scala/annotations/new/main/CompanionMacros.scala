package main

import scala.annotation.compileTimeOnly
import scala.meta._

@compileTimeOnly("@classMacro not expanded")
class classMacro extends scala.annotation.StaticAnnotation {

  inline def apply(classDefn: Defn.Class, companionDefn: Defn.Object): Stat = meta {
    val classTree: Stat = {
      val q"""
        ..$mods class $tname[..$tparams] ..$ctorMods (...$paramss) extends { ..$earlyStats } with ..$ctorcalls {
          $selfParam =>
          ..$stats
        }
      """ = classDefn
      q"""
        ..$mods class $tname[..$tparams] ..$ctorMods (...$paramss) extends { ..$earlyStats } with ..$ctorcalls {
          $selfParam =>
          ..$stats
        }
      """
    }

    val companionTree: Stat = {
      val q"""
        ..$mods object $tname extends { ..$earlyStats } with ..$ctorcalls {
          $selfParam =>
          ..$stats
        }
      """ = companionDefn
      q"""
        ..$mods object $tname extends { ..$earlyStats } with ..$ctorcalls {
          $selfParam =>
          ..$stats
        }
      """
    }

    Term.Block(scala.collection.immutable.Seq(classTree, companionTree))
  }

}
