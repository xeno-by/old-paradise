package org.scalamacros.paradise
package parser

import scala.tools.nsc.{Global => NscGlobal, Phase, SubComponent, Settings => NscSettings}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.collection.mutable
import org.scalamacros.paradise.parser.{SyntaxAnalyzer => ParadiseSyntaxAnalyzer}
import scala.reflect.internal.util.BatchSourceFile

trait HijackSyntaxAnalyzer {
  self: NscPlugin =>

  def hijackSyntaxAnalyzer(): Unit = {
    val newSyntaxAnalyzer = new { val global: self.global.type = self.global } with ParadiseSyntaxAnalyzer
    val syntaxAnalyzerField = classOf[NscGlobal].getDeclaredField("syntaxAnalyzer")
    syntaxAnalyzerField.setAccessible(true)
    syntaxAnalyzerField.set(global, newSyntaxAnalyzer)

    val phasesDescMapGetter = classOf[NscGlobal].getDeclaredMethod("phasesDescMap")
    val phasesDescMap = phasesDescMapGetter.invoke(global).asInstanceOf[mutable.Map[SubComponent, String]]
    val phasesSetMapGetter = classOf[NscGlobal].getDeclaredMethod("phasesSet")
    val phasesSet = phasesSetMapGetter.invoke(global).asInstanceOf[mutable.Set[SubComponent]]
    if (phasesSet.exists(_.phaseName == "parser")) { // `scalac -help` doesn't instantiate standard phases
      phasesSet -= phasesSet.find(_.phaseName == "parser").head
      phasesSet += newSyntaxAnalyzer
      phasesDescMap(newSyntaxAnalyzer) = "parse source into ASTs (with support for inline defs)"
    }
  }
}