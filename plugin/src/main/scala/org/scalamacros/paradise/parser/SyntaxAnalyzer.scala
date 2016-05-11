package org.scalamacros.paradise
package parser

import scala.language.reflectiveCalls
import scala.tools.nsc.ast.parser.{SyntaxAnalyzer => NscSyntaxAnalyzer}
import scala.tools.nsc.ast.parser.BracePatch
import scala.tools.nsc.ast.parser.Tokens._
import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.reflect.internal.Flags
import scala.collection.mutable.ListBuffer

abstract class SyntaxAnalyzer extends NscSyntaxAnalyzer {
  import global._

  val runsAfter = List[String]()
  val runsRightAfter = None
  override val initial = true

  private def initialUnitBody(unit: CompilationUnit): Tree = {
    if (unit.isJava) new JavaUnitParser(unit).parse()
    else if (currentRun.parsing.incompleteHandled) new ParadiseUnitParser(unit).parse()
    else new ParadiseUnitParser(unit).smartParse()
  }

  def newUnitParser(unit: CompilationUnit): UnitParser = new ParadiseUnitParser(unit)
  private class ParadiseUnitParser(unit: global.CompilationUnit, patches: List[BracePatch]) extends UnitParser(unit, Nil) {
    def this(unit: global.CompilationUnit) = this(unit, Nil)
    // override def withPatches(patches: List[BracePatch]): UnitParser = new UnitParser(unit, patches)

    private val INLINEkw = TermName("inline")
    private def isInline = in.token == IDENTIFIER && in.name == INLINEkw && skippingModifiers(in.token == DEF)
    private def skippingModifiers[T](op: => T): T = lookingAhead(if (isModifier) lookingAhead(skippingModifiers(op)) else op)
    override def isExprIntroToken(token: Token) = !isInline && super.isExprIntroToken(token)
    override def isDclIntro: Boolean = isInline || super.isDclIntro

    private def markInline(offset: Offset, mods: Modifiers): Modifiers = {
      val metaInlineAnnot = rootMirror.getClassIfDefined("scala.meta.internal.inline.inline")
      if (metaInlineAnnot != NoSymbol) mods.withAnnotations(List(atPos(offset)(New(metaInlineAnnot))))
      else { syntaxError(offset, "new-style (\"inline\") macros require scala.meta"); mods }
    }

    private def invoke(name: String, args: Any*): Any = {
      val meth = classOf[Parser].getDeclaredMethods().find(_.getName == name).get
      meth.setAccessible(true)
      meth.invoke(this, args.asInstanceOf[Seq[AnyRef]]: _*)
    }
    private def normalizeModifiers(mods: Modifiers): Modifiers = invoke("normalizeModifiers", mods).asInstanceOf[Modifiers]
    private def addMod(mods: Modifiers, mod: Long, pos: Position): Modifiers = invoke("addMod", mods, mod, pos).asInstanceOf[Modifiers]
    private def tokenRange(token: TokenData): Position = invoke("tokenRange", token).asInstanceOf[Position]
    private def flagTokens: Map[Int, Long] = invoke("flagTokens").asInstanceOf[Map[Int, Long]]
    override def modifiers(): Modifiers = normalizeModifiers {
      def loop(mods: Modifiers): Modifiers = in.token match {
        case IDENTIFIER if isInline =>
          val offset = in.offset
          in.nextToken()
          loop(markInline(offset, mods))
        case PRIVATE | PROTECTED =>
          loop(accessQualifierOpt(addMod(mods, flagTokens(in.token), tokenRange(in))))
        case ABSTRACT | FINAL | SEALED | OVERRIDE | IMPLICIT | LAZY =>
          loop(addMod(mods, flagTokens(in.token), tokenRange(in)))
        case NEWLINE =>
          in.nextToken()
          loop(mods)
        case _ =>
          mods
      }
      loop(NoMods)
    }
    override def localModifiers(): Modifiers = {
      def loop(mods: Modifiers): Modifiers = {
        if (isInline) { val offset = in.offset; in.nextToken(); loop(markInline(offset, mods)) }
        else if (isLocalModifier) loop(addMod(mods, flagTokens(in.token), tokenRange(in)))
        else mods
      }
      loop(NoMods)
    }
  }

  override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
    override val checkable = false
    override val keepsTypeParams = false

    def apply(unit: CompilationUnit) {
      informProgress("parsing " + unit)
      // if the body is already filled in, don't overwrite it
      // otherwise compileLate is going to overwrite bodies of synthetic source files
      if (unit.body == EmptyTree)
        unit.body = initialUnitBody(unit)

      if (settings.Yrangepos && !reporter.hasErrors)
        validatePositions(unit.body)

      if (settings.Ymemberpos.isSetByUser)
        new MemberPosReporter(unit) show (style = settings.Ymemberpos.value)
    }
  }
}