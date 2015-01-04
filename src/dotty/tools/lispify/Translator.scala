/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */
package dotty.tools.lispify

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd._
import dotty.tools.dotc.core.Contexts.{ContextBase, Context}
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.util.{NoSource, SourceFile}

import scala.reflect.io.PlainFile




object Translator {

  import Lispyfy._

  protected def initCtx = (new ContextBase).initialCtx


  def main(args: Array[String]): Unit = {
    implicit val ctx: Context = initCtx.fresh

    def getSource(fileName: String): SourceFile = {
      val f = new PlainFile(fileName)
      if (f.exists) new SourceFile(f)
      else {
        ctx.error(s"not found: $fileName")
        NoSource
      }
    }

//    val fileName = "/Users/oleg/IdeaProjects/dotty-master/examples/hello.scala"

    val fileName = "/Users/oleg/HelloWorld.scala"

    val sourceCode = getSource(fileName)

    val unitX = new CompilationUnit(sourceCode)

    val tree = new Parser(unitX.source).parse()

    println("stage 1:")
    println(tree.show)

    println("STAGE 2:")
    println("------------------------------------------------------------------------")

    tree match {
      case tree: PackageDef => {
        println("PackageDef.pid: " + tree.pid)
        println()
        for (ss <- tree.stats) {
          procTopModuleDef(ss.asInstanceOf[ModuleDef])
          }
        }
    }

    println("------------------------------------------------------------------------")

  }


}
