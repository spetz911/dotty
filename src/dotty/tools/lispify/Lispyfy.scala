package dotty.tools.lispify

import dotty.tools.dotc.ast.untpd._
import dotty.tools.dotc.core.Constants.Constant


object Lispyfy {


  var shift = 0

  def debug(tree: Any) = {
    print(tree + " ")
  }
  def shiftLn(): Unit = {
    println()
    print(" " * shift)
  }

  def shiftLeft(): Unit = {
    shift -= 4
  }

  def shiftRight(): Unit = {
    shift += 4
  }


  //  case class If[-T >: Untyped] private[ast] (cond: Tree[T], thenp: Tree[T], elsep: Tree[T])
  def procIf(tree: If) = {
    debug("(if")
    expression(tree.cond)
    shiftRight()
    shiftLn()
    expression(tree.thenp)
    shiftLn()
    expression(tree.elsep)
    shiftLeft()
    debug(")")
  }



  // case class Block[-T >: Untyped] private[ast] (stats: List[Tree[T]], expr: Tree[T])
  def procBlock(xx: Block) = {
    if (xx.stats.nonEmpty) {
      debug("(begin")
      shiftRight()
      for (el <- xx.stats) {
        shiftLn()
        expression(el)
      }
      shiftLn()
      expression(xx.expr)
      shiftLeft()
      debug(")")
    } else {
      expression(xx.expr)
    }
  }

  //  case class InfixOp(left: Tree, op: Name, right: Tree) extends OpTree
  def procInfixOp(tree: InfixOp) = {
    debug("(" + tree.op)
    shiftRight()
    expression(tree.left)
    expression(tree.right)
    shiftLeft()
    debug(")")
  }

  //  case class Apply[-T >: Untyped] private[ast] (fun: Tree[T], args: List[Tree[T]])
  def procApply(tree: Apply) = {
    debug("(" + (tree.fun match {case xx: Ident => xx.name}))
    for (arg <- tree.args) {
      expression(arg)
    }
    debug(")")
  }


  // case class ValDef[-T >: Untyped] private[ast] (name: TermName, tpt: Tree[T], rhs: Tree[T])
  def procValDef(tree: ValDef) = {
    debug("(ValDef " + tree.name)
    expression(tree.rhs)
    debug(")")
  }


  //  case class Function(args: List[Tree], body: Tree) extends Tree {
  def procFunction(tree: Function) = {
    shiftLn()
    debug("(Function (")
    for (param <- tree.args) {
      param match {
        case xx: ValDef => debug(xx.name)
        case _ => throw new Exception("procDefDef bad function param")
      }
    }
    debug(")")
      shiftRight()
      shiftLn()
      expression(tree.body)
      shiftLeft()
      shiftLn()
      print(")")

  }

//  case class CaseDef[-T >: Untyped] private[ast] (pat: Tree[T], guard: Tree[T], body: Tree[T])
  def procCaseDef(tree: CaseDef) = {
    debug("(case ")
  tree.pat match {
    case xx: Typed => procTyped(xx)
    case xx: Ident => debug(xx.name)
  }
    shiftLn()
    expression(tree.body)
    debug(")")
    shiftLn()
}

//  case class Match[-T >: Untyped] private[ast] (selector: Tree[T], cases: List[CaseDef[T]])
  def procMatch(tree: Match) = {
    shiftLn()
    debug("(Match ")
    expression(tree.selector)
    shiftRight()
    shiftLn()
    for (caseDef <- tree.cases) {
      procCaseDef(caseDef)
    }
    debug(")")
    shiftLeft()
    shiftLn()
}

//  case class Typed[-T >: Untyped] private[ast] (expr: Tree[T], tpt: Tree[T])
  def procTyped(tree: Typed) = {
    debug("(isInstanceOf")
    debug(tree.tpt)
    expression(tree.expr)
    debug(")")
  }

//  case class Select[-T >: Untyped] private[ast] (qualifier: Tree[T], name: Name)
  def procSelect(tree: Select): Unit = {
    debug("(Select")
    expression(tree.qualifier)
    debug(tree.name)
    debug(")")
  }

  def expression(tree: Tree): Unit = {
    tree match {
      case xx: Parens => expression(xx.t)
      case xx: Block => procBlock(xx)
      case xx: If => procIf(xx)
      case xx: InfixOp => procInfixOp(xx)
      case xx: Apply => procApply(xx)
      case xx: Ident => debug(xx.name)
      case xx: Select => procSelect(xx)
      case xx: Function => procFunction(xx)
      case xx: Match => procMatch(xx)
      case xx: Tree if xx.isEmpty => debug(xx)
      case xx: ValDef => procValDef(xx)
      case xx: Literal => xx.const match {
        case vv: Constant if vv.isNumeric => debug(vv.stringValue)
        case vv: Constant if !vv.isNumeric => debug("\"" + vv.stringValue + "\"")
      }
    }
  }


  def procParamsList(vparamss: List[List[ValDef]]) = {
    print("(")
    for (arg <- vparamss) {
      for (param <- arg) {
        param match {
          case xx: ValDef => debug(xx.name)
          case _ => throw new Exception("procDefDef bad function param")
        }
      }
    }
    print(")")
  }


  // case class DefDef(name: TermName, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], tpt: Tree[T], rhs: Tree[T])
  def procDefDef(tree: DefDef) = {
    //  println("fun tparams: " + tree.tparams) // U dont need this
    //    println("fun vparams: " + tree.vparamss)
    //    println("fun tpt: " + tree.tpt)
    shiftLn()
    debug("(defun " + tree.name)
    procParamsList(tree.vparamss)
    shiftRight()
    shiftLn()
    //  println("fun rhs: " + tree.rhs) // body block
    tree.rhs match {
      case xx: Block => procBlock(xx)
      case xx: Match => procMatch(xx); shiftLn
    }
    //    procBlock(.asInstanceOf[Block])
    print(")")
    shiftLeft()
    shiftLn()
  }

//  case class Template[-T >: Untyped] private[ast] (constr: DefDef[T], parents: List[Tree[T]], self: ValDef[T], body: List[Tree[T]])
  def procTemplate(xx: Template) = {
//    debug("constr: " + xx.constr.name)
    procParamsList(xx.constr.vparamss)
    shiftRight()
    shiftLn()
    debug("(extends")
    for(parent <- xx.parents) {
      debug(parent match {case xx: Ident => xx.name})
    }
    debug(")")
    shiftLn()
    for (tmp <- xx.body) {
      tmp match {
        case z: DefDef => procDefDef(z)
      }
    }
    shiftLeft()
  shiftLn()
  }

  //  case class TypeDef[-T >: Untyped] private[ast] (name: TypeName, rhs: Tree[T])
  def procTypeDef(tree: TypeDef) = {
    shiftLn()
    debug("(TypeDef " + tree.name )
    tree.rhs match {
      //      case class Template[-T >: Untyped] private[ast] (constr: DefDef[T], parents: List[Tree[T]], self: ValDef[T], body: List[Tree[T]])
      case xx: Template => {
        procTemplate(xx)
      }
    }
    debug(")")
    shiftLn()
  }

  def procObjectDef(modulo: ModuleDef) {
    shiftLn()
    debug("(ModuleDef " + modulo.name )
    procTemplate(modulo.impl)
    debug(")")
    shiftLn()
  }


  def procTopModuleDef(modulo: ModuleDef) {
    debug("(module " + modulo.name )
    modulo.impl match {
      case tmpl: Template => {
        for (x <- tmpl.body) {
          x match {
            case z: DefDef => procDefDef(z)
            case z: TypeDef => procTypeDef(z)
            case z: ModuleDef => procObjectDef(z)
          }
        }
      }
    }
    shiftLeft()
    shiftLn()
    debug(")")
    shiftLn()
  }


}
