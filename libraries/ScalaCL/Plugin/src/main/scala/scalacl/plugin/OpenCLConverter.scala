/*
 * ScalaCL - putting Scala on the GPU with JavaCL / OpenCL
 * http://scalacl.googlecode.com/
 *
 * Copyright (c) 2009-2010, Olivier Chafik (http://ochafik.free.fr/)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of Olivier Chafik nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY OLIVIER CHAFIK AND CONTRIBUTORS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE REGENTS AND CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package scalacl ; package plugin
import com.nativelibs4java.scalaxy._
import common._
import pluginBase._
import components._

import scala.collection.immutable.Stack
import scala.reflect.NameTransformer
import scala.reflect.generic.{Names, Trees, Types, Constants, Universe}
import scala.tools.nsc.Global
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.plugins.PluginComponent
trait OpenCLConverter
extends MiscMatchers 
   with CodeFlattening 
{
  this: PluginComponent with WithOptions =>
  
  val global: Global
  import global._
  import definitions._

  def nodeToStringNoComment(tree: Tree): String
  
  var openclLabelIds = new Ids
  
  var placeHolderRefs = new Stack[String]
  
  val randomName = N("random")
  val randomIntName = N("randomInt")
  
  val cl_khr_fp64 = "#pragma OPENCL EXTENSION cl_khr_fp64: enable"

  def valueCode(v: String) = FlatCode[String](Seq(), Seq(), Seq(v))
  def emptyCode = FlatCode[String](Seq(), Seq(), Seq())
  def statementCode(s: String) = FlatCode[String](Seq(), Seq(s), Seq())

  /**
   * Returns <code>FlatCode[String]</code> representation of <code>body</code> and
   * a boolean representing whether this tree needs a random number
   * generator.
   */
  def convert(body: Tree): (FlatCode[String], Boolean) = {
    def cast(expr: Tree, clType: String) = {
      val (exprConv, usesRandom) = convert(expr)
      (exprConv.mapEachValue(v => Seq("((" + clType + ")" + v + ")")), usesRandom)
    }
      /*
    def convertForeach(from: Tree, to: Tree, isUntil: Boolean, by: Tree, function: Tree) = {
        val Function(List(vd @ ValDef(paramMods, paramName, tpt, rhs)), body) = function
        val id = openclLabelIds.next
        val iVar = "iVar$" + id
        val nVal = "nVal$" + id

        out("int ", iVar, ";\n")
        out("const int ", nVal, " = ", to, ";\n")
        out("for (", iVar, " = ", from, "; ", iVar, " ", if (isUntil) "<" else "<=", " ", nVal, "; ", iVar, " += ", by, ") {\n")
        doConvertExpr(argNames + (vd.symbol -> iVar), body, false, conversion, b)._1
        out("\n}")
    }*/
    body match {
      case TupleCreation(tupleArgs) =>//Apply(TypeApply(Select(TupleObject(), applyName()), tupleTypes), tupleArgs) if isTopLevel =>
        val (convTupleArgs, usesRandom) = convertTrees(tupleArgs)
        val flatCode = convTupleArgs.reduceLeft(_ ++ _)
        (flatCode, usesRandom)
      case Literal(Constant(value)) =>
        if (value == ())
          (emptyCode, false)
        else
          (valueCode(value.toString), false)
      case Ident(name) =>
        (valueCode(name.toString), false)

      case If(condition, then, otherwise) =>
        // val (a, b) = if ({ val d = 0 ; d != 0 }) (1, d) else (2, 0)
        // ->
        // val d = 0
        // val condition = d != 0
        // val a = if (condition) 1 else 2
        // val b = if (condition) d else 0
        val (FlatCode(dc, sc, Seq(vc)), randc) = convert(condition)
        val (fct @ FlatCode(Seq(), st, vt), randt) = convert(then)
        val (fco @ FlatCode(Seq(), so, vo), rando) = convert(otherwise)
        val usesRandom = randc || randt || rando
        
        def newIf(t: String, o: String, isValue: Boolean) =
          if (isValue)
            "((" + vc + ") ? (" + t + ") : (" + o + "))"
          else
            "if (" + vc + ") {\n" + t + "\n} else {\n" + o + "\n}\n"

        val (rs, rv) = (st, so) match {
          case (Seq(), Seq()) if !vt.isEmpty && !vo.isEmpty =>
            (
              Seq(),
              vt.zip(vo).map { case (t, o) => newIf(t, o, true) } // pure (cond ? then : otherwise) form, possibly with tuple values
            )
          case _ =>
            (
              Seq(newIf((st ++ vt).mkString("\n"), (so ++ vo).mkString("\n"), false)),
              Seq()
            )
        }
        (FlatCode[String](dc, sc ++ rs, rv), usesRandom)
      case Apply(Select(target, applyName()), List(singleArg)) =>
        val (converted, usesRandom) = convertTrees(Seq(target, singleArg))
        val flatCode = merge(converted:_*) { case Seq(t, a) => Seq(t + "[" + a + "]") }
        (flatCode, usesRandom)
      case Apply(Select(target, updateName()), List(index, value)) =>
        val (converted, usesRandom) = convertTrees(Seq(target, index, value))
        val flatCode = merge(converted:_*) { case Seq(t, i, v) => Seq(t + "[" + i + "] = " + v) }
        (flatCode, usesRandom)
      case Assign(lhs, rhs) =>
        val (converted, usesRandom) = convertTrees(Seq(lhs, rhs))
        val flatCode = merge(converted:_*) { case Seq(l, r) => Seq(l + " = " + r + ";") }
        (flatCode, usesRandom)
      case Typed(expr, tpt) =>
        val t = convertTpe(tpt.tpe)
        val (exprConv, usesRandom) = convert(expr)
        val flatCode = exprConv.mapValues(_.map(v => "((" + t + ")" + v + ")")) 
        (flatCode, usesRandom)
      case DefDef(mods, name, tparams, vparamss, tpt, body) =>
        val b = new StringBuilder
        b ++= convertTpe(body.tpe) + " " + name + "("
        var first = true
        for (param <- vparamss.flatten) {
          if (first)
            first = false
          else
            b ++= ", "
          b ++= constPref(param.mods) + convertTpe(param.tpt.tpe) + " " + param.name
        }
        b ++= ") {\n"
        val (convBody, usesRandom) = convert(body)
        convBody.statements.foreach(b ++= _)
        if (!convBody.values.isEmpty) {
          val Seq(ret) = convBody.values
          b ++= "return " + ret + ";"
        }
        b ++= "\n}"
        (FlatCode[String](convBody.outerDefinitions :+ b.toString, Seq(), Seq()), usesRandom)
      case vd @ ValDef(paramMods, paramName, tpt: TypeTree, rhs) =>
        val (convValue, usesRandom) = convert(rhs)
        val flatCode = FlatCode[String](
          convValue.outerDefinitions,
          convValue.statements ++
          Seq(
            constPref(paramMods) + convertTpt(tpt) + " " + paramName + (
              if (rhs != EmptyTree) {
                val Seq(value) = convValue.values
                " = " + value
              } else 
                ""
            ) + ";"
          ),
          Seq()
        )
        (flatCode, usesRandom)
      //case Typed(expr, tpe) =>
      //  out(expr)
      case Match(ma @ Ident(matchName), List(CaseDef(pat, guard, body))) =>
        //for ()
        //x0$1 match {
        //  case (_1: Long,_2: Float)(Long, Float)((i @ _), (c @ _)) => i.+(c)
        //}
        //Match(Ident("x0$1"), List(CaseDef(Apply(TypeTree(), List(Bind(i, Ident("_")), Bind(c, Ident("_"))), EmptyTree Apply(Select(Ident("i"), "$plus"), List(Ident("c")
        convert(body)
      case Apply(s @ Select(expr, randomIntName()), List(n)) if (isPackageReference(expr, "scalacl.math")) =>
        /* scalacl.math.randomInt(n) (uniform distribution for ints between 0 and n-1 inclusively) */
        val (nConv, nUsesRandom) = convert(n)
        val nConvStr = nConv.values.last
        (FlatCode[String](Seq(cl_khr_fp64), Seq(), Seq(MWC64X_RNG.mwc64xIntRangeValueCode(nConvStr))), true)
      case s @ Select(expr, randomIntName()) if (isPackageReference(expr, "scalacl.math")) =>
        /* scalacl.math.randomInt (uniform distribution for signed ints) */
        (FlatCode[String](Seq(cl_khr_fp64), Seq(), Seq(MWC64X_RNG.mwc64xIntValueCode)), true)
      case s @ Select(expr, randomName()) if (isPackageReference(expr, "scala.math") || isPackageReference(expr, "scalacl.math")) =>
        /* scala.math.random (uniform distribution for doubles from 0 to 1) */
        (FlatCode[String](Seq(cl_khr_fp64), Seq(), Seq(MWC64X_RNG.mwc64xDoubleValueCode)), true)
      case Select(s @ Select(expr, randomName()), toFloatName()) if (isPackageReference(expr, "scala.math") || isPackageReference(expr, "scalacl.math")) =>
        /* scala.math.random (casted to float for systems without double support) */
        (FlatCode[String](Seq(), Seq(), Seq(MWC64X_RNG.mwc64xFloatValueCode)), true)
      case Select(expr, toSizeTName()) => cast(expr, "size_t")
      case Select(expr, toLongName()) => cast(expr, "long")
      case Select(expr, toIntName()) => cast(expr, "int")
      case Select(expr, toShortName()) => cast(expr, "short")
      case Select(expr, toByteName()) => cast(expr, "char")
      case Select(expr, toCharName()) => cast(expr, "short")
      case Select(expr, toDoubleName()) => cast(expr, "double")
      case Select(expr, toFloatName()) => cast(expr, "float")
      case ScalaMathFunction(functionType, funName, args) =>
        (convertMathFunction(functionType, funName, args), false)
      case Apply(s @ Select(left, name), args) =>
        val List(right) = args
        NameTransformer.decode(name.toString) match {
          case op @ ("+" | "-" | "*" | "/" | "%" | "^" | "^^" | "&" | "&&" | "|" | "||" | "<<" | ">>" | "==" | "<" | ">" | "<=" | ">=" | "!=") =>
            val (converted, usesRandom) = convertTrees(Seq(left, right))
            val flatCode = merge(converted:_*) {
              case Seq(l, r) => Seq("(" + l + " " + op + " " + r + ")")
            }
            (flatCode, usesRandom)
          case n if isPackageReference(left, "scala.math") =>
            (convertMathFunction(s.tpe, name, args), false)
            //merge(Seq(right).map(convert):_*) { case Seq(v) => Seq(n + "(" + v + ")") }
          case n =>
            println(nodeToStringNoComment(body))
            throw new RuntimeException("[ScalaCL] Unhandled method name in Scala -> OpenCL conversion : " + name + "\n\tleft = " + left + ",\n\targs = " + args)
            (valueCode("/* Error: failed to convert " + body + " */"), false)
        }
      case s @ Select(expr, fun) =>
        val (exprConv, usesRandom) = convert(expr)
        val flatCode = exprConv.mapEachValue(v => {
          val fn = fun.toString
          if (fn.matches("_\\d+")) {
            Seq(v + "." + fn)
          } else {
            throw new RuntimeException("Unknown function " + s)
            Seq("/* Error: failed to convert " + body + " */")
          }
        })
        (flatCode, usesRandom)
      case WhileLoop(condition, content) =>
        /* Convert the content */
        val (contentConvSeq, contentUsesRandom) = convertTrees(content)
        val FlatCode(dcont, scont, vcont) = contentConvSeq.reduceLeft(_ >> _)
        
        /* Convert the condition */
        val (FlatCode(dcond, scond, Seq(vcond)), conditionUsesRandom) = convert(condition)

        val usesRandom = contentUsesRandom || conditionUsesRandom
        val flatCode = FlatCode[String](
          dcond ++ dcont,
          scond ++
          Seq(
            "while (" + vcond + ") {\n" +
              (scont ++ vcont).mkString("\n") + "\n" +
            "}"
          ),
          Seq()
        )
        
        (flatCode, usesRandom)
      case Apply(target, args) =>
        val (convSeq, usesRandom) = convertTrees(target :: args)
        val flatCode = merge(convSeq:_*)(seq => {
          val t :: a = seq.toList
          Seq(t + "(" + a.mkString(", ") + ")") 
        })
        (flatCode, usesRandom)
      case Block(statements, Literal(Constant(empty))) =>
        assert(empty == (), "Valued blocks should have been flattened in a previous phase !")
        val (convStatements, usesRandom) = convertTrees(statements)
        val flatCode = convStatements.map(_.noValues).reduceLeft(_ >> _)
        (flatCode, usesRandom)
      case t =>
        //println(nodeToStringNoComment(body))
        throw new RuntimeException("Failed to convert " + body.getClass.getName + ": \n" + body + " : \n" + nodeToStringNoComment(body))
    }
  }
  
  def convertTrees(trees:Seq[Tree]):(Seq[FlatCode[String]], Boolean) = {
    val (convTrees, usesRandomSeq) = trees.map(convert).unzip
    val usesRandom = usesRandomSeq.contains(true)
    (convTrees, usesRandom)
  }
  
  def convertMathFunction(functionType: Type, funName: Name, args: List[Tree]) = {
    var outers = Seq[String]()//"#include <math.h>")
    val hasDoubleParam = args.exists(_.tpe == DoubleClass.tpe)
    if (hasDoubleParam)
      outers ++= Seq(cl_khr_fp64)

    val normalizedArgs = args.map(_ match {
      case Select(a, toDoubleName()) => a
      case arg => arg
    })
    val (convArgs, usesRandom) = convertTrees(normalizedArgs)

    assert(convArgs.forall(_.statements.isEmpty), convArgs)
    FlatCode[String](
      convArgs.flatMap(_.outerDefinitions) ++ outers,
      convArgs.flatMap(_.statements),
      Seq(
        funName + "(" +
        convArgs.zip(normalizedArgs).map({ case (convArg, normalizedArg) =>
          assert(convArg.statements.isEmpty, convArg)
          val Seq(value) = convArg.values
          //"(" + convertTpe(normalizedArg.tpe) + ")" + value
          functionType match {
            case _ //MethodType(List(param), resultType) 
            if normalizedArg.tpe != DoubleClass.tpe =>
              "(float)" + value
            case _ =>
              "(" + convertTpe(normalizedArg.tpe) + ")" + value
          }
        }).mkString(", ") +
        ")"
      )
    )
  }
  def constPref(mods: Modifiers) =
    (if (mods.hasFlag(MUTABLE)) "" else "const ") 
      
  def convertTpt(tpt: TypeTree) = convertTpe(tpt.tpe)
  def convertTpe(tpe: Type) = {
    if (tpe == null) {
      throw new RuntimeException("Null type cannot be converted to OpenCL !")
      "?"
    } else if (tpe == NoType) 
      "void" 
    else 
      tpe.toString match {
        case "Int" => "int"
        case "Long" => "long"
        case "Short" => "short"
        case "Char" => "short"
        case "Byte" => "char"
        case "Float" => "float"
        case "Double" => "double"
        case "Boolean" => "char"
        case "org.bridj.SizeT" => "size_t"
        case _ => throw new RuntimeException("Cannot convert unknown type " + tpe + " to OpenCL")
      }
  }
}
