package org.eltex.ems.netconf

import scala.util.parsing.combinator.JavaTokenParsers
import org.parboiled2._
/**
  * Created by xoton on 03.02.2018.
  */


trait GenericYangParser extends JavaTokenParsers {
  def unordered[T, U](tp: Parser[T], tu: Parser[U]): Parser[$[T, U]] =
    tp ~ tu ^^ { case (x ~ y) => $(x, y) } | tu ~ tp ^^ { case (x ~ y) => $(y, x) }

  case class $[+a, +b](_1: a, _2: b)

  implicit class ExtParser[+T](val parser: Parser[T]) {
    def $[U](tu: Parser[U]): Parser[$[T, U]] = unordered(parser, tu)
  }

}

object YangParser extends GenericYangParser {
  // ## RFC 5234 core rules.
  def alpha: Parser[ALPHA] = "[A-Za-z]".r ^^ ALPHA
  def cr: Parser[CR] = '\r' ^^^ CR()
  def crlf: Parser[CRLF] = "\r\n" ^^^ CRLF()
  def digit: Parser[DIGIT] = "[0-9]".r ^^ DIGIT
  def dquote: Parser[DQUOTE] = '\"' ^^^ DQUOTE()
  def hexdig: Parser[HEXDIG] = (
    digit ^^ (x => HEXDIG(x.digit))
      | "a-f".r ^^ HEXDIG
    )
  def htab: Parser[HTAB] = '\t' ^^^ HTAB()
  def lf: Parser[LF] = '\n' ^^^ LF()
  def sp: Parser[SP] = ' ' ^^^ SP()
  def vchar: Parser[VCHAR] = "[^\\x00\\x08\\x0B\\x0C\\x0E-\\x1F]" ^^ VCHAR
  def wsp: Parser[WSP] = (sp | htab) ^^^ WSP()
  def squote: Parser[SQUOTE] = '\'' ^^^ SQUOTE()
  // ## Basic Rules
  def identifier: Parser[Identifier] =
    (
      (
        alpha.filter({ x => !(Set("X","x","M","m","L","l") contains x.char)}) ^^ {_.char}
      | "_"
      | failure("An identifier MUST NOT start with digits and (('X'|'x') ('M'|'m') ('L'|'l'))")
      )
      ~ rep
      (
            alpha ^^ {_.char}
          | digit ^^ {_.digit}
          | "_"
          | "-"
          | "."
      )
    ) ^^ { x => Identifier((x._1 :: x._2).mkString) }

  def identifierArg = identifier ^^ IdentifierArg
  // a string that matches the rule identifier-arg ?
  // The "string that matches" is a dequoted string without any "+" or quotes,
  // so the grammar rules don't have to handle those explicitly.
  // section 6.1.2/6.1.3 makes this clear.
  // todo #quoting
  def identifierArgStr = identifier ^^ IdentifierArgStr
  def prefix = identifier ^^ Prefix
  def identifierRefArg : Parser[IdentifierRefArg] = opt(prefix <~ ":") ~ identifier ^^ { case optPrefix ~ id => IdentifierRefArg(optPrefix, id)}
  def identifierRefArgStr = identifierRefArg ^^ IdentifierRefArgStr // todo #quoting
  def prefixArg = prefix ^^ PrefixArg
  def prefixArgStr = prefix ^^ PrefixArgStr // todo #quoting
  // "unquoted string as returned by the scanner"
  // (no quotes, no concats, no pretty printing whitespace)
  def string = stringLiteral ^^ YangString
  def nonZeroDigit = "[1-9]".r ^^ NonZeroDigit
  def positiveIntegerValue = nonZeroDigit ~ rep(digit) ^^ {x => PositiveIntegerValue((x._1.char :: x._2).mkString)}
  def nonNegativeIntegerValue = (
    "0" ^^ NonNegativeIntegerValue
      | positiveIntegerValue ^^ { x => NonNegativeIntegerValue(x.value) }
    )
  def integerValue = (
    ("-" ~ nonNegativeIntegerValue) ^^ (x => IntegerValue(x._1 + x._2.value) )
    | nonNegativeIntegerValue ^^ {x => IntegerValue(x.value)}
    )

  def zeroIntegerValue = rep1(digit) ^^ {x => ZeroIntegerValue(x.mkString)}
  def demicalValue = integerValue ~ "." ~ zeroIntegerValue ^^
    { case x ~ dot ~ y => DecimalValue(x.value + dot + y.value)}

  def lineBreak = crlf | lf ^^^ LineBreak
  def sep = rep1(wsp | lineBreak) ^^^ Sep
  def optsep = rep(wsp | lineBreak) ^^^ OptSep
  def stmtSep = rep(wsp | lineBreak | unknownStatement)
  def stmtEnd = ";" | ( "{" ~> rep(unknownStatement) <~ "}" ) ^^^ StmtEnd
  def unknownStatement2 : Parser[UnknownStatement2] =
    (opt(prefix <~ ":") ~ identifier ~ opt(sep ~> string) ~ optsep
      ~ (";" |( "{" ~> rep(unknownStatement2) <~ "}")) ^^^ UnknownStatement2())
  def unknownStatement : Parser[UnknownStatement] =
    ((prefix <~ ":") ~ identifier ~ opt(sep ~> string) ~ optsep
      ~ (";" |( "{" ~> rep(unknownStatement2) <~ "}")) ^^^ UnknownStatement())

  def yangVersionArg = "1" ^^ YangVersionArg
  def yangVersionArgStr = yangVersionArg ^^ YangVersionArgStr // todo #quoting
  def yangVersionKeyword = "yang-version" ^^^ YangVersionKeyword
  def yangVersionStmt = yangVersionKeyword ~> sep ~> yangVersionArgStr <~ optsep <~ stmtEnd ^^ YangVersionStmt


  def prefixKeyword = "prefix" ^^^ PrefixKeyword
  def prefixStmt = prefixKeyword ~> sep ~> prefixArgStr <~ optsep <~ stmtEnd ^^ PrefixStmt

  def moduleHeaderStmts = ( opt(yangVersionStmt <~ stmtSep) ~ prefixStmt <~ stmtSep
    ^^ { case version ~ prefix => ModuleHeaderStmts(version, prefix) })
  def moduleKeyword = "module" ^^^ ModuleKeyword
  def moduleStmt = ( (optsep ~> moduleKeyword ~> sep ~> identifierArgStr <~ optsep)
    ~ ("{" ~> stmtSep ~> moduleHeaderStmts <~ "}" <~ optsep )
    ^^ {case name~ header => ModuleStmt(name, header)}
    )

  // todo Нужно обязательно добавить в xxxArgStr кавычки, иначе не парсится
}

class YangParserV2(val input: ParserInput) extends Parser {
  def alpha  = rule { anyOf("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz") ~> { x => ALPHA(x) } }
  def cr = rule { ch('\r') ~> CR }
  def test : Rule2[ALPHA, CR] = rule { alpha ~ cr }
  //def cr: Parser[CR] = '\r' ^^^ CR()
  //def crlf: Parser[CRLF] = "\r\n" ^^^ CRLF()
  //def digit: Parser[DIGIT] = "[0-9]".r ^^ DIGIT
  //def dquote: Parser[DQUOTE] = '\"' ^^^ DQUOTE()
  //def hexdig: Parser[HEXDIG] = (
  //  digit ^^ (x => HEXDIG(x.digit))
  //    | "a-f".r ^^ HEXDIG
  //  )
  //def htab: Parser[HTAB] = '\t' ^^^ HTAB()
  //def lf: Parser[LF] = '\n' ^^^ LF()
  //def sp: Parser[SP] = ' ' ^^^ SP()
  //def vchar: Parser[VCHAR] = "[^\\x00\\x08\\x0B\\x0C\\x0E-\\x1F]" ^^ VCHAR
  //def wsp: Parser[WSP] = (sp | htab) ^^^ WSP()
  //def squote: Parser[SQUOTE] = '\'' ^^^ SQUOTE()
}

sealed trait YangToken

// RFC 5234 core rules

case class ALPHA(char: String) extends YangToken

/** Carriage return */
case class CR() extends YangToken

/** Internet standard new line */
case class CRLF() extends YangToken

case class DIGIT(digit: String) extends YangToken

case class DQUOTE() extends YangToken

/** Only lower-case a..f */
case class HEXDIG(digit: String) extends YangToken

case class HTAB() extends YangToken

/** linefeed */
case class LF() extends YangToken

/** space */
case class SP() extends YangToken

/** visible (printing) characters */
case class VCHAR(char: String) extends YangToken

/** whitespace */
case class WSP() extends YangToken

// Basic Rules

case class SQUOTE() extends YangToken
case class Prefix(identifier: Identifier) extends YangToken
case class PrefixArg(name : Prefix) extends YangToken
case class PrefixArgStr(name : Prefix) extends YangToken
/** An identifier MUST NOT start with (('X'|'x') ('M'|'m') ('L'|'l')) */
case class Identifier(name : String) extends YangToken
case class IdentifierArg(identifier : Identifier) extends YangToken
case class IdentifierArgStr(identifier : Identifier) extends YangToken
case class IdentifierRefArg(prefix: Option[Prefix], identifier : Identifier) extends YangToken
case class IdentifierRefArgStr(value: IdentifierRefArg) extends YangToken
/** an unquoted string */
case class YangString(value : String) extends YangToken
case class NonZeroDigit(char: String) extends YangToken
case class LineBreak() extends YangToken
case class PositiveIntegerValue(value: String) extends YangToken
case class NonNegativeIntegerValue(value: String) extends YangToken
case class IntegerValue(value : String) extends YangToken
case class ZeroIntegerValue(value : String) extends YangToken
/** unconditional separator */
case class Sep() extends YangToken
case class OptSep() extends YangToken
case class StmtSep() extends YangToken
case class StmtEnd() extends YangToken
case class DecimalValue(value : String) extends YangToken // Float ?

case class UnknownStatement2() extends YangToken
case class UnknownStatement() extends YangToken

// Keywords

case class ModuleKeyword() extends YangToken
case class YangVersionKeyword() extends YangToken
case class PrefixKeyword() extends YangToken

// Common statements

case class ModuleStmt(name : IdentifierArgStr, header : ModuleHeaderStmts) extends YangToken
case class ModuleHeaderStmts(versionStmt: Option[YangVersionStmt], prefixStmt: PrefixStmt) extends YangToken
case class YangVersionArg(value : String) extends YangToken
case class YangVersionArgStr(arg: YangVersionArg) extends YangToken
case class YangVersionStmt(argStr: YangVersionArgStr) extends YangToken
case class PrefixStmt(arg : PrefixArgStr) extends YangToken