package com.chotom.sql.parser

import fastparse.all._

object Parsers {

  case class NamedFunction[T, V](f: T => V, name: String) extends (T => V){
    def apply(t: T) = f(t)
    override val toString: String = name

  }


  val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")
  val space         = P( CharsWhileIn(" \r\n").? )
  val digits        = P( CharsWhileIn("0123456789"))
  val exponent      = P( CharIn("eE") ~ CharIn("+-").? ~ digits )
  val fractional    = P( "." ~ digits )
  val integral      = P( "0" | CharIn('1' to '9') ~ digits.? )

  val number = P( CharIn("+-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(
    x => BigDecimal(x)
  )

  val `false`       = P( "false" ).map(_ => false)
  val `true`        = P( "true" ).map(_ => true)

  val hexDigit      = P( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
  val unicodeEscape = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
  val escape        = P( "\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape) )

  val strChars = P( CharsWhile(StringChars) )
  val string = P( space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Js.Str)
}

