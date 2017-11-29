import fastparse.all._


case class NamedFunction[T, V](f: T => V, name: String) extends (T => V){
  def apply(t: T) = f(t)
  override def toString() = name

}

// Here is the parser
//val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

val space         = P( CharsWhileIn(" \r\n").? )
val digits        = P( CharsWhileIn("0123456789"))
val exponent      = P( CharIn("eE") ~ CharIn("+-").? ~ digits )
val fractional    = P( "." ~ digits )
val integral      = P( "0" | CharIn('1' to '9') ~ digits.? )

val number = P( CharIn("+-").? ~ integral ~ fractional.? ~ exponent.?).!.map(
  x => x.toDouble
)

number.parse("124535432")
number.parse("-124535432")
number.parse("+124535432")
number.parse("1.223232233E8")

