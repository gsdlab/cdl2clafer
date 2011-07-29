/*
 */
package gsd.cdl2clafer.model


sealed abstract class CExpression {
  def & (other : CExpression) : CExpression = other match {
    case CTrue() => this
    case _ => CAnd(this, other)
  }

  def gte (other : CExpression) : CExpression = CGreaterThanOrEq(this, other)
  def <= (other : CExpression) : CExpression = CLessThanOrEq(this, other)
  def === (other: CExpression):CExpression = CEq(this, other)

  def | (other : CExpression) : CExpression = other match {
    case CFalse() => this
    case _ => COr(this, other)
  }
  def unary_! = CNot(this)
  def implies (other : CExpression) : CExpression = CImplies(this, other)

/*
  def splitConjunctions : List[CExpression] = this match {
    case CAnd(x,True()) => x.splitConjunctions
    case CAnd(True(),y) => y.splitConjunctions
    case CAnd(x,y) => x.splitConjunctions ++ y.splitConjunctions
    case x => List(x)
  }
 
  def children():List[CExpression] =  
	(for(i <- 0 until this.asInstanceOf[Product].productArity;
		child = this.asInstanceOf[Product].productElement (i);
		if (child.isInstanceOf[CExpression])) 
		yield child.asInstanceOf[CExpression]).toList
*/ 

}

case class CNonBoolean( e : CExpression ) extends CExpression

case class CStringLiteral(value : String) extends CExpression {
//  override def toString = value
//  override def toString = "\"" + value + "\""
}
case class CLongIntLiteral(value : Long) extends CExpression {
  override def toString = "" + value
}

case class CDoubleLiteral(value : Double) extends CExpression {
  override def toString = "" + value
}

case class CIdentifier(id : String) extends CExpression {
  override def toString = id
}

case class CConditional(cond : CExpression,
                       pass : CExpression,
                       fail : CExpression) extends CExpression

sealed abstract class CUnaryExpression(e : CExpression,
                                      op : String) extends CExpression {
  override def toString = op + e
}

sealed abstract class CBinaryExpression(l : CExpression,
                                       r : CExpression,
                                       op : String) extends CExpression {
  override def toString = "(" + l + " "  + op + " " + r + ")"
}

case class COr(left : CExpression, right : CExpression)
        extends CBinaryExpression(left, right, "||")
case class CAnd(left : CExpression, right : CExpression)
        extends CBinaryExpression(left, right, "&&")

case class CEq(left : CExpression, right : CExpression)
        extends CBinaryExpression(left, right, "==")
case class CNEq(left : CExpression, right : CExpression)
        extends CBinaryExpression(left, right, "!=")

case class CLessThan(left : CExpression, right : CExpression)
        extends CBinaryExpression(left, right, "<")
case class CLessThanOrEq(left : CExpression, right : CExpression)
        extends CBinaryExpression(left, right, "<=")
case class CGreaterThan(left : CExpression, right : CExpression)
        extends CBinaryExpression(left, right, ">")
case class CGreaterThanOrEq(left : CExpression, right : CExpression)
        extends CBinaryExpression(left, right, ">=")

case class CPlus(left :CExpression, right : CExpression)
        extends CBinaryExpression(left, right, "+")
case class CMinus(left :CExpression, right : CExpression)
        extends CBinaryExpression(left, right, "-")
case class CDot(left : CExpression, right : CExpression)
        extends CBinaryExpression(left, right, ".")

case class CBtAnd(left : CExpression, right : CExpression)
        extends CBinaryExpression(left, right, "&")
case class CBtOr(left : CExpression, right : CExpression)
        extends CBinaryExpression(left, right, "|")
case class CBtXor(left : CExpression, right : CExpression)
        extends CBinaryExpression(left, right, "^")
case class CBtLeft(left : CExpression, right : CExpression)
        extends CBinaryExpression(left, right, "<<")
case class CBtRight(left : CExpression, right : CExpression)
        extends CBinaryExpression(left, right, ">>")

case class CTimes(left :CExpression, right : CExpression)
        extends CBinaryExpression(left, right, "*")
case class CDiv(left :CExpression, right : CExpression)
        extends CBinaryExpression(left, right, "/")
case class CMod(left :CExpression, right : CExpression)
        extends CBinaryExpression(left, right, "%")

case class CMinusMinus(expr : CExpression)
        extends CUnaryExpression(expr, "--")
case class CNot(expr : CExpression)
        extends CUnaryExpression(expr, "!")

case class CFunctionCall(name : String, arguments : List[CExpression]) extends CExpression {
  override def toString() = name + "(" + arguments.foldLeft("")( (a,b) => a + ( if( a!="" ) "," else "" ) + b ) + ")"
}


// added for boolean transformation
case class CImplies(left : CExpression, right:CExpression)
      extends CBinaryExpression(left, right, "->")

case class CTrue() extends CExpression{
  override def & (other: CExpression) = other
  override def implies (other: CExpression) = other
  override def toString = "TRUE"
}
case class CFalse() extends CExpression{
  override def | (other : CExpression) = other
  override def toString = "FALSE"
}



case class CNEqInterfaceCount(interface:CExpression, count:CLongIntLiteral) extends CExpression {
  override def toString = "#" + interface.toString + " != " + count.toString
}

case class CEqInterfaceCount(interface:CExpression, count:CLongIntLiteral) extends CExpression {
  override def toString = "#" + interface.toString + " == " + count.toString
}

case class CLessThanInterfaceCount(interface:CExpression, count:CLongIntLiteral) extends CExpression {
  override def toString = "#" + interface.toString + " < " + count.toString
}

case class CLessThanOrEqInterfaceCount(interface:CExpression, count:CLongIntLiteral) extends CExpression {
  override def toString = "#" + interface.toString + " <= " + count.toString
}

case class CGreaterThanInterfaceCount(interface:CExpression, count:CLongIntLiteral) extends CExpression {
  override def toString = "#" + interface.toString + " > " + count.toString
}

case class CGreaterThanOrEqInterfaceCount(interface:CExpression, count:CLongIntLiteral) extends CExpression {
  override def toString = "#" + interface.toString + " >= " + count.toString
}

