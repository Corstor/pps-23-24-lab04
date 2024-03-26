package tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    case class ComplexNumber(re: Double, im: Double)

    type Complex = ComplexNumber
    def complex(re: Double, im: Double): Complex = ComplexNumber(re, im)
    extension (complex: Complex)
      def re(): Double = complex match
        case ComplexNumber(re, _) => re
    
      def im(): Double = complex match
        case ComplexNumber(_, im) => im
      def sum(other: Complex): Complex = ComplexNumber(complex.re + other.re, complex.im + other.im)
      
      def subtract(other: Complex): Complex = ComplexNumber(complex.re - other.re, complex.im - other.im)
      def asString(): String = complex match
        case ComplexNumber(re, 0) => re.toString()
        case ComplexNumber(0, im) => im + "i"
        case ComplexNumber(re, im) if im > 0 => re + " + " + im + "i"
        case _ => complex.re + " - " + complex.im.abs + "i"
      
