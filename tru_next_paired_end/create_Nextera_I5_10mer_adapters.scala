import java.io._
import scala.io._
import scala.collection._

val fiveStart = "AATGATACGGCGACCACCGAGATCTACAC"
val fiveEnd = "TCGTCGGCAGCGTCAGATGTGTATAAGAGACAG"

def rev(base: Char): Char = base match {
  case('A') => 'T'
  case('C') => 'G'
  case('G') => 'C'
  case('T') => 'A'
  case(_) => 'N'
}
def comp(str: String) = str.toUpperCase.map{base => rev(base)}.mkString("")
def gcCont(str: String) = str.toUpperCase.map{char => if (char == 'G' || char == 'C') 1 else 0}.sum.toDouble / str.length.toDouble

val letters = Array[String]("A","B","C","D","E","F","G","H")
val numbers = (1 until 7)

val seenCodes = new mutable.HashMap[String,Boolean]()

val output = new PrintWriter("Next_i5_10mer_output.txt")
output.write("WellPosition\tName\tSequence\tNotes\n")
val inputs = Source.fromFile("./Amp_bar_set.tsv").getLines().drop(1)
var index = 1
letters.foreach{letter => {
  numbers.foreach{number => {
    val sp = inputs.next().split("\t")
    val primer = fiveStart + sp(3) + fiveEnd
    if (seenCodes contains sp(3))
      println("SEEN THAT CODE!!! " + sp(3))
    seenCodes(sp(3)) = true
    output.write(letter + number + "\tamNxtI5v3_10merAA" + index + "\t" + primer + "\tgcCont=" + gcCont(primer) + ";barcode=" + sp(3) + "\n")
    index += 1
  }}
}}
output.close()
