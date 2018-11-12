import java.io._
import scala.io._
import scala.collection._

val fiveStart = "CAAGCAGAAGACGGCATACGAGAT"
val gcPad = "gaccgtcggc"
val pulR = "ACTTTATCAATCTCGCTCCAAACC"

def rev(base: Char): Char = base match {
  case('A') => 'T'
  case('C') => 'G'
  case('G') => 'C'
  case('T') => 'A'
  case(_) => 'N'
}
def revComp(str: String) = str.toUpperCase.map{base => rev(base)}.mkString("").reverse
def gcCont(str: String) = str.toUpperCase.map{char => if (char == 'G' || char == 'C') 1 else 0}.sum.toDouble / str.length.toDouble

val letters = Array[String]("A","B","C","D","E","F","G","H")
val numbers = (1 until 13)

val seenCodes = new mutable.HashMap[String,Boolean]()

val output = new PrintWriter("Amp_barcode_output.txt")
output.write("WellPosition\tName\tSequence\tNotes\n")
val inputs = Source.fromFile("./Amp_bar_set.tsv").getLines().drop(1)
var index = 1
letters.foreach{letter => {
  numbers.foreach{number => {
    val sp = inputs.next().split("\t")
    val primer = fiveStart + sp(3) + gcPad + pulR
    if (seenCodes contains sp(3))
      println("SEEN THAT CODE!!! " + sp(3))
    seenCodes(sp(3)) = true
    output.write(letter + number + "\tPul10merAmpPri" + index + "\t" + primer + "\tgcCont=" + gcCont(primer) + ";barcode=" + sp(3) + "\n")
    index += 1
  }}
}}
output.close()


// CAAGCAGAAGACGGCATACGAGAT tctaacgtct         ACTTTATCAATCTCGCTCCAAACC
// CAAGCAGAAGACGGCATACGAGAT ACTGCTGGAG ACCGTCGGC ACTTTATCAATCTCGCTCCAAACC
