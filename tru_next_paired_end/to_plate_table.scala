import scala.io._
import java.io._

def revComp(c: Char): Char = c match {
  case('A') => 'T'
  case('C') => 'G'
  case('G') => 'C'
  case('T') => 'A'
  case(_) => 'N'
}
def revBases(str: String): String = {str.map{t => revComp(t)}}.mkString("").reverse

val next = Source.fromFile("Next_i5_10mer_output.txt").getLines().drop(1).map{line => line.split("\t")(4)}.toArray
val tru = Source.fromFile("Tru_i7_10mer_output.txt").getLines().drop(1).map{line => line.split("\t")(4)}.toArray

val output = new PrintWriter("Plate_barcodes.txt")
output.write("well\tTruSeq_i5\tNext_i7\n")

val letters = Array[String]("A","B","C","D","E","F","G","H")

for (i <- 1 until 13) {
  letters.zipWithIndex.foreach{case(letter,index) => {
    output.write(letter + i + "\t" + tru(index) + "\t" + revBases(next(i-1)) + "\t" + next(i-1) + "\n")
  }}
}
output.close()
