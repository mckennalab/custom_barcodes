import scala.io._
import java.io._


case class BarcodeObject(index: Int, name: String, barcode: String) {
  def comp(base: Char): Char = base match {
    case x if x == 'A' || x == 'a' => 'T'
    case x if x == 'C' || x == 'c' => 'G'
    case x if x == 'G' || x == 'g' => 'C'
    case x if x == 'T' || x == 't' => 'A'
  }

  def reverseComp(): String = barcode.map{case(c) => comp(c)}.reverse

}

val nextera = Source.fromFile("Next_i5_10mer_output.txt").getLines().drop(1).zipWithIndex.map{case(line,index) => {
  val sp = line.split("\t")
  BarcodeObject(index + 1, sp(1), sp(3).split(";")(1).split("=")(1).toUpperCase)
}}.toArray

val truseq = Source.fromFile("Tru_i7_10mer_output.txt").getLines().drop(1).zipWithIndex.map{case(line,index) => {
  val sp = line.split("\t")
  BarcodeObject(index + 1, sp(1), sp(4).toUpperCase)
}}.toArray

val output = new PrintWriter("modern_plate.txt")
output.write("well\tnextera\ttruseq\tnexteraBarcode\tnexterabarcodeRC\ttruseqBarcode\ttruseqBarcodeRC\n")
Array[String]("A","B","C","D","E","F","G","H").zipWithIndex.foreach{case(letter,index) => {
  (1 until 13).zipWithIndex.foreach{case(number,index2) => {
    output.write(letter + number + "\t" +  nextera(index2).name + "\t" + truseq(index).name + "\t" + nextera(index2).barcode + "\t" + nextera(index2).reverseComp + "\t" + "\t" + truseq(index).barcode + "\t" + truseq(index).reverseComp + "\n")
  }}
}}
output.close()
