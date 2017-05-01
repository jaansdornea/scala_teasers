package org.jaansdornea.utility
case class Row(val values: Int*)
object Matrix {
  def apply(rows: Row*): Matrix = new Matrix(rows:_*)
}

class Matrix(val rows: Row*) {
  if (!rows.isEmpty) {
    require {
      rows(0).values.length > 0
      rows.foldLeft(true)( (equal, row) => equal && row.values.length == rows(0).values.length)
    }
  }
  
  def ^ : Matrix = {
    val columns: Seq[Seq[Int]] = 
      (0 to (rows(0).values.length)).map((index) => rows.map( (row) => row.values(index)))
    Matrix(columns.map ( (column) => Row(column:_*)):_*)
  }
  
  def * (other: Matrix): Matrix = {
    require {
      !rows.isEmpty && !other.rows.isEmpty && rows(0).values.length == other.rows.length
    }
    
    ???
  }

  def * (scalar: Int): Matrix = {
    ???
  }
  
  def + (other: Matrix): Matrix = {
    ???
  }
  
  override def toString(): String = {
    ???
  }
}