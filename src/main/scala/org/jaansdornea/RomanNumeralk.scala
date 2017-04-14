package org.jaansdornea

import scala.annotation.tailrec

object RomanNumeralk extends App {
  
  def value(x: Char): Int = 
    x match {
    case 'I' => 1
    case 'V' => 5
    case 'X' => 10
    case 'L' => 50
    case 'C' => 100
    case 'D' => 500
    case 'M' => 1000
    case default => throw new IllegalArgumentException(s"$x is not a valid roman numeral")
    }
  
  def peakOne(rom: String): Option[Char] =
    if(rom.isEmpty()) None
    else Some(rom.head)
    
  def peakTwo(rom: String): (Option[Char], Option[Char]) =
    if (rom.isEmpty()) (None, None)
    else(Some(rom.head), peakOne(rom.tail))
  
  def getPeakedSum(peaked: (Option[Char], Option[Char])): Int = 
    peaked match {
      case (None, None) => 0
      case (Some(x), None) => value(x)
      case (Some(x), Some(y)) => {
        val _x = value(x)
        val _y = value(y)
        if (_x >= _y) _x + _y
        else _y - _x
      }
    }
  
  def value(romanNumeral: String): Int = {
    @tailrec
    def accum(v: Int, rest: String) : Int = {
      if (rest.length() == 0) v
      else if(rest.length() == 1) v + value(rest.head)
      else {
        val additional = getPeakedSum(peakTwo(rest)) + v
        val subString = rest.tail.tail
        accum(additional, subString)
      }
    }
    accum(0, romanNumeral)
  }
  
  assert (value("IX") == 9)
  assert (value("XXX") == 30)
  assert (value("IXXI") == 20)
  assert (value("LXIII") == 63)
  assert (value("") == 0)
  
}