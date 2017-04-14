package org.jaansdornea

case class Node (val value: Int, var left: Option[Node] = None, var right: Option[Node] = None)