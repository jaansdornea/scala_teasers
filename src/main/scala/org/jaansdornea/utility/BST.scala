package org.jaansdornea.utility

case class Node[T <% Ordered[T]](val value: T, var left: Option[Node[T]] = None, var right: Option[Node[T]] = None)

trait SearchAbleStore[T] {
  def insert(t: T) 
  def remove(t: T) 
  def contains(t: T): Boolean
}
trait SearchAbleTree[T] extends SearchAbleStore[T] {
  def size(): Int 
  def height(): Int
  def preOrder(): List[T] 
  def postOrder (): List[T] 
  def inOrder(): List[T] 
  def balance(): SearchAbleTree[T]
}

object BST {
  
  // build with set of values
  def apply[T <% Ordered[T]](sortedAscVals: T*): Option[BST[T]] = {
    // find the middle object
    def node(values: Seq[T]): Option[Node[T]] = {
      if (values.isEmpty) None
      else if (values.length == 1) Some(Node(values(0), None, None))
      else {
        val midpoint = (values.size/2).toInt
        Some(Node(values(midpoint), 
            node(values.slice(0, midpoint)), 
            node(values.slice(midpoint + 1, values.length)))) 
      }
    }
    node(sortedAscVals) match {
      case None => None
      case Some(r) => Some(new BST(r))
    }
  }
  
  // build with a supplied root node
  def apply[T <% Ordered[T]](root: Node[T]): Option[BST[T]] = Some(new BST(root))
}

class BST[T <% Ordered[T]](val root: Node[T]) extends SearchAbleTree[T] {
  
  def insert(t: T) {
    // if the node is already present short circuit
    if (this.contains(t)) return
   
    def insertAt(at: Node[T]) {
      at match {
        case i if (at.value < t) => {
          at.right match {
            case None => at.right = Some(Node(t))
            case Some(r) => insertAt(r)
          }
        }
        case i if (at.value > t) => {
          at.left match {
            case None => at.left = Some(Node(t))
            case Some(l) => insertAt(l)
          }
        }
        case default => throw new IllegalArgumentException("value already exists in search tree")
      }
    }
    insertAt(root)
  }
  
  def contains(t: T): Boolean = {
    def searchAt(at: Option[Node[T]]): Boolean = {
      at match {
        case None => false
        case Some(n) if (n.value == t) => true
        case Some(n) if (n.value < t) => searchAt(n.right)
        case default => searchAt(at.get.left)
      }
    }
    searchAt(Some(root))
  }
  
  def remove(t: T) {
    if (!contains(t)) return
    // we have t so 
    // 1.) find t and parent
    val (parent, nt) : (Option[Node[T]], Option[Node[T]]) = (None, None)
    // if parent is none, this t is root pop t and make right child root, add left child to leftmost child of right
    // then rebalance
    throw new UnsupportedOperationException("not yet implemented")
  }
  
  def size(): Int = {
    def count(at: Option[Node[T]]): Int = 
      at match { 
        case None => 0
        case Some(n) => 1 + count(n.left) + count(n.right) 
      }
    count(Some(root))
  }
  
  def height(): Int = {
    def maxLen(at: Option[Node[T]]): Int = 
      at match {
      case None => 0
      case Some(n) => 1 + math.max(maxLen(n.left), maxLen(n.right))
    }
    maxLen(Some(root))
  }
  
  def preOrder(): List[T] = {
    // preorder => V, L, R
    def traverse(next: Option[Node[T]]): List[T] = {
      next match {
        case None => List[T]()
        case Some(x) => x.value :: traverse(x.left) ::: traverse(x.right)
      }
    }
    traverse(Some(this.root))
  }
  
  def postOrder(): List[T] = {
    def traverse(next: Option[Node[T]]): List[T] = {
      next match {
        case None => List[T]()
        case Some(x) => traverse(x.left) ::: traverse(x.right) ::: List[T](x.value)
      }
    }
    traverse(Some(this.root))
  }
  
  def inOrder(): List[T] = {
    def traverse(next: Option[Node[T]]): List[T] = {
      next match {
        case None => List[T]()
        case Some(x) => (traverse(x.left) :+ x.value) ::: traverse(x.right)
      }
    }
    traverse(Some(this.root))
  }
  
  def balance(): BST[T] = {???}
  
}