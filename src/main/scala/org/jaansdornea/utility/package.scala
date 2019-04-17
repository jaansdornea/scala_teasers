package org.jaansdornea

import org.jaansdornea.tree.Tree
import org.jaansdornea.tree._

package object utility {
 
  def prettyPrint(tree: Tree): Unit = {
    tree.foreach {
      case (key, tuple) =>
        println(s"key: $key")
        println("-----------")
        println("starts")
        tuple._1.foreach {
          case (point, lines) =>
            println(s"\t\t $point => ")
            lines.foreach { line =>
              println(s"\t\t\t $line")
            }
        }
        println("ends")
        tuple._2.foreach {
          case (point, lines) =>
            println(s"\t\t $point => ")
            lines.foreach { line =>
              println(s"\t\t\t $line")
            }
        }
        println("----------")
    }
  }
  
  /*
   * function for taking a tree and picking a level to add a new branch to
   * returns a tuple (max branch levels, and random branch level)
   */
  private val findLevelForNewBranch: (Tree) => (Int, Int) = (tree) => {
    val upper = tree.keySet.max(Ordering[Int])
    if (upper == 1) (upper, 1)
    else (Math.random() * 1000).toInt.%(upper) match {
      case 0 => (upper, 1)
      case x => (upper, x)
    }
  }
  
   /*
   * function that finds an optional random index
   *  at a given level on a tree
   * Note: in case of no branch at a given level receive None
   * 1  \/ 1
   * 3  |
   * the above tree would yield None in the event of random index at branch 2
   */
  private val randomIndexAtLevel: (Int, Tree) => Option[Int] =
    (level, tree) => {
      tree.get(level) match {
        case Some(_) =>
          if (tree(level)._1.size > 0)
            Some((Math.random() * 1000).toInt.%(tree(level)._1.size))
          else None
        case None => None
      }
    }
    
    /**
   * yields a new tree with an added branch  
   */
  def addBranch(tree: Tree): Tree = {

    /**
     * utility function that updates Optional node on the tree
     * pulls node out of branch level L and adds node on tree to Level L + 1
     */
    def applyUpdate(branch: Option[(Int, Line)], updatedTree: Tree): Tree = {
      branch match {
        case Some((level, line)) => {
          //add to map in alternate branch level
          val branchUpdated: Tree = updatedTree.get(level + 1) match {
            case None => {
              updatedTree.updated(level + 1, (
                  Map[Point, Set[Line]]((line.start, Set(line))), 
                  Map[Point,Set[Line]]((line.end, Set(line))))
               )
            }
            case Some(startEndMaps) => {
              updatedTree.updated(level + 1, {
                
                // add lines to sets or new set at line.start
                val startSet = updatedTree(level + 1)._1.getOrElse(line.start, Set[Line]())
                val endSet = updatedTree(level + 1)._2.getOrElse(line.end, Set[Line]())
                
                (updatedTree(level + 1)._1.updated(line.start, startSet.+(line)),
                    updatedTree(level + 1)._2.updated(line.end, endSet.+(line)))
              })
            }
          }
          
          // pop original values
          val (first, second) = branchUpdated.get(level) match {
            case None => (Map((line.start, Set(line))), Map((line.end, Set(line))))
            case Some((fm, sm)) => {
              val firstMap = fm.get(line.start) match {
                case None => Map((line.start, Set(line)))
                case Some(s) => fm.updated(line.start, s - line).filter { case(key, value) => !value.isEmpty }
              }
              val secondMap = sm.get(line.end) match {
                case None => Map((line.end, Set(line)))
                case Some(s) => sm.updated(line.end, s - line).filter { case(key, value) => !value.isEmpty }
              }
              (firstMap, secondMap)
            }
          }
          // update original values with updated original removal
          val updated = branchUpdated.updated(level, (first, second))
          updated
        }
        case None => updatedTree
      }
    }

    def updateBranchAtLevelOnBranch(branch: Option[(Int, Line)], updatedTree: Tree): Tree =
      {
        val maxLevel = updatedTree.keySet.max

        println(s"entering updateBranchAtLevelOnBranch with $branch")

        branch match {
          case Some((level, line)) => {

            // update original values with updated original removal
            val newTree = applyUpdate(branch, updatedTree)

            // are we done
            // if there exists a branch connection
            // defined by branch.start == otherBranch.end
            // we are not done
            val (startMap, endMap) = newTree(level + 1)
            
            //for all end maps containing line starts are not empty
            ((1) to maxLevel).filter { l => 
              {
            
                val endMap = newTree(l)._2
                if (!endMap.isEmpty 
                    && endMap.contains(line.start) 
                    && endMap.get(line.start).isDefined) !endMap(line.start).isEmpty
                else false
              }
            }.toList match {
              case head :: rest => {
                // this is the problem, line.end not always in map
                val l = newTree(head)._2(line.start).head
                println(l)
                updateBranchAtLevelOnBranch(Some((head, l)), newTree)
              }
              case Nil => 
                newTree
            }
          }
          case None => updatedTree
              
        }
       
      }

    
    val (maxLevel, branchLevel) = findLevelForNewBranch(tree)
    val branchToAddTo = randomIndexAtLevel(branchLevel, tree) match {
        case None => {
          val level = 1
          val pointsAtLevel: List[(Point, Set[Line])] = tree(level)._1.toList
          val pointsIndex = (Math.random() * 1000).toInt % pointsAtLevel.size
          
          val branches = pointsAtLevel(pointsIndex)._2.toList
          val branchesIndex = (Math.random() * 1000).toInt % branches.size
          
          branches(branchesIndex)
          
        }
        case Some(i) => {
          val branches = tree(branchLevel)._1.toList(i)._2.toList
          println(branches.size)
          val index = (Math.random() * 1000).toInt % branches.size
          println(index)
          branches(index)
        }
    }
    //add branch to map in alternate branch level
    val newBranch = draw(branchToAddTo.end, branchLength(), branchSlope())

    val withNewBranch: Tree = tree.updated(1, {
      val startSet = tree(1)._1.get(newBranch.start) match {
        case None => Set(newBranch)
        case Some(s) => s + newBranch
      }
      val endSet = tree(1)._2.get(newBranch.end) match {
        case None => Set(newBranch)
        case Some(s) => s + newBranch
      }
      (tree(1)._1.updated(newBranch.start, startSet), tree(1)._2.+((newBranch.end, endSet)))
    })
    updateBranchAtLevelOnBranch(Some((branchLevel, branchToAddTo)), withNewBranch)
  }

  
}