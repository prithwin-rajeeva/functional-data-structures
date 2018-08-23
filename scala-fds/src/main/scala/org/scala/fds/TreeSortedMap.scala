package org.scala.fds

case class Entry[K,V](key:K,value:V)

trait SortedMap[K,V] {
  def +(key: K, value: V)(implicit ordering: Ordering[K]): SortedMap[K, V]
  def -(key: K)(implicit ordering: Ordering[K]): SortedMap[K, V]
  def apply(key: K)(implicit ordering: Ordering[K]): Option[V]
}

abstract class TreeMapNode[K,V]{
  def isEmpty:Boolean
  def node:Entry[K,V]
  def left:TreeMapNode[K,V]
  def right:TreeMapNode[K,V]
}

case class NonEmptyTreeMapNode[K,V](node:Entry[K,V],left: TreeMapNode[K,V],right: TreeMapNode[K,V]) extends TreeMapNode[K,V]{
  override def isEmpty: Boolean = false
}

case class <>[K,V]() extends TreeMapNode[K,V] {
  override def isEmpty: Boolean = true
  override def node:Entry[K,V] = throw new IllegalAccessException("looking up a key of an empty node")
  override def left: TreeMapNode[K, V] = throw new IllegalAccessException("left of an empty tree")
  override def right: TreeMapNode[K, V] = throw new IllegalArgumentException("right of an empty tree")
}

class TreeSortedMap[K,V](root:TreeMapNode[K,V]) extends SortedMap[K,V] {

  override def +(key: K, value: V)(implicit ordering: Ordering[K]): SortedMap[K, V] = {
    def plus(rt:TreeMapNode[K,V],k:K,v:V):TreeMapNode[K,V] = {
      if(!rt.isEmpty) {
        if(ordering.lt(k,rt.node.key)) {
          NonEmptyTreeMapNode(rt.node, plus(rt.left, k, v), rt.right)
        } else if (ordering.gt(key,rt.node.key)) {
          NonEmptyTreeMapNode(rt.node, rt.left, plus(rt.right, k, v))
        } else {
          rt
        }
      } else {
        NonEmptyTreeMapNode(Entry(key,value),new <>, new <>)
      }
    }
    TreeSortedMap(plus(root,key,value))
  }

  override def -(key: K)(implicit ordering: Ordering[K]): SortedMap[K, V] = {
    def min(rt:TreeMapNode[K,V]):Entry[K,V] = {
      if(rt.left.isEmpty && rt.right.isEmpty) rt.node
      else {
        min(rt.left)
      }
    }

    def truncate(rt: TreeMapNode[K, V]): TreeMapNode[K,V] = {
      if(rt.left.isEmpty && rt.right.isEmpty) new <>
      else  {
        NonEmptyTreeMapNode(rt.node,truncate(rt.left),rt.right)
      }
    }

    def minus(rt: TreeMapNode[K,V], k: K): TreeMapNode[K,V] = {
      if(!rt.isEmpty) {
        if(ordering.lt(k,rt.node.key)) {
          NonEmptyTreeMapNode(rt.node, minus(rt.left,k),rt.right)
        } else if(ordering.gt(k,rt.node.key)) {
          NonEmptyTreeMapNode(rt.node, rt.left, minus(rt.right,k))
        } else {
          if(rt.left.isEmpty) rt.right
          else if(rt.right.isEmpty) rt.right
          else {
            NonEmptyTreeMapNode(min(rt.right),rt.left,truncate(rt.right))
          }
        }
      } else {
        throw new NoSuchElementException
      }
    }
    TreeSortedMap(minus(root,key))
  }
  override def apply(key: K)(implicit ordering: Ordering[K]): Option[V] = {
    def find(rt:TreeMapNode[K,V],k:K):Option[V] = {
      if(rt.isEmpty) None
      else {
        if(ordering.lt(key,rt.node.key)) {
          find(rt.left,k)
        } else if(ordering.gt(key,rt.node.key)) {
          find(rt.right,k)
        } else {
          Some(rt.node.value)
        }
      }
    }
    find(root,key)
  }
}

object TreeSortedMap {
  def empty[K,V]:TreeSortedMap[K,V] = new TreeSortedMap[K,V](new <>[K,V])
  def apply[K,V](in: TreeMapNode[K, V]): TreeSortedMap[K,V] = new TreeSortedMap(in)
}
