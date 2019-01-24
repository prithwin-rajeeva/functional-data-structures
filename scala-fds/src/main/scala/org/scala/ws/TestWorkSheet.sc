  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null
  }
  /**
    * Definition for a binary tree node.
    * class TreeNode(var _value: Int) {
    *   var value: Int = _value
    *   var left: TreeNode = null
    *   var right: TreeNode = null
    * }
    */
  object Solution {
    def isValidBST(root: TreeNode): Boolean = {
      def isValid(n: TreeNode, min: Int, max: Int, seen: Set[Int]): Boolean = {
        if (n == null) true
        else
          !seen.contains(n.value) &&
            n.value >= min && n.value <= max &&
            isValid(n.left, min, n.value, seen + n.value) &&
            isValid(n.right, n.value, max, seen + n.value)
      }

      isValid(root, Int.MinValue, Int.MaxValue, Set.empty[Int])
    }
  }