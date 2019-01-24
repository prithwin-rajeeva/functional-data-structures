package org.scala.events;
import java.util.*;

public class Test {

public class TreeNode {
 int val;
     TreeNode left;
     TreeNode right;
     TreeNode(int x) { val = x; }
 }

    public List<List<Integer>> levelOrder(TreeNode root) {
        Queue<TreeNode> q = new LinkedList<>();
        List<List<Integer>> response = new ArrayList<>();
        q.offer(root);
        while(!q.isEmpty()) {
            int ql = q.size();
            List<Integer> level = new ArrayList<>();
            for(int i = 0 ; i < ql ; i++) {
                TreeNode item = q.poll();
                level.add(item.val);
                q.offer(item.left);
                q.offer(item.right);
            }
            response.add(level);
        }
        return response;
    }
}
