package org.scala.algo

object NoOfIslands {

  def dissolve(grid: Array[Array[Char]], i: Int, j: Int):Unit = {
    if((i >= 0) && (i < grid.length) && (j >= 0) && (j < grid(i).length) && (grid(i)(j) == '1')) {
        grid(i)(j) = '0'
        dissolve(grid,i+1,j)
        dissolve(grid,i-1,j)
        dissolve(grid,i,j+1)
        dissolve(grid,i,j-1)
      }
    }

  def numIslands(grid: Array[Array[Char]]): Int = {
    var i = 0
    var j = 0
    var count = 0
    while(i < grid.length) {
      while(j < grid(i).length) {
        if(grid(i)(j) == '1') {
          count+=1
          dissolve(grid,i,j)
        }
        j+=1
      }
      i+=1
    }
    count
  }
}
