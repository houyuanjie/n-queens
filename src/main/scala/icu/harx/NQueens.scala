package icu.harx

import scala.annotation.tailrec

/**
 * @see
 *   [[https://blog.rockthejvm.com/n-queens/ N-Queens in Scala: How to Approach Algorithm Questions]]
 */
object NQueens:
    case class RowCol(row: Int, col: Int)

    def apply(n: Int): List[List[Int]] =
        /** 越界 */
        def colOutOfBounds(rowCol: RowCol): Boolean = rowCol.col >= n

        /** 兄弟结点 */
        def brother(rowCol: RowCol): RowCol = RowCol(rowCol.row, rowCol.col + 1)

        /** 孩子结点 */
        def child(rowCol: RowCol): RowCol = RowCol(rowCol.row + 1, 0)

        /** 判断一个解是否为可行解 */
        def good(rowCols: List[RowCol]): Boolean =
            /** 约束方程 */
            def goodTwo(a: RowCol, b: RowCol): Boolean =
                (a.col != b.col) && (a.row != b.row) && ((a.col - b.col).abs != (a.row - b.row).abs)

            val testAll = for
                a <- rowCols
                b <- rowCols diff List(a)
            yield goodTwo(a, b)

            testAll forall true.equals

        /** 回溯法 深度优先 行有序 */
        @tailrec
        def backtracking(
          currPosition: RowCol,
          currQueens: List[RowCol],
          solutions: List[List[RowCol]]
        ): List[List[RowCol]] =
            // 列越界 && 棋盘为空 == 遍历了根的所有子树 ==> 返回结果
            if colOutOfBounds(currPosition) && currQueens.isEmpty then solutions
            // 列越界 && 棋盘不空 == 遍历了当前结点的子树 ==> 回溯, 切换到兄弟结点
            else if colOutOfBounds(currPosition) then
                val position = brother(currQueens.head)
                val queens   = currQueens.tail
                backtracking(position, queens, solutions)
            // 当前结点加入后冲突了 ==> 切换到当前结点的兄弟结点
            else if !good(currPosition :: currQueens) then
                val position = brother(currPosition)
                val queens   = currQueens
                backtracking(position, queens, solutions)
            // 已经到了最后一行且没有冲突 == 完成一个解 ==> 切换到兄弟结点
            else if currQueens.length == n - 1 then
                val position     = brother(currPosition)
                val queens       = currQueens
                val newSolutions = (currPosition :: currQueens) :: solutions
                backtracking(position, queens, newSolutions)
            // dfs 深度优先遍历
            else
                val position = child(currPosition)
                val queens   = currPosition :: currQueens
                backtracking(position, queens, solutions)

        backtracking(RowCol(0, 0), Nil, Nil)
            .map(solution => solution.map(rowCol => rowCol.col))


    @main
    def test(): Unit =
        println(apply(4))
        println(apply(8))
