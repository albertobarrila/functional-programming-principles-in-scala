package recfun

import scala.annotation.tailrec
import java.util.Stack

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do print(s"${pascal(col, row)} ")
      println()

  /** Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    @tailrec
    def loop(c0: Int, r0: Int, pred: Array[Int], cur: Array[Int]): Int =
      cur(c0) =
        if (c0 == 0 || c0 == cur.length - 1) 1 else pred(c0 - 1) + pred(c0)
      if ((c0 == c) && (r0 == r)) cur(c0)
      else if (c0 + 1 < cur.length) loop(c0 + 1, r0, pred, cur)
      else loop(0, r0 + 1, cur, new Array(_length = cur.length + 1))

    if (c == 0 && r == 0) 1
    else loop(0, 1, Array(1), new Array(_length = 2))

  /** Exercise 2
    */
  def balance(chars: List[Char]): Boolean =
    @tailrec
    def loop(chars: List[Char], acc: List[Char]): Boolean =
      if (chars.isEmpty) acc.isEmpty
      else if (chars.head == '(') loop(chars.tail, '(' :: acc)
      else if (chars.head == ')') !acc.isEmpty && loop(chars.tail, acc.tail)
      else loop(chars.tail, acc)

    loop(chars, List[Char]())

  /** Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    def loop(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty) 0
      else if (money == 0) 1
      else loop(money, coins.tail) + loop(money - coins.head, coins)
    }

    loop(money, coins)
