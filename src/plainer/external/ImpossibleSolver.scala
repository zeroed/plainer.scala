package plainer.external

object  ImpossibleSolver extends App {
  type XY = (Int, Int)
  val step0 = for {
    x <- 1 to 100
    y <- 1 to 100
    if 1 < x && x < y && x + y < 100
  } yield (x, y)
 
  def sum(xy: XY) = xy._1 + xy._2
  def prod(xy: XY) = xy._1 * xy._2
  def sumEq(xy: XY) = step0 filter { sum(_) == sum(xy) }
  def prodEq(xy: XY) = step0 filter { prod(_) == prod(xy) }
 
  val step2 = step0 filter { sumEq(_) forall { prodEq(_).size != 1 }}
  val step3 = step2 filter { prodEq(_).intersect(step2).size == 1 }
  val step4 = step3 filter { sumEq(_).intersect(step3).size == 1 }
  println(step4)
}

/**
Puzzle

X and Y are two different integers, greater than 1, with sum less than 100. S and P are two mathematicians; S knows the sum X+Y, P knows the product X*Y, and both know the information in these two sentences. The following conversation occurs.

    P says "I cannot find these numbers."
    S says "I was sure that you could not find them. I cannot find them either."
    P says "Then, I found these numbers."
    S says "If you could find them, then I also found them."

What are these numbers?
Solution

The solution has X and Y as 4 and 13 (or vice versa), with P initially knowing the product is 52 and S knowing the sum is 17.

Initially P does not know the solution, since

    52 = 4 × 13 = 2 × 26

and S knows that P does not know the solution since all the possible sums to 17 within the constraints produce similarly ambiguous products. However, each can work out the solution by eliminating other possibilities following the other's statements and that is enough for the reader to find the solution given the constraints.
Detailed Solution

Mathematician P

P knows p=52. P suspects (2,26) and (4,13). P knows s=28 or s=17.

If s=28:

    S would suspect (2,26), (3,25), (4,24), (5,23), (6,22), (7,21), (8,20), (9,19), (10,18), (11,17), (12,16), and (13,15).
    S would know if (5,23) or (11,17), P would know the numbers.
    S would not be able to say "I was sure that you could not find them."

If s=17:

    S would suspect (2,15), (3,14), (4,13), (5,12), (6,11), (7,10), and (8,9).
    S would know that P would not know the numbers.
    S would be able to say "I was sure that you could not find them."

Therefore, when S says "I was sure that you could not find them," P eliminates (2,26) and learns (4,13) is the answer.

Mathematician S

S knows s=17. S suspects (2,15), (3,14), (4,13), (5,12), (6,11), (7,10), and (8,9). S knows p is 30, 42, 52, 60, 66, 70, or 72.

When P says "Then, I found these numbers," S knows his statement eliminated all but one possibility for P.

S simulates P's thinking
Case 1 (p=30)[show]
Case 2 (p=42)[show]
Case 3 (p=52)[show]
Case 4 (p=60)[show]
Case 5 (p=66)[show]
Case 6 (p=70)[show]
Case 7 (p=72)[show]

Only Case 3 eliminates all but one possibility for P. This is how S decides (4,13) is the answer.


The above solution is a verification, not a solution. It verifies that if P is given 52 and S is given 17, then P would could know and S would know. But, it has not established that (4,13) is the only answer. After the second question is answered, (i.e. S saying "I know you couldn't know"), is 52 necessarily the product P was given?

The answer to this is yes. An excel spreadsheet can be used to find the solution.

If x and y are the numbers, the two equations are x+y=S and xy=P. Solving for y and substituting gives x2-Sx+P=0

The excel spreadsheet looks for integer solutions for each S and P. Column B contains the sums (starting at row 4), row 3 contain the products (starting at C). A large table is created with sums down the side and products across the top. Sums can count by 2, displaying only the odd sums.

Formula in the middle of a large table is:

    IF($B4^2-4*C$3>0,($B4+SQRT($B4^2-4*C$3))/2,"") $B4 is the sum at left, S. C$3 is the product at top, P. (The dollar signs allow copying to the right and down, fixing the references to S and P)

The formula refers to the quadratic formula, which says if b2-4ac is positive, show the solution for x (one of the numbers), otherwise leave blank. The formula gives the largest of the two factors of the product.

The above formula shows decimal factors, so another IF statement can filter out the non-integers. The easiest thing to do is used a second table that refers to the first with this formula:

    IF($B56<>C$55+1,IF(C4<>"",IF(INT(C4)<>C4,"",C4),""),"") This filters out one group of impossible answers, and puts either a blank or the original integer into the parallel table. The net results is only integer value factors showing for each S and P.

It turns out that every row for a sum that's 2 past a prime represents a sum where S would not be sure that P can't know. That's because for such a row, the a possible product exists that would be 2 x prime #, and S would have to credit P with knowing the two factors. Ignore such rows.

Along the row for sum 17 under product 52 is the number 13. There are no other numbers in the same column that have odd factors for 52, and 13 is the only number for which this is the case.
*/