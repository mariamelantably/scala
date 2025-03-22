object GCD{
    /** Calculate the quotient and remainder of x divided by y
     * Pre: x >= 0, y > 0
     * Post: returns a tuple of the quotient and the remainder, (q,r), 0 <= r < y */
    def euclideanDivision(x : Int, y : Int) : (Int, Int) = {
        var q = 0
        var r = x
        //Invariant: x = q*y + r and 0 <= r <= x.
        while (r >= y) {
            q += 1
            r = x - q*y
        }
        (q,r)
    }

    def gcdBezout(m: Int, n : Int) : (Int, Int, Int) = {
        var arr = euclideanDivision(m,n)
        var q = arr(0)
        var r = arr(1)
        var x0 = 0; var y0 = 1
        var x1 = 1; var y1 = -1*q
        var a = n
        var b = r
        //Invariant: x1*m + y1*n = b, 
        while (b != 0) {
            println(a.toString + "  " + b.toString + "  " + x0.toString + "  " + y0.toString + "  " + x1.toString + "  " + y1.toString)
            var arr = euclideanDivision(a,b)
            var q = arr(0)
            var r = arr(1)
            if (r != 0){
                var tx = x1
                var ty = y1
                x1 = x0 - q*x1
                y1 = y0 - q*y1
                x0 = tx
                y0 = ty
            }
            a = b
            b = r
        }
        return (a, x1, y1)
    }

    def main(args : Array[String]) : Unit = {
        //This code expects to receive two inputs - the numbers to use in the algorithm
        if (args.length != 2) {println("Expected two arguments for the algorithm")}
        else {
            val m = args(0).toInt
            val n = args(1).toInt 
            val (x,y,z) = gcdBezout(m,n)
            println("For the integers " + m.toString + " " + n.toString + " gcd = " + x.toString + " x = " + y.toString + " y = " + z.toString)
        }
    }
}