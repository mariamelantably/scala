def binary_root(y : Int) : Int = {
  require (y >=0 )
  if (y <= 1) return y 
  var a = 0 ; var b  = y
  while (a + 1 < b){
    var m = (a/2) + (b/2)
    if (a%2 == 1 && b%2 == 1) m += 1
    if (m <= (y/m)) a = m else b = m 
  }
  a
}

//Part (b)
def tenary_root(y : Int) : Int = {
  require (y >= 0)
  if (y <= 1) return y 
  var a = 0; var b = y; 
  //Inv: a^2 <= y < b^2, 0 <= a < b <= y
  //Var: b - (a+1)
  //Pre-condition of Inv: y >= a^2 = 0, by pre-condition, and y < y^2 as y != 1,
  while (a + 1 < b){
    var m = (a/2) + (b/2) //find interval bounds without any overflows by doing divisions after additions
    if (a%2 == 1 && b%2 == 1) m += 1
    var c = m/2 + a/2
    if (a%2 == 1 && m%2 == 1) c += 1
    var d = m/2 + b/2
    if (m%2 == 1 && b%2 == 1) d += 1
      if (c > (y/c)) b = c else { //find correct range for next interval, this method avoids squaring which can cause overflow errors
        if (d > (y/d)) {a = c ; b = d} else {a=d}
      } 
  }
  // Post condition of Inv:
  // Inv ∧ ¬(a + 1 < b) -> a^2 <= y <= b^2 ∧ b = a + 1 -> a^2 <= y < (a+1)^2 -> a = ⌊√y⌋
  a
}