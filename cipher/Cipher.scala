object Cipher{
  /** Bit-wise exclusive-or of two characters */
  def xor(a: Char, b: Char) : Char = (a.toInt ^ b.toInt).toChar

  /** Print ciphertext in octal */
  def showCipher(cipher: Array[Char]) =
    for(c <- cipher){ print(c/64); print(c%64/8); print(c%8); print(" ") }

  /** Read file into array */
  def readFile(fname: String) : Array[Char] = 
    scala.io.Source.fromFile(fname).toArray

  /** Read from stdin in a similar manner */
  def readStdin() = scala.io.Source.stdin.toArray

  /* ----- Functions below here need to be implemented ----- */

  /** Encrypt plain using key; can also be used for decryption */
  def encrypt(key: Array[Char], plain: Array[Char]) : Array[Char] =  {
    val n = plain.length
    val m = key.length
    var cipher = new Array[Char](n)
    for (i <- 0 until n) {
      cipher(i) = xor(key(i%m), plain(i))
    }
    return cipher
  }

  def testCrib(keyChars:Array[Char], j : Int) : Boolean = {
    val k = keyChars.length
    var i : String = new String (keyChars.slice(0, k - j))
    var x : String = new String (keyChars.slice(j,k))
    return i == x
  }

  /** Try to decrypt ciphertext, using crib as a crib */
  def tryCrib(crib: Array[Char], ciphertext: Array[Char]) : Unit = {
    val k = crib.length
    val n = ciphertext.length
    var keyChars = new Array[Char](k)
    var j = -1
    var i = -1
    var found = false
    while (j == -1 & i < n - k) {
      var c = ciphertext.slice(i, i + k)
      keyChars = encrypt(c, crib)
      //encrypt is symmetric
      j = 0
      while (j <= k - 2 && !found) {
        j += 1
        found = testCrib(keyChars, j)
      } 
     if (!found) {
        j = -1
      }
      i += 1
    }
    if (j == -1) {
      println("This crib doesn't decrypt the message :(")
    } 
    else {
      //now we have to shuffle it around to check if it is the correct one
      val shift = (i-1)%j
      var newKey = new Array[Char](j)
      for (i <- 0 until j) {
        newKey((i+shift)%j) = keyChars(i)
      }
      println(new String (newKey))
      val message = encrypt(newKey, ciphertext)
      println( new String (message))
    }
  }

  def getMatches(text: Array[Char], shift : Int) : Int = {
    var j = 0 
    val m = text.length
    var count = 0
    while (j < m - shift) {
      if (text(j) == text(shift + j)){
        count += 1
      }
      j += 1
    }
    return count
  }

  /** The first optional statistical test, to guess the length of the key */
  def crackKeyLen(ciphertext: Array[Char]) : Unit = {
    val n = 30
    var i = 1
    var m = 0
    while (i <= n) {
      m = getMatches(ciphertext, i)
      println(i.toString + ": " + m.toString)
      i += 1
    }
  }

  def getMatchChar(text : Array[Char], shift : Int, klen : Int) : Unit = {
    var newChar : Char = ' ' 
    val space : Char = ' '
    var j = 0 
    val m = text.length
    var position = 0
    while (j < m - shift) {
      if (text(j) == text(shift + j)){
        newChar = xor(space, text(j))
        position = j%klen
        if ((newChar.toInt >= 32) && (newChar.toInt <= 127)){
          println(position.toString + " " + newChar)
        }
      }
      j += 1
    }

  }

  /** The second optional statistical test, to guess characters of the key. */
  def crackKey(klen: Int, ciphertext: Array[Char]) : Unit = {
    var newShift = klen
    val n = ciphertext.length 
    while (newShift < n) {
      getMatchChar(ciphertext, newShift, klen)
      newShift += klen
    }
  }

/** The main method just selects which piece of functionality to run */
  def main(args: Array[String]) = {
    // string to print if error occurs
    val errString = 
      "Usage: scala Cipher (-encrypt|-decrypt) key [file]\n"+
      "     | scala Cipher -crib crib [file]\n"+
      "     | scala Cipher -crackKeyLen [file]\n"+
      "     | scala Cipher -crackKey len [file]"

    // Get the plaintext, either from the file whose name appears in position
    // pos, or from standard input
    def getPlain(pos: Int) = 
      if(args.length==pos+1) readFile(args(pos)) else readStdin()

    // Check there are at least n arguments
    def checkNumArgs(n: Int) = if(args.length<n){println(errString); sys.exit}

    // Parse the arguments, and call the appropriate function
    checkNumArgs(1)
    val command = args(0)
    if(command=="-encrypt" || command=="-decrypt"){
      checkNumArgs(2); val key = args(1).toArray; val plain = getPlain(2)
      print(new String (encrypt(key,plain)))
    }
    else if(command=="-crib"){
      checkNumArgs(2); val key = args(1).toArray; val plain = getPlain(2)
      tryCrib(key, plain)
    }
    else if(command=="-crackKeyLen"){
      checkNumArgs(1); val plain = getPlain(1)
      crackKeyLen(plain)
    }      
    else if(command=="-crackKey"){
      checkNumArgs(2); val klen = args(1).toInt; val plain = getPlain(2)
      crackKey(klen, plain)
    }
    else println(errString)
  }
}

/* Key: PEMBERLey

Be not alarmed, Madam, on receiving this letter, by the
apprehension of its containing any repetition of those
sentiments, or renewal of those offers, which were last night
so disgusting to you.  I write without any intention of paining
you, or humbling myself, by dwelling on wishes, which, for the
happiness of both, cannot be too soon forgotten; and the effort
which the formation and the perusal of this letter must
occasion should have been spared, had not my character required
it to be written and read.  You must, therefore, pardon the
freedom with which I demand your attention; your feelings, I
know, will bestow it unwillingly, but I demand it of your
justice. */

/* Key: HOGWARTS

HOGWARTS SCHOOL of WITCHCRAFT and WIZARDRY
Headmaster: Albus Dumbledore (Order of Merlin, First Class, Grand
Sorc., Chf. Warlock, Supreme Mugwump, International Confed. of
Wizards)
Dear Mr Potter,
We are pleased to inform you that you have been accepted at Hogwarts
School of Witchcraft and Wizardry. Please find enclosed a list of all
necessary books and equipment. Term begins on 1 September. We await
your owl by no later than 31 July.
Yours sincerely, Minerva McGonagall. Deputy HeadmistressY% */
