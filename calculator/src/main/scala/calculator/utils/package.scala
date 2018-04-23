package calculator

package object utils {
  /**
    * Opération à un opérande
    * @param operator opérateur
    * @param op opérande
    * @return résultat de l'opération
    */
  def op(operator: Char, op :Double) : Double = {
    operator match {
      case '!' => if (!op.isValidInt) throw new Error("Opérande invalide, doit être entier.") else factorial(op.toInt)
      case _ => throw new Error("Opérateur inconnu.")
    }
  }
  /**
    * Opération à deux opérande
    * @param operator opérateur
    * @param op1 première opérande
    * @param op2 seconde opérande
    * @return résultat de l'opération
    */
  def op(operator: Char, op1 : Double, op2 : Double) : Double = {
    operator match {
      case '+' => op1 + op2
      case '-' => op1 - op2
      case '*' => op1 * op2
      case '/' => op1 / op2
      case '%' => op1 % op2
      case '^' =>  if (!op2.isValidInt) throw new Error("Opérande invalide, second opérande doit être entier.") else power(op1, op2.toInt)
      case _ => throw new Error("Opérateur inconnu.")
    }
  }

  /**
    * Calcul le carré de x
    * @param x opérande
    * @return carré de x
    */
  def square(x: Double) = x * x

  /**
    * Calcul la puissance d'un ombre. L'exposant doit être entier,
    * le cas d'un exposant réel n'est pas implémenté
    * @param x opérande
    * @param n exposant (entier)
    * @return x puissance n
    */
  def power(x: Double, n: Int): Double = {
    n match {
      case 0 => 1.0
      case a if (n < 0) => 1 / power(x, -n)
      case b if (n % 2 == 0) =>square(power(x, n / 2))
      case _ => x * power(x, n - 1)
    }
  }


  /**
    * Valeur absolue de x
    * @param x opérande
    * @return valeur absolue de x
    */
  def abs(x: Double) ={
    if(x < 0 ) -x
    else x
  }


  /**
    * Racine carré de n
    * @param n opérande
    * @return racine carré de n
    */
  def sqrt(n: Double) = {
    val epsilon = 0.0001
    def calcSqrt(n: Double, x : Double): Double = {
      if((abs(square(x) -n )/n) < epsilon) x
      else calcSqrt(n, ((x + n/x)/2))
    }

    n match{
      case 0.0 => 0.0
      case x if(n < 0) => throw new Error("Opérande invalide, doit être positif")
      case _ =>{
        calcSqrt(n, 1)
      }
    }
  }



  /**
    * Calcul la factoriel de n de manière récursive terminal
    * @param n opérande
    * @return factoriel de n
    */
  def factorial(n: Int): Int = {
    if(n < 0) throw new Error("Opérande invalide, doit être positive")
    def loop(acc: Int, n: Int) : Int = {
      if (n == 0) acc
      else loop(acc * n, n - 1)
    }

    loop(1, n)
  }

  /**
    * Calcul le plus grand diviseur commun
    * @param a premier opérande
    * @param b second opérande
    * @return plus grand diviseur commun de a et b
    */
  def gcd(a: Int, b: Int) : Int =
    if (b == 0) a else gcd(b, a % b)


  /*
   * Mémoire de la machine
   */
  var memory: Map[String, Double] = Map()

  /**
    * Enregistrer une variable dans la mémoire.
    * Lève une exception si le nom de la variable est invalide.
    * @param variable nom de la variable
    * @param value valeur de la variable
    */
  def saveVariable(variable: String, value: Double) : Unit = {
    if (!variable.matches("[a-zA-Z]+")) {
      throw new Error("Nom de variable invalide. Seulement a-zA-Z")
    }
    memory += variable -> value
  }

  /**
  Récupérer la valeur d'une variable. Lève une exception
  si la variable n'existe pas dans la mémoire.
    * @param key nom de la variable
    * @return valeur de la variable
    */
  def getVariable(key: String) : Double = {
    memory(key)
  }


  /**
    * Retourne si x est un nombre premier
    * (Les valeurs < 2 ne sont pas considérées comme nombre premier)
    * @param x opérande
    * @return si x est un ombre premier
    */
  def primeNumber(x: Int) : String ={
    def isPrimeNumber(a: Int, b:Int) : String = {
      if(b >= sqrt(a)) a + " is a prime number"
      else if(a % b == 0) "Not a prime number"
      else isPrimeNumber(a, b + 1)
    }

    if (x < 2) "Not a prime number"
    else isPrimeNumber(x, 2)
  }

  /*
    * Algorithme d'euclide étendu
    * Retourne les coefficients de Bézout (u,v)
    */
  def eucl(r: Int, u: Int, v: Int, rp: Int, up: Int, vp: Int): (Int, Int) = {
    if(rp == 0)
      return (u, v)
    else
      eucl(rp, up, vp, r - (r / rp) * rp, u - (r / rp) * up, v - (r / rp) * vp)
  }
  /**
    * Algorithme d'euclide étendu
    * @param a 1er opérande
    * @param b 2eme opérande
    * @return
    */
  def egcd(a: Int, b: Int): Int = {
    val r = eucl(a, 1, 0, b, 0, 1)
    return a * r._1 +  b * r._2
  }


  /**
    * Calcul l'inverse modulaire
    * @param a premier opérande
    * @param b second opérande
    * @return inverse modulaire
    */
  def modInvert(a: Int, b: Int): Int = {
    val r = eucl(a, 1, 0, b, 0, 1)
    val e = a * r._1 +  b * r._2

    if(e != 1) throw new Error("No modular multiplicative inverse")

    return r._1
  }


  /**
    * Résoud une équation du second degré
    * @param a coefficient de x2
    * @param b coefficient de x
    * @param c coeffeicient
    * @return valeurs de x
    */
  def solve(a: Double, b: Double, c: Double): Any = {
    def delta(a: Double, b: Double, c: Double) = square(b) - 4 * a * c

    // Si a = 0 alors ce n'est pas une equation du 2ème degré
    if (a == 0) throw new Error("Syntax Error")

    val d = delta(a, b, c)
    d match {
      // delta = 0 : solution unique
      case 0 => List() :+ (-b) / (2 * a)
      // delta > 0 : 1ère solution et 2ème solution
      case x if (d > 0) => {
        List() :+ ((-b + sqrt(d)) / 2 * a) :+ ((-b - sqrt(d)) / 2 * a)
      }
      // delta < 0 :  réelle et tuple(1ère complete, 2ème complexe)
      case _ => {
        List() :+ (-b) / (2 * a) :+ (sqrt((-d)) / (2 * a), (-sqrt((-d))) / (2 * a))
      }
    }
  }
}
