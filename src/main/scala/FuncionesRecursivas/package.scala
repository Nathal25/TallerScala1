package object FuncionesRecursivas {
  /**
   * Ejercicio 1.1.1
   * maximo comun divisor a partir del teorema fundamental de la aritmetica
   * only use  isEmtpy, head y tail.
   */
  def mcdTFA(ln: List[Int], lm: List[Int], primos: List[Int]): Int = {
    def selectMin(ln: List[Int], lm: List[Int], primos: List[Int], acumulator: Int): Int = {
      if (ln.isEmpty || lm.isEmpty || primos.isEmpty) {
        acumulator
      } else {
        val lowestExpo = if (ln.head < lm.head) ln.head else lm.head
        val operation = potencia(primos.head, lowestExpo)
        selectMin(ln.tail, lm.tail, primos.tail, acumulator * operation)
      }
    }

    def potencia(base: Int, exponente: Int): Int = {
      def aux(base: Int, exponente: Int, resultado: Int): Int = {
        if (exponente == 0) resultado
        else aux(base, exponente - 1, resultado * base)
      }

      aux(base, exponente, 1)
    }

    selectMin(ln, lm, primos, 1)
  }
  /**
   * Ejerccio 1.1.2
   * maximo comun divisor a partir del teorema de Euclides con coeficientes de Bezout
   */
  
  def mcdEBez(n: Int, m: Int): (Int, Int, Int) = {
    if (m==0) {
      //caso base
      (n,1,0)
    } else {
      val (d,x1,y1) = mcdEBez(m,n%m)

      val x=y1
      val y=x1-(n/m)*y1

      (d,x,y)
    }
  }


  /**
   * Ejercicio 1.2.1
   * fibonacci recursivo de arbol
   */
  def fibonacciA(n: Int): Int = {
    if (n <= 0) 1
    else if (n == 1) 1
    else fibonacciA(n - 1) + fibonacciA(n - 2)
  }
  /**
   * Ejercicio 1.2.2
   * fibonacci iterativo
   */
  def fibonacciI(n: Int): Int = {
    def contar(a: Int, b: Int, count: Int): Int = {
      if (count >= n) b
      else contar(b, a + b, count + 1)
    }
    if (n <= 0) 1
    else contar(1, 1, 1) //las sumas empiezan en 1, al igual que el contador
  }
}
