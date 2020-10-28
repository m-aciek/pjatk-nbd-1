object a1 extends App {
  //  1. Stwórz 7 elementową listę zawierającą nazwy dni tygodnia. Napisz funkcję tworzącą w oparciu o nią stringa
  //  z elementami oddzielonymi przecinkami korzystając z:
  // a. Pętli for
  val dni = List(
    "poniedziałek",
    "wtorek",
    "środa",
    "czwartek",
    "piątek",
    "sobota",
    "niedziela"
  )

  var string = ""
  var dzień = null
  var pierwszy = true

  for (dzień <- dni) {
    if (!pierwszy)
      string = string.concat(", ");
    string = string.concat(dzień);
    pierwszy = false;
  }
  println(string)

  // b. Pętli for wypisując tylko dni z nazwami zaczynającymi się na „P”
  string = ""
  dzień = null
  pierwszy = true

  for (dzień <- dni) {
    if (dzień.take(1) == "p") {
      if (!pierwszy)
        string = string.concat(", ");
      string = string.concat(dzień);
      pierwszy = false;
    }
  }
  println(string)

  // c. Pętli while
  string = ""
  var indeks = 0
  pierwszy = true


  while (indeks < dni.size) {
    if (!pierwszy)
      string = string.concat(", ");
    string = string.concat(dni(indeks));
    pierwszy = false;
    indeks += 1
  }
  println(string)

  // 2. Dla listy z ćwiczenia 1 napisz funkcję tworzącą w oparciu o nią stringa z elementami oddzielonymi przecinkami
  // korzystając z:
  //
  // a. Funkcji rekurencyjnej
  def rozdzielPrzecinkami(lista: List[String]): String = {
    if (lista.isEmpty)
      ""
    else if (lista.size == 1)
      lista.last
    else
      rozdzielPrzecinkami(lista.dropRight(1)).concat(", ").concat(lista.last)
  }
  println(rozdzielPrzecinkami(dni))

  // b. Funkcji rekurencyjnej wypisując elementy listy od końca
  def odKońcaRozdzielPrzecinkami(lista: List[String]): String = {
    if (lista.isEmpty)
      ""
    else if (lista.size == 1)
      lista.head
    else
      odKońcaRozdzielPrzecinkami(lista.drop(1)).concat(", ").concat(lista.head)
  }
  println(odKońcaRozdzielPrzecinkami(dni))

  // 3. Stwórz funkcję korzystającą z rekurencji ogonowej do zwrócenia oddzielonego przecinkami stringa zawierającego
  // elementy listy z ćwiczenia 1
  def rozdzielPrzecinkamiOgonowo(lista: List[String]): String = {
    if (lista.isEmpty)
      ""
    else if (lista.size == 1)
      lista.head
    else
      lista.head.concat(", ").concat(rozdzielPrzecinkamiOgonowo(lista.drop(1)))
  }
  println(rozdzielPrzecinkami(dni))

  // 4. Dla listy z ćwiczenia 1 napisz funkcję tworzącą w oparciu o nią stringa z elementami oddzielonymi przecinkami
  // korzystając z:
  //
  // a. Metody foldl
  def rozdzielPrzecinkamiFoldl(lista: List[String]): String = {
    lista.tail.foldLeft(lista.head)((a, b) => a + ", " + b)
  }
  println(rozdzielPrzecinkamiFoldl(dni))

  // b. Metody foldr
  def rozdzielPrzecinkamiFoldr(lista: List[String]): String = {
    lista.dropRight(1).foldRight(lista.last)(_ + ", " + _)
  }
  println(rozdzielPrzecinkamiFoldr(dni))

  // c. Metody foldl wypisując tylko dni z nazwami zaczynającymi się na „P”
  def rozdzielPrzecinkamiNaPFoldl(lista: List[String]): String = {
    lista.tail.filter(_.take(1) == "p").foldLeft(lista.head)(_ + ", " + _)
  }
  println(rozdzielPrzecinkamiNaPFoldl(dni))

  // 5. Stwórz mapę z nazwami produktów i cenami. Na jej podstawie wygeneruj drugą, z 10% obniżką cen. Wykorzystaj
  // mechanizm mapowania kolekcji.
  val produkty = Map("masło" -> BigDecimal(6), "mąka" -> BigDecimal(3), "ciastka" -> BigDecimal(5.5))
  println(produkty.transform((_, cena) => 0.9 * cena))

  // 6. Zdefiniuj funkcję przyjmującą krotkę z 3 wartościami różnych typów i wypisującą je
  def wypiszKrotkę(krotka:(Boolean, Int, String)) = {
    println(krotka._1, krotka._2, krotka._3)
  }
  wypiszKrotkę((true, 0, "foo"))

  // 7. Zaprezentuj działanie Option na dowolnym przykładzie (np. mapy, w której wyszukujemy wartości po kluczu)
  def wypiszWielkimi(nazwa: Option[String]) {
    val wielkimi = nazwa map {
      _.trim
    } filter {
      _.length != 0
    } map {
      _.toUpperCase
    }
    println(wielkimi getOrElse "")
  }
  wypiszWielkimi(Some(""))
  wypiszWielkimi(Some("foo"))

  // 8. Napisz funkcję usuwającą zera z listy wartości całkowitych (tzn. zwracającą listę elementów mających wartości
  // różne od 0). Wykorzystaj rekurencję.
  val całkowite = List(1, 0, 2, 0, 4, 5, 0, 3, 1)
  def usuńZera(lista: List[Int], indeks: Int): List[Int] = {
    if (indeks == lista.size)
      lista
    else if (lista(indeks) == 0)
      usuńZera(lista.take(indeks) ++ lista.drop(indeks + 1), indeks + 1)
    else
      usuńZera(lista, indeks + 1)
  }
  println(usuńZera(całkowite, 0))

  // 9. Zdefiniuj funkcję, przyjmującą listę liczb całkowitych i zwracającą wygenerowaną na jej podstawie listę, w
  // której wszystkie liczby zostały zwiększone o 1. Wykorzystaj mechanizm mapowania kolekcji
  def zwiększone(lista: List[Int]) = {
    lista.map(_ + 1)
  }
  println(zwiększone(całkowite))

  // 10. Stwórz funkcję przyjmującą listę liczb rzeczywistych i zwracającą stworzoną na jej podstawie listę zawierającą
  // wartości bezwzględne elementów z oryginalnej listy należących do przedziału <-5,12>. Wykorzystaj filtrowanie.
  val rzeczywiste = List(-5.5, -4, 1, 13)
  def małeBezwględne(lista: List[Double]) = {
    lista.filter(_ >= -5).filter(_ <= 12).map(_.abs)
  }
  println(małeBezwględne(rzeczywiste))
}
