package lab03

def hipoteza(liczba: Int): Unit = {
    @annotation.tailrec
    def pomocnicza(liczba: Int, liczba2: Int): Unit = {
        if (liczba2 == 0) println("Nie znaleziono liczb pierwszych")
        else if (czyPierwsza(liczba2, 2) == true && czyPierwsza(liczba - liczba2, 2) == true) println(s"$liczba2 i ${liczba - liczba2}")
        else pomocnicza(liczba, liczba2 - 1)
    }
    @annotation.tailrec
    def czyPierwsza(num: Int, divisor: Int): Boolean = {
        if (num == 1 ) return false
        if (num % divisor == 0) return false
        else if (divisor * divisor > num) return true
        else czyPierwsza(num, divisor + 1)   
    }
    pomocnicza(liczba, liczba-1)
 
}

@main 
def zad_02(liczba: Int): Unit = {
    require(liczba>2)
    require(liczba%2==0)
    //Zdefiniuj funkcję hipoteza, która jako argument pobiera 
    //parzystą liczbę naturalną większą od 2 oraz 
    //sprawdza czy jest ona sumą dwóch liczb pierwszych. 
    //Jeżeli tak, to funkcja hipoteza powinna wypisać je na konsoli. 
    //W przeciwnym wypadku na konsoli powinien pojawić się komunikat mówiący, że 
    //liczb takich nie udało sie znaleźć.
    hipoteza(liczba)
}