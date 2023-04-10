package lab03

def IntToBin(liczba: Int): Int = {
    @annotation.tailrec
    def pomocnicza(liczba:Int, acc:Int): Int = {
        if (liczba == 0) acc
        else {
            // mnozenie przez 10 to przesuniecie o jedna cyfre w lewo
            // dodanie reszty z dzielenia przez 2 to dodanie cyfry w jednosciach
            pomocnicza(liczba/2, liczba%2 + 10*acc) 
        }
    }
    pomocnicza(liczba, 0)   
}

@main 
def zad_04(liczba: Int): Unit = {
    require(liczba>=0)
    //Napisz funkcję IntToBin, która dla podanej liczby naturalnej zwróci jej reprezentację w systemie binarnym
    val binarna = IntToBin(liczba)
    println(s"$liczba w systemie binarnym jest zapisywana jako $binarna")
}

