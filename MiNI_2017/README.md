# Warsztaty Badawcze na MiNI

## Motto

Z raportu [,,Przeszłość, teraźniejszość i przyszłość edukacji akademickiej''](http://www.wz.uw.edu.pl/pracownicyFiles/id12939-Billig_last.pdf), z punku widzenia socjologa

> Studenci preferują zajęcia nastawione na zdobycie potrzebnych na rynku pracy umiejętności praktycznych, opisywane przez nich często jako „atrakcyjne i łatwe” niż zajęcia „trudne i znojne”, nie zdając sobie sprawy z tego, że do odniesienia sukcesu w konkurencji na globalnym rynku potrzebna jest przede wszystkim sprawność myślenia, *a tę kształci się wychodząc poza sferę komfortu.*

## Cel

Celem naszych zajęć jest praca nad dwoma kompetencjami, ważnymi w pracy badawczej, a słabo rozwijanymi w standardowej edukacji:

- Głęboka praca nad danymi
- Dyskutowanie uzyskanych rozwiązań

Aby ten cel zrealizować będziemy pracować nad projketem związanym z badaniem PISA 2015,
pytanie badawcze, które nam przyświeca to:

  Czy uczniowie mają różne strategie związane z rozwiązywaniem testu (czasem przeznaczonym na różne zadania) i czy te strategie wpływają na końcowy wynik PISA?

## Dane i materiały

* Przykładowy kwestionariusz https://github.com/pbiecek/WarsztatyBadawcze/blob/master/MiNI_2017/materialy/EnglishStudentQuestionnaire.pdf
* Manual omawiający jak należy analizować dane z badania PISA https://github.com/pbiecek/WarsztatyBadawcze/blob/master/MiNI_2017/materialy/SPSS_Analysis_Manual.pdf
* Omowienie części wyników z badania PISA 2015 https://github.com/pbiecek/WarsztatyBadawcze/blob/master/MiNI_2017/materialy/PISA_2015_results.pdf

Dane PISA 2015 są dostępne na stronie http://www.oecd.org/pisa/data/2015database/. Krótka instrukcja jak wczytać te dane znajduje się tutaj
http://smarterpoland.pl/index.php/2016/12/pisa-2015-how-to-readprocessplot-the-data-with-r/


## Metodologia do omówienia

Na podstawie https://github.com/pbiecek/WarsztatyBadawcze/blob/master/MiNI_2017/materialy/SPSS_Analysis_Manual.pdf

* Jak wyglądają wagi w badaniu PISA? (rozdział 3)
* Jak wyglądają wagi replikacyjne? (rozdział 4)
* Jak wygląda model Racha? (rozdział 5)
* Czym są Plausible values? (rozdział 6)
* Jak liczyć błędy standardowe? (rozdział 7)
* Jak liczyć błędy standardowe dla miar PV? (rozdział 8)

## Pre

Pakiet BetaBit jest dostępny na CRAN (https://cran.r-project.org/web/packages/BetaBit/index.html). 
Zainstaluj go, a następnie rozwiąż możliwie wiele zagadek z zadań `proton`, `frequon` i `pearson`.
Wyniki jako skrypt knitra z kodem i wynikami prześlij na adres email.


## Ocena

### Zadanie 0
Aby móc zrealizować poniższe zadania, należy najpierw przygotowac ramkę danych z następującymi kolumnami (wiersz to para zadanie-student):

- CNT - id kraju
- CNTSCHID - id szkoły
- CNTSTUID - id studenta
- BOOKID - id kwestionariusza
- CBASCI - magiczna liczba dla permutacji zadań przyrodniczych
- item - nazwa zadania
- item_short - skrótowa nazwa zadania
- result - czy student rozwiązał zadanie czy nie?
- n.actions - ile akcji wykonał aby rozwiązać zadanie
- timing - ile czasu trwało rozwiązanie tego zadania
- position - pozycja, na której znajdowało się zadanie w danym kwestionariuszu
- ST016 - odpowiedź na pytanie ST016
- ST118 - odpowiedź na pytanie ST118

Taka ramka, będzie miała około 40 milionów wierszy. W przypadku problemów z pracą nad wszystkimi danymi, nalezy wybrać ~10 krajów i wykonać taką ramkę dla wybranych krajów.

### Ważne!!

Dane opisujące zadania są udostępnione w pliku 'Cognitive item data file'. Cześć zadań jest przeprowadzana na papierze (te nas nie interesują), część na komputerze. Nazwa przykładowej kolumny wygląda tak: `CM909Q01S`. Jak ją czytać?

- Pierwszy znak to C/P/D i opisuje czy zadanie było na papierze czy komputerze. Wartości C i D oznaczają zadania na komputerze, Wartość P oznacza zadanie papierowe (ignorujemy).
- Drugi znak S/M/R opisuje czy zadanie dotyczy czytania (R), matematyki (M) czy nauk przyrodniczych (S). Interesują nas wszystkie
- Kolejne trzy znaku to numer zadania. Tutaj jest to `909`
- Szóstym znakiem powinno być zawsze `Q`
- Kolejne dwa znaki to fragment zadania. Zazwyczaj to kolejne liczby, tutaj to `01`, ale jest też `02` itp. Każdy z framentów jest niezależnie oceniany.
- Kolejny znak opisuje co jest w danej kolumnie. `S` znaczy score, czyli w tej kolunnie jest informajca czy student rozwiązał to zadanie (Full credit) nie rozwiązał (No credit), rozwiązał częściowo lub nie widział na oczy. `T` oznacza imings, w tej kolumnie jest czas rozwiązywania zadania. `A` oznacza actions, czyli liczbę interakcji z komputerem zanim rozwiązano zadanie. Kozostałe litery sa praktycznie nieciekawe.

Czyli kolumny `CM909Q01S` i `CM909Q02S` opisują wyniki dwóch kolejnych podpunktów zadania `909` z matematyki, robionego na komputerze. Kolumny  `CM909Q01T` i `CM909Q02T` opisują czasy rozwiązywania zadań.

### Zadanie na 3



### Zadanie na 3,5



### Zadanie na 4



### Zadanie na 4,5



### Zadanie na 5





