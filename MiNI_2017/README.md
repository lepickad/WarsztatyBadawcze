# Warsztaty Badawcze na MiNI

## Motto

Z raportu [,,Przeszłość, teraźniejszość i przyszłość edukacji akademickiej''](http://www.wz.uw.edu.pl/pracownicyFiles/id12939-Billig_last.pdf), z punku widzenia socjologa

> Studenci preferują zajęcia nastawione na zdobycie potrzebnych na rynku pracy umiejętności praktycznych, opisywane przez nich często jako „atrakcyjne i łatwe” niż zajęcia „trudne i znojne”, nie zdając sobie sprawy z tego, że do odniesienia sukcesu w konkurencji na globalnym rynku potrzebna jest przede wszystkim sprawność myślenia, *a tę kształci się wychodząc poza sferę komfortu.*

## Cel

Celem naszych zajęć jest praca nad dwoma kompetencjami, ważnymi w pracy badawczej, a słabo rozwijanymi w standardowej edukacji:

- Głęboka praca nad danymi
- Dyskutowanie uzyskanych rozwiązań

Aby ten cel zrealizować będziemy pracować nad projektem związanym z badaniem PISA 2015,
pytanie badawcze, które nam przyświeca to:

  *Czy uczniowie mają różne strategie związane z rozwiązywaniem testu (czasem przeznaczonym na różne zadania) i czy te strategie wpływają na końcowy wynik PISA?*

## Na start

Wszystkie wyniki należy zgłaszać przez GitHuba https://github.com/pbiecek/WarsztatyBadawcze/tree/master/MiNI_2017/projekty.

Postęp prac można śledzić na https://waffle.io/pbiecek/WarsztatyBadawcze. Zespół przed oddaniem kolejnego etapu, powinien zgłosić nowy issue a w nazwie podać nazwę etapu i nazwę zespołu.

Wszyscy członkowie zespołu otrzymują tą samą ocenę.

Każdy zespół powinien wybrać jedną osobę, kapitana, przez którego prowadzona będzie komunikacja.

## Plan spotkań

* 2017-03-03 Omówienie problemu do rozwiązania
* 2017-03-10 Przedstawienie metodologii analizy danych PISA
* 2017-03-17 Indywidualne spotkania
* 2017-03-31 Prezentacja obecnego stanu rozwiązań
* 2017-04-07 Indywidualne spotkania
* 2017-04-21 Prezentacje uzyskanych rozwiązań

## Dane i materiały

* Przykładowy kwestionariusz https://github.com/pbiecek/WarsztatyBadawcze/blob/master/MiNI_2017/materialy/EnglishStudentQuestionnaire.pdf
* Manual omawiający jak należy analizować dane z badania PISA https://github.com/pbiecek/WarsztatyBadawcze/blob/master/MiNI_2017/materialy/SPSS_Analysis_Manual.pdf
* Omowienie części wyników z badania PISA 2015 https://github.com/pbiecek/WarsztatyBadawcze/blob/master/MiNI_2017/materialy/PISA_2015_results.pdf

Dane PISA 2015 są dostępne na stronie http://www.oecd.org/pisa/data/2015database/. Krótka instrukcja jak wczytać te dane znajduje się tutaj
http://smarterpoland.pl/index.php/2016/12/pisa-2015-how-to-readprocessplot-the-data-with-r/

W pliku https://github.com/pbiecek/WarsztatyBadawcze/blob/master/MiNI_2017/materialy/sheet_structure.xlsx znajduje się struktura kolejności zadań dla czterech głównych klastrów.
Kolejność klastrów przyrodniczych w połączeniu z ''magiczną liczbą'' jest opisana w pliku https://github.com/pbiecek/WarsztatyBadawcze/blob/master/MiNI_2017/materialy/science_cluster_encoding.xlsx. Przydział do klastrów jest w pliku https://github.com/pbiecek/WarsztatyBadawcze/blob/master/MiNI_2017/materialy/science_cluster_combination.xlsx.



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

### Zadanie 0 - [PrzygotowanieDanych]
Aby móc zrealizować poniższe zadania, należy najpierw przygotować ramkę danych z następującymi kolumnami (wiersz to para zadanie-student):

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

Taka ramka, będzie miała około 40 milionów wierszy. W przypadku problemów z pracą nad wszystkimi danymi, należy wybrać ~10 krajów i wykonać taką ramkę dla wybranych krajów.

### Ważne!!

Dane opisujące zadania są udostępnione w pliku 'Cognitive item data file'. Cześć zadań jest przeprowadzana na papierze (te nas nie interesują), część na komputerze. Nazwa przykładowej kolumny wygląda tak: `CM909Q01S`. Jak ją czytać?

- Pierwszy znak to C/P/D i opisuje czy zadanie było na papierze czy komputerze. Wartości C i D oznaczają zadania na komputerze, Wartość P oznacza zadanie papierowe (ignorujemy).
- Drugi znak S/M/R opisuje czy zadanie dotyczy czytania (R), matematyki (M) czy nauk przyrodniczych (S). Interesują nas wszystkie
- Kolejne trzy znaku to numer zadania. Tutaj jest to `909`
- Szóstym znakiem powinno być zawsze `Q`
- Kolejne dwa znaki to fragment zadania. Zazwyczaj to kolejne liczby, tutaj to `01`, ale jest też `02` itp. Każdy z fragmentów jest niezależnie oceniany.
- Kolejny znak opisuje co jest w danej kolumnie. `S` znaczy score, czyli w tej kolumnie jest informacja czy student rozwiązał to zadanie (Full credit) nie rozwiązał (No credit), rozwiązał częściowo lub nie widział na oczy. `T` oznacza timings, w tej kolumnie jest czas rozwiązywania zadania. `A` oznacza actions, czyli liczbę interakcji z komputerem zanim rozwiązano zadanie. Pozostałe litery są praktycznie nieciekawe.

Czyli kolumny `CM909Q01S` i `CM909Q02S` opisują wyniki dwóch kolejnych podpunktów zadania `909` z matematyki, robionego na komputerze. Kolumny  `CM909Q01T` i `CM909Q02T` opisują czasy rozwiązywania zadań.

### Zadanie na 3 [StatystykiKrajow]

W tym zadaniu należy na podstawie tabeli, stworzone w punkcie [PrzygotowanieDanych], przygotować dla każdego zadania statystyki jeżeli chodzi o wyniki i czas rozwiązywania w podziale na kraje.

Wynikiem tego zadania jest plik knitrowy (html) z kodem R oraz statystykami czasów rozwiązywania i wyników rozwiązywania w podziale na kraje.

### Zadanie na 3,5 [Pozycja]

W tym zadaniu, należy uwzględnić kolejność rozwiązywania zadań. Statystyki z punktu [StatystykiKrajow] należy wyznaczyć osobno dla zadań prezentowanych na pozycji 1/2/3/4. Należy sprawdzić jak zachowują się profile odpowiedzi/czasu na odpowiedzi w zależności od kolejności rozwiązywania zadań.

### Zadanie na 4 [Segmentacja]

W tym zadaniu, należy na podstawie danych z zadania [PrzygotowanieDanych], zadania [StatystykiKrajow] i zadania [Pozycja] opracować podział strategii uczniów na od 4 do 9 nazwanych i zdefiniowanych segmentów. Przykłady segmentów - uczniowie poświęcający dużo czasu na początku testu a później szybko rozwiązujący zadania; uczniowie poświęcający dużo czasu na zadania, którzy nie zdążą rozwiązać całego testu; uczniowie którzy poświęcają mało czasu na zadania których nie potrafią rozwiązać a więcej czasu na te które potrafią rozwiązać.

Należy następnie porównać kraje/szkoły pod kątem występowania poszczególnych strategii.

### Zadanie na 4,5 [EfektStrategii]

Należy odpowiedzieć na pytanie, czy różnice w stosowanych strategiach mają wpływ na końcowy wynik osiągnięty w badaniu PISA.
Oraz jak wyglądałby końcowy ranking PISA gdyby usunąć wpływ stosowanej strategii.

### Zadanie na 5 [RaportBadawczy]

Należy przygotować raport w języku angielskim podsumowujący przeprowadzone badanie, razem z odniesieniami do literatury na tematy podobnych badań, z podsumowaniem uzyskanych wyników i dyskusją końcową.
Raport powinien mieć nie więcej niż 4 strony (licząc wykresy i ewentualne tabele).
