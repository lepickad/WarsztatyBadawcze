# Warsztaty Badawcze na MiNI

## Motto

Z raportu ,,Przeszłość, teraźniejszość i przyszłość edukacji akademickiej'', z punku widzenia socjologa

> Studenci preferują zajęcia nastawione na zdobycie potrzebnych na rynku pracy umiejętności praktycznych, opisywane przez nich często jako „atrakcyjne i łatwe” niż zajęcia „trudne i znojne”, nie zdając sobie sprawy z tego, że do odniesienia sukcesu w konkurencji na globalnym rynku potrzebna jest przede wszystkim sprawność myślenia, *a tę kształci się wychodząc poza sferę komfortu.*

## Cel

Celem naszych zajęć jest praca nad dwoma kompetencjami, ważnymi w pracy badawczej, a słabo rozwijanymi w standardowej edukacji:

- Iteracyjna praca nad danymi
- Opisywanie uzyskanych rozwiązań

Aby ten cel zrealizować będziemy pracować nad zadaniem:
,,Identyfikacja czynników różnicujących prognozy w rokowaniach dalszego czasu życia w zależności od tła genetycznego''

## Założenia

Pracujemy w małych grupach: 2-3 osoby

Każda grupa wybiera jedną z platform opisujących dane genetyczne (jedną platformę mogą analizować nie więcej niż dwie grupy)

* Białka (`RTCGA.RPPA`)
* Ekspresja genów (`RTCGA.mRNA`)
* Ekspresja genów (`RTCGA.miRNASeq`)
* Mutacje genów (`RTCGA.mutations`)
* Profil metylacji (`RTCGA.methylation`)
* Profil duplikacji na genomie (`RTCGA.CNV`)

Informacje kliniczne znajdują się w pakiecie `RTCGA.clinical`.

W zadaniu należy 

* Dla zadanej platformy, dla każdego z dużych nowotworów należy zidentyfikować biomarkery (w liczbie 100-200), które korelują z czasem przeżycia (albo stosując regresję logistyczną i 5-letni czas życia albo analizę przeżycia),
* Należy zbadać jaka jest część wspólna/przecięcie zbioru istotnych biomarkerów pomiędzy nowotworami,
* Należy zbadać jak wygląda zróżnicowanie pomiędzy nowotworami bazując na określonej sygnaturze,
* Należy przygotować aplikację shiny pozwalającą na przedstawienie jak przeżycia zależą od wybranego bio-markera dla określonego rodzaju nowotworu,
* Należy przygotować krótki raport opisujące wykonane obliczenia oraz opracowaną aplikację. 

Każdy zespół wybiera sobie nazwę i tworzy katalog w https://github.com/pbiecek/WarsztatyBadawcze. W tym katalogu powinny znajdować się przynajmniej trzy podkatalogi `shiny` (z aplikacją shiny), `report` (z raportem w knitrze), `biomarkers` (z listą biomarkerów uznanych za ważne). Poza nimi można dodawać dowolnie dużo innych plików i katalogów, ale nie należy wgrywać zbyt dużych plików by nie utrudnić pracy innym. 

## Plan spotkań i ocena

1. [9 XII] Spotkanie wprowadzające 
2. [16 XII] ??? (ustalić godzinę!) Prezentacja charakterystyki danych z wybranej platformy oraz zidentyfikowanych biomarkerów.
3. [21 XII]
4. [13 I]
5. [20 I]
6.

Aby zwiększyć szansę na indywidualną pracę nad raportem, każde ze spotkań składać się będzie z 2h wspólnego bloku, oraz pozostałego czasu w którym z każdym zespołem porozmawiam o jego obecnym podejściu (średnio 20 min na zespół).


Ocena będzie składała się z trzech części:

- (językowa) ocena raportu, pod kątem kompletności, klarowności i czytelności,
- (merytoryczna) ocena listy znalezionych biomarkerów, pod kątem ich sensowności,
- (inżynierska) jakość aplikacji, wykorzystanego zaplecza statystycznego i narzędzie do prezentacji wyników (np. ggplot2, krzywe przeżycia).


## Zbiory danych RTCGA

* Zbiory danych są dostępne na stornie https://github.com/RTCGA
* Opis badania https://tcga-data.nci.nih.gov/tcga/


* Analiza danych o metylacji w R https://www.bioconductor.org/packages/3.3/bioc/vignettes/methyAnalysis/inst/doc/methyAnalysis.pdf
* Analiza danych o CNV w R http://www.biomedcentral.com/1755-8794/4/47 https://www.bioconductor.org/packages/3.3/bioc/vignettes/CNVtools/inst/doc/CNVtools-vignette.pdf
* Analiza danych o ekspresji białek http://www.biotechniques.com/BiotechniquesJournal/2014/September/RPPanalyzer-Toolbox-An-improved-R-package-for-analysis-of-reverse-phase-protein-array-data/biotechniques-353895.html https://cran.r-project.org/web/packages/RPPanalyzer/vignettes/RPPanalyzer.pdf
* Analiza danych o mutacji (wyłącznie do inspiracji) https://www.bioconductor.org/packages/release/bioc/html/CancerMutationAnalysis.html
* Analiza danych o ekspresji http://www.bioconductor.org/help/workflows/rnaseqGene/ https://www.bioconductor.org/packages/3.3/bioc/vignettes/DESeq2/inst/doc/DESeq2.pdf



