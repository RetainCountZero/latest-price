▀▄▀▄▀▄██▓▒░
  ╭────────────╮
  │ Einleitung │
  ╰────────────╯

Anleitung zur Einrichtung des Konverters für Dateien vom Typ:

aepreise_13.10.2015_18.10.2015.csv

Diese Dateien enthalten Ausgleichenergiepreise.  In jeder Datei sind
pro Gastag mehrfache Einträge vorhanden.  Zu jedem Zeitpunkt einer
Preisfestlegung ist ein Preis für den Gastag vorhanden.

In BelVis soll jedoch nur der aktuellste Preis importiert werden.
Alle zeitlich vorher liegenden Preisfeststellungen des Gastags werden
nicht benötigt.

Da der BelVis Importer keine flexiblen Filter- und Sortiermethoden
realisiert, wird diese Aufgabe in einem Pre-Processing Schritt durch
den Konverter "LATEST-PRICE"/"LATEST-PRICE.exe" erledigt.



▀▄▀▄▀▄██▓▒░
  ╭─────────────────╮
  │ Voraussetzungen │
  ╰─────────────────╯

1. KiDSM
   Die Umsetzung wurde mit KiDSM 5.160.1 getestet.  Diese Version oder
   neuer sollte ausreichend sein.
2. BelVis PFMGAS
   Die Umsetzung wurde mit BelVis PFMGAS 3.21.160 getestet.  Diese
   Version oder neuer sollte ausreichend sein.
3. Konverter-Dateien
   Die Konverter Dateien sind in dem gleichen Zip zu finden, wie
   dieses Readme.  Wenn du diesen Text liest, hast du wahrscheinlich
   auch die Konverter Dateien.
4. CCL (Optional für Neubau des Konverter-Binary)
   CCL steht für Clozure Common Lisp.  Der Download ist unter
   http://ccl.clozure.com
   zu finden.  Getestet wurde mit CCL-1.10.



▀▄▀▄▀▄██▓▒░
  ╭─────────────╮
  │ Einrichtung │
  ╰─────────────╯

Es wird angenommen, das KiDSM erfolgreich installiert wurde.


Konverter Dateien
=================
Die Datei latest-price.exe ist in einen Ordner zu kopieren, auf den
KiDSM zugreifen kann.  Es bietet sich an, diese Datei außerhalb des
KiDSM Ordners abzulegen, damit bei einem Update des KiDSM die Datei
und deren Dateipfad unverändert bleibt.  Beispielhaft könnte die Datei
in einem Ordner D:\KISTERS\Converter\ abgelegt werden.


Arbeitsordner
=============
Die Arbeitsweise des Converters basiert auf verschiedenen Ordnern.
KiDSM überwacht einen Eingangsordner auf neue Dateien.  Falls dort
eine neue Datei ist, so wird versucht, die Datei zu konvertieren.
Die konvertierte Datei wird entweder in einen Ausgangsorder kopiert
oder bei Fehlern in einen Junk Ordner.  Zusätzlich verschiebt KiDSM
die Originaldatei in einen Ordner für Originaldateien.

  \some-folder\AEP-In    <-- Eingangsordner
  \some-folder\AEP-Out   <-- erfolgreich konvertierte Dateien
  \some-folder\AEP-Junk  <-- nicht verarbeitbare Dateien
  \some-folder\AEP-Orig  <-- Originaldateien


Steuerdatei für konfigurierbaren Import
=======================================
Die BelVis Steuerdatei PFMGAS-AEP-Rev-1.0.ctrl ist in den Ordner für
BelVis3 Steuerdateien zu kopieren.



▀▄▀▄▀▄██▓▒░
  ╭─────────────────────╮
  │ Konfiguration KiDSM │
  ╰─────────────────────╯

Es liegen vier Screenshots bei, welche eine beispielhafte
Konfiguration des KiDSM zeigen (KiDSM-Config-?-4.png).

Datenquelle
===========
Es ist eine Datenquelle einzurichten, aus der die Originaldatei
eingelesen wird.  Als Name des Datenorts wird AEP-In vorgeschlagen.
Die Datenquelle muss als Pfad dann auf den Ordner zeigen, in den die
zu konvertierende Datei abgelegt wird.


Datenziel
=========
Weiterhin ist ein Datenort für die Originaldateien anzulegen.  Dieser
Datenort ist ein Zielordner im KiDSM.  In diesen Ordner werden die
Originaldateien (unkonvertiert) nach Bearbeitung verschoben.  Als Name
des Datenorts bietet sich AEP-Orig an.  Der Pfad sollte auf der
gleichen Ebene liegen, wie der Pfad des Datenortes AEP-In.  Bitte im
Zweifelsfalle die Screenshots zu Rate ziehen.


Konverter
=========
Es ist ein neuer Konverter anzulegen.  Dieser bekommt den Namen
"AEP-Rhenag" und ist von Typ "Kommandozeilen-Konverter".

Im Konverter ist bei den "Speziellen Eigenschaften" Folgendes zu
verwenden:

  Befehl:       d:/KISTERS/Converter/latest-price.exe
  Ausführen in:
  Parameter:    ${file}
  Timeout:      600


Job
===
An Knoten "Datentransfer" ist schlussendlich ein neuer Job
einzurichten, der Datenquelle, Datenziel und Konverter zum Einsatz
bringt. 

Der Job erhält den Namen AEP-Rhenag.

Bei den Datenorten wird unter "Übertragen von" als Quelle der Datenort
"AEP-In" referenziert und der Konverter "AEP-Rhenag" ausgewählt.

In der Auswahlbox "Übertragen nach" wird der Datenort "AEP-Orig"
eingetragen.

Bei den Zusätzlichen Eigenschaften ist Folgendes zu verwenden:

  Periodizität (Sekunden):                 60
  Priorität:                               unverändert belassen
  Max. Wiederholungen:                     2
  Anzahl der Übertragungen/Job-Ausführung: 1000


Übertragung in den Importordner
===============================
Die konvertierte CSV-Datei wird im Ordner AEP-Out abgelegt.  Im
Regelfall ist das nicht der BelVis Importordner.  Daher ist ein
zweiter Job im KiDSM anzulegen, welcher die Übertragung vom AEP-Out
Ordner in den spezifischen Importordner für den BelVis Mandanten
vornimmt. 


▀▄▀▄▀▄██▓▒░
  ╭──────────────────────╮
  │ Konfiguration BelVis │
  ╰──────────────────────╯

In BelVis ist die Steuerdatei "PFMGAS-AEP-Rev-1.0.ctrl" einzurichten.
Die Option Zeitreihen-Metadaten unterbinden sollte eingeschaltet
werden.

Die Zielzeitreihen können im PFMGAS an Stationen des Typs "Preis"
angelegt werden oder im EDMGAS an jeweils einer allg. Station.

Zeitreihe anfügen ..
- Tageswerte
- Mittelwert
- Virtuell: Nein
- Name: VHP-Ind
- Zeitreihenspezifikation: Neu > Name: VHP-Ind
Entstehung: Import, 01.10.2015
Datenaustauschnummer: VHP_IND

Analog für AE-Neg/AE_NEG und AE-Pos/AE_POS vorgehen, um die beiden
weiteren Importzeitreihen anzulegen.



▀▄▀▄▀▄██▓▒░
  ╭─────╮
  │ Fin │
  ╰─────╯

Das war's auch schon.

