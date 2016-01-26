# Einleitung

Anleitung zur Einrichtung des Konverters für Dateien vom Typ:

    aepreise_13.10.2015_18.10.2015.csv

Diese Dateien enthalten Ausgleichenergiepreise.  In jeder Datei sind pro Gastag mehrfache Einträge vorhanden.  Zu jedem Zeitpunkt einer Preisfestlegung ist ein Preis für den Gastag vorhanden.  
In BelVis soll jedoch nur der aktuellste Preis importiert werden. Alle zeitlich vorher liegenden Preisfeststellungen des Gastags werden nicht benötigt.

Da der BelVis Importer keine flexiblen Filter- und Sortiermethoden realisiert, wird diese Aufgabe in einem Pre-Processing Schritt durch den Konverter _LATEST-PRICE_/_LATEST-PRICE.exe_ erledigt.



# Voraussetzungen

1. KiDSM

   Die Umsetzung wurde mit KiDSM 5.161.0 getestet.  Diese Version oder neuer sollte ausreichend sein.

2. BelVis PFMGAS
   
   Die Umsetzung wurde mit BelVis PFMGAS 3.21.160 getestet.  Diese Version oder neuer sollte ausreichend sein.

3. Konverter-Datei
   
   Die Konverter Dateien sind in dem gleichen Zip zu finden, wie dieses Readme.

4. CCL (Optional für Neubau des Konverter-Binary)
   
   CCL steht für Clozure Common Lisp.  Der Download ist unter

   http://ccl.clozure.com
   
   zu finden.  Getestet wurde mit CCL-1.11.



# Einrichtung

Es wird angenommen, das KiDSM erfolgreich installiert wurde.


## Konverter Dateien

Die Datei latest-price.exe ist in einen Ordner zu kopieren, auf den KiDSM zugreifen kann.  Es bietet sich an, diese Datei außerhalb des KiDSM Ordners abzulegen, damit bei einem Update des KiDSM die Datei und deren Dateipfad unverändert bleibt.  Beispielhaft könnte die Datei in einem Ordner _C:\KISTERS\Konverter_ abgelegt werden.

> ***ACHTUNG***
> 
> Aufgrund eines Bugs in der Windows Version von CCL beendet sich der Konverter manchmal nicht.  
> Um diesen Bug zu vermeiden ist bei den Eigenschaften des Konverters latest-price.exe die  
> Kompatilität für alle Anwender auf Windows 7 (oder Server 2008) zu stellen.  Dadurch sollte  
> sich der Konverter nach Abschluss der Konvertierung korrekt beenden.  


## Arbeitsordner

Die Arbeitsweise des Konverters basiert auf verschiedenen Ordnern.  KiDSM überwacht einen Eingangsordner auf neue Dateien.  Falls dort eine neue Datei ist, so wird versucht, die Datei zu konvertieren.  Die konvertierte Datei wird entweder in einen Ausgangsorder verschoben oder bei Fehlern wird die nicht verarbeitbare Datei in einen Junk Ordner verschoben.  Nach einer erfolgreichen Konvertierung wird die Originaldatei von KiDSM gelöscht.  Falls diese noch benötigt wird, so ist zuvor eine Kopie davon zu erstellen.

    \some-folder\AEP-In    <-- Eingangsordner
  
    \some-folder\AEP-Out   <-- Ausgangsordner für erfolgreich konvertierte Dateien

    \some-folder\AEP-Junk  <-- nicht verarbeitbare Dateien

Es sind die beiden Ordner anzulegen, welche als Eingangsordner und als Ausgangsordner zu verwenden sind.  KiDSM benötigt Vollzugriff auf diese beiden Ordner.  Oftmals ist der Ausgangsordner identisch mit dem Importordner von BelVis3.  
Der Ordner AEP-Junk wird automatisch auf gleicher Ebene mit dem Eingangsordner angelegt, sobald dieser das erste mal benötigt wird.


## Steuerdatei für konfigurierbaren Import

Die BelVis Steuerdatei _PFMGAS-AEP-Rev-1.0.ctrl_ ist in den Ordner für BelVis3 Steuerdateien zu kopieren.



# Konfiguration KiDSM

Es liegen vier Screenshots bei, welche eine beispielhafte Konfiguration des KiDSM zeigen (KiDSM-x-something.png). 

## Datenquelle

Es ist eine Daten_quelle_ einzurichten, aus der die Originaldatei eingelesen wird.  Als Name des Datenorts wird _AEP-In_ vorgeschlagen.  Die Datenquelle muss als Pfad auf den Ordner zeigen, in den die zu konvertierende Datei abgelegt wird.  Die Datenquelle muss ein lokaler Ordner sein.  Ein UNC-Pfad wird nicht unterstützt.


## Datenziel

Weiterhin ist ein Datenort für die Originaldateien anzulegen.  Dieser Datenort ist ein _Ziel_ordner im KiDSM.  In diesen Ordner werden die konvertierten Dateien nach Bearbeitung verschoben.  Als Name des Datenorts bietet sich _AEP-Out_ an.  Das Datenziel muss ein lokaler Ordner sein.  Ein UNC-Pfad wird nicht unterstützt.


## Konverter
Es ist ein neuer Konverter anzulegen.  Dieser bekommt den Namen _AEP_ und ist von Typ _Kommandozeilen-Konverter_.

Im Konverter ist bei den _Speziellen Eigenschaften_ Folgendes zu verwenden:

    Befehl:                  c:/KISTERS/Konverter/latest-price.exe
    Ausführen in:
    Parameter:               ${file} ${proc_outputDir}
    Timeout:                 600
    Rückgabewert ignorieren: 

## Job

An Knoten _Datentransfer_ ist schlussendlich ein neuer Job einzurichten, der Datenquelle, Datenziel und Konverter zum Einsatz bringt. 

Der Job erhält den Namen _AEP_.

Bei den Datenorten wird unter _Übertragen von_ als Quelle der Datenort _AEP-In_ referenziert und der Konverter _AEP-Konverter_ ausgewählt.

In der Auswahlbox _Übertragen nach_ wird der Datenort _AEP-Out_ eingetragen.

Bei den Zusätzlichen Eigenschaften ist Folgendes zu verwenden:

    Periodizität (Sekunden):                 60
    Priorität:                               unverändert belassen
    Max. Wiederholungen:                     2
    Anzahl der Übertragungen/Job-Ausführung: 1000


## Übertragung in den Importordner

Die konvertierte CSV-Datei wird im Ordner _AEP-Out_ abgelegt.  Das kann z.B. direkt der Importordner von BelVis3 sein.  Falls der Importordner auf einem anderen Rechner liegt, so ist ein weiterer Job einzurichten, der die konvertierte Datei in den Importordner überträgt.



# Konfiguration BelVis

In BelVis ist die Steuerdatei _PFMGAS-AEP-Rev-1.0.ctrl_ einzurichten.  Die Option Zeitreihen-Metadaten unterbinden sollte eingeschaltet werden.

Die Zielzeitreihen können im PFMGAS an Stationen des Typs _Preis_ angelegt werden oder im EDMGAS an jeweils einer _allg. Station_.

Zeitreihe anfügen ..  
- Tageswerte  
- Mittelwert  
- Virtuell: Nein  
- Name: VHP-Ind  
- Zeitreihenspezifikation: Neu > Name: VHP-Ind  
Entstehung: Import, 01.10.2015  
Datenaustauschnummer: VHP_IND  

Analog für _AE-Neg_/_AE\_NEG_ und _AE-Pos_/_AE\_POS_ vorgehen, um die beiden weiteren Importzeitreihen anzulegen.


# Weitere Hinweise

## Fehlermeldungen des Konverters

Falls bei der Konvertierung ein Problem auftritt, so versucht der Konverter sich zu beenden.  Bei bekannten Problemen wird eine Meldung in die Datei _kidsm-transfertools-debug.txt_ ausgegeben.  Eine Meldung in dieser Datei könnte z.B. so aussehen:

    2016-01-26T18:38:16.014Z ERROR [converter.CommandConverter.convertItem] Error Buffer: Latest-Price: File  
    C:/Kisters/BelVisData/KiDSM/AEP-In/Leere Datei.txt has to be of filetype CSV.

    2016-01-26T18:38:16.029Z ERROR [location.LocationBase.transmitLoadedItems] Exception happened while working  
    with a converter for location AEP-In de.kisters.kidsm.auto.AutoException: Converter returned with error code

Der Text "Latest-Price: File C:/Kisters/BelVisData/KiDSM/AEP-In/Leere Datei.txt has to be of filetype CSV." stammt vom Latest-Price Konverter.  Momentan sind folgende Meldungen und Errorcodes hinterlegt:

> 1 - Latest-Price: Called converter with no arguments  
> 2 - Latest-Price: File ~a does not exist.  
> 3 - Latest-Price: File ~a has to be supplied as absolute filename.  
> 4 - Latest-Price: File ~a has to be of filetype CSV.  
> 5 - Latest-Price: Line does not contain 11 columns.  

Wobei ~a jeweils durch den aktuellen Dateinamen oder Zeileninhalt ersetzt werden, soweit dies sinnvoll ist.  
Bei der Nutzung von KiDSM ist eher mit den Fehlern der Nummer 4 (Eingangsdatei hat nicht die Endung CSV) oder Nummer 5 (eine Zeile der Importdatei hat nicht genau 11 Spalten) zu rechnen.  Die anderen Fehlercodes sollten bei korrekter Einrichtung im KiDSM nicht auftreten.

## Probleme beheben

Im Regelfall braucht der Konverter für eine Datei nur wenige Sekunden.  Falls der KiDSM für längere Zeit bei der Konvertierung verweilt, dann könnte es ein nicht abgefangenes Problem mit der Importdatei geben.  In diesem Falle empfiehlt es sich, den Prozess _latest-price.exe_ im Windows Taskmanager zu beenden.