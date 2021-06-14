# Kuery

## Installation

Um das Projekt zu builden muss einmalig 'cabal build' ausgeführt werden.
Die Applikation kann nachher mit 'cabal run' gestartet werden.
Per default versucht die Applikation eine Verbindung zur Datenbank "kuery" auf 127.0.0.1 her zu stellen (Port 3306 für MySql und Port 27017 für MongoDB).
Der Datenbankname und Host können im Main.hs geändert werden.

## Demo Applikation

Die Demo Applikation ist eine Konsolen Applikation die eine Liste von User aus der Datenbank ausliest und anzeigt.
Zu Beginn muss ausgewählt werden ob MongoDB oder MySQL verwendet werden soll und ob die Monad oder Function Syntax verwendet werden soll.

![image](https://user-images.githubusercontent.com/49570944/121898330-8f7fd100-cd23-11eb-8944-c48bc12cf882.png)

Nachher kann durch die Liste von User navigiert werden. Dazu gibt es folgende Inputs:
- (n)ext Zeigt die nächste Seite an
- (p)revious Zeigt die vorherige Seite an
- (m)ore Vergrössert die Seite um 10 User
- (l)ess Verkleinert die Seite um 10 User

![image](https://user-images.githubusercontent.com/49570944/121898234-7a0aa700-cd23-11eb-8343-9e66b5544a36.png)

## Ordnerstruktur

- example
- example/Main.hs                     Einstiegspunkt in die Demo Applikation (und DB Konfiguration)
- example/Example/Function.hs         Demo Applikation mit Infix Functions
- example/Example/Monad.hs            Demo Applikation mit Monad Syntax
- lib                             
- lib/Kuery
- lib/Kuery/Language                  In diesem Ordner sind die Typen die von aussen verwendet werden. Sozusagen die API des Query Builder
- lib/Kuery/Language/Base.hs          Grundlegende Typen für CRUD Statements
- lib/Kuery/Language/Operators.hs     Convenience Operatoren die in den Statements verwendet werden können
- lib/Kuery/Language/Result.hs        Rückgabetyp eines Statements
- lib/Kuery/Language/Value.hs         Variablen
- lib/Kuery/Monad              
- lib/Kuery/Monad/Operations.hs              
- lib/Kuery/Providers  
- lib/Kuery/Providers/Mongo              
- lib/Kuery/Providers/Mongo/Base.hs   Anbindung an Mongo Datenbank
- lib/Kuery/Providers/MySql              
- lib/Kuery/Providers/MySql/Base.hs   Anbindung an MySQL Datenbank        
- lib/Kuery/Providers/Mongo.hs        Implementierung für MongoDB     
- lib/Kuery/Providers/MySql.hs        Implementierung für MySQL 
- lib/Kuery/Connection.hs             
- lib/Kuery/Helpers.hs            
- lib/Kuery/Operations.hs       
- lib/Kuery/Result.hs                 Monad für Fehlerbehandlung
- lib/Kuery.hs
- kuery.cabal        
