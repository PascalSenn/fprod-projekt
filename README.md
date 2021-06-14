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
