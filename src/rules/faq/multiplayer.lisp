;;;; src/rules/faq/multiplayer.lisp

(in-package #:nervous-island.rules)

(define-entry multiplayer.1 ()
  :polish
  "Czy sojusznicy mogą razem dyskutować o wykonywanych akcjach przez jednego z nich? Tak, ale ostateczną decyzję podejmuje gracz, którego jest obecna tura, a podczas Bitwy właściciel jednostki, której działania dotyczy decyzja. W przypadku zeskoperowanego modułu decyduje właściciel Skopera. W przypadku dwóch jednostek różnych graczy, kiedy działanie ich jest wzajemnie alternatywne (np. Medyków wzajemnie się leczących, albo leczących inną jednostkę równocześnie), to decyzję o tym, kto ma ostateczny głos, podejmuje gracz, który wywołał obecną Bitwę albo którego jest obecnie tura."
  :english
  "TODO")

(define-entry multiplayer.2 ()
  :polish
  "Wieloosobowa gra na punkty – Czy w grze wieloosobowej na punkty działają następujące zdolności: Leczenie (Dancer) oraz Wchłanianie (Pożarcie (Death Breath))? Jeśli tak to w jaki sposób? Zmniejsza punkty zdobyte przeciwnikom, gracz wybiera któremu (którym) graczom zmniejszy punkty."
  :english
  "TODO")

(define-entry multiplayer.3 ()
  :polish
  "Wieloosobowa gra na punkty – Jak liczone są punkty w grze wieloosobowej na punkty dla Dancera? Każdy Obiekt osobno czy wspólnie? Wygrywa grę: jak suma punktów dojdzie do 20 czy 30, a może jak jeden z Obiektów uzyska 10? Wspólnie, do 20."
  :english
  "TODO")

(define-entry multiplayer.4 ()
  :polish
  "Wieloosobowa gra na punkty – Czy jeśli jeden z graczy w czasie Bitwy zdobędzie 20 punktów, to gra automatycznie się kończy jego zwycięstwem, czy dogrywamy Bitwę do końca? Dogrywamy do końca."
  :english
  "TODO")

(define-entry multiplayer.5 ()
  :polish
  "Wieloosobowa gra na punkty – Czy można zdobyć ponad 20 punktów? Nie."
  :english
  "TODO")

(define-entry multiplayer.6 ()
  :polish
  "Wieloosobowa gra na punkty – Czy w przypadku remisu 20:20 przeprowadza się kolejną turę graczy w celu jego rozstrzygnięcia? Nie."
  :english
  "TODO")

(define-entry multiplayer.7 ()
  :polish
  "Wieloosobowa gra na punkty a Friendly Fire – Dla kogo liczą się punkty zadane przez własny atak? Należy wybrać jednego z przeciwników, w przypadku zadania więcej niż jednego Obrażenia można je rozdzielić pomiędzy przeciwników."
  :english
  "TODO")

(define-entry multiplayer.8 ()
  :polish
  "Rozpoczęcie gry drużynowej – W instrukcji jest napisane, że Dancer wystawia się jako pierwszy a Vegas ostatni czy zatem nie mogą one być w jednej parze w grze drużynowej, bo wtedy warunek ten nie będzie spełniony, jeśli gracze tej samej drużyny mają się ruszać naprzemiennie? Jest to wyjątek, faza rozstawiania Sztabów nie jest turą gracza, więc najpierw rozstawia się Dancer a jako ostatni Vegas, natomiast tury gracze wykonują naprzemiennie, poczynając od gracza kierującego Dancerem."
  :english
  "TODO")

(define-entry multiplayer.9 ()
  :polish
  "W instrukcji w opisie trybu drużynowego jest napisane, że Sztaby zaczynają z 15 punktami Wytrzymałości, natomiast w opisie Dancera jest informacja, że może leczyć sojuszniczy Sztab do 20. Nie ma też informacji o ewentualnym zmniejszeniu początkowej wytrzymałości Obiektom, co daje im dodatkową przewagę. Jak powinno się grać w trybie drużynowym z Dancerem? W trybie drużynowym nie powinno się zmieniać wytrzymałości Sztabów, w szczególności, gdy bierze w niej udział Dancer."
  :english
  "TODO")

(define-entry multiplayer.10 ()
  :polish
  "Pojedynczy gracz kontra drużyna – W instrukcji w opisie trybu pojedynczy gracz kontra drużyna, jest napisane, że gracze z drużyny zaczynają z 13 punktami Wytrzymałości, jak ma się to do Obiektów? W trybie pojedynczy gracz kontra drużyna, po stronie drużyny nie można grać Dancerem."
  :english
  "TODO")

(define-entry multiplayer.11 ()
  :polish
  "Pojedynczy gracz kontra drużyna – Jak wygląda Sekwencja końcowa, w przypadku jak skończą się żetony pojedynczemu graczowi? Przebiega na normalnych zasadach z tym wyjątkiem, że tylko następny po nim przeciwnik będzie miał okazję wykonać swoją turę."
  :english
  "TODO")

(define-entry multiplayer.12 ()
  :polish
  "Czy można zwiększyć liczbę posiadanych punktów Wytrzymałości Sztabowi albo Obiektowi ponad początkową wartość? Nie."
  :english
  "TODO")

(define-entry multiplayer.13 ()
  :polish
  "Czy po wyeliminowaniu jednego z graczy w grze wieloosobowej z końcem Bitwy, w której zginął, są zdejmowane znaczniki należące do armii tego gracza? Pozostają jedynie znaczniki Jadu oraz Paraliżu i Sieci dystansowej, jeśli Sztab został zniszczony nie podczas Bitwy oraz te należące do przejętych Jednostek."
  :english
  "TODO")

(define-entry multiplayer.14 ()
  :polish
  "Sztab Vegas (pole A) przejął sieciarza (pole B) skierowanego na pole C sąsiadujące ze sztabem Vegas. Przeciwnik postawił na polu C sieciarza skierowanego na pole A. (Przy założeniu, że sieciarze są z wrogich frakcji) Czy sztab Vegas będzie zasieciowany? Nie."
  :english
  "TODO")

(define-entry multiplayer.15 ()
  :polish
  "Znaczniki jadu – „W przypadku sojuszu z armią, która posiada własne znaczniki Jadu (np. Neodżungla) pula znaczników Jadu jest wspólna.” a co w przypadku gdy Missisipi lub Neodżungla jest w sojuszu z inną armią, to czy pula znaczników Jadu jest wspólna, a Bojler daje jej bonus? Tak."
  :english
  "TODO")

(define-entry multiplayer.16 ()
  :polish
  "Znaczniki jadu – Jaka jest kolejność decydowania o położeniu znaczników Jadu, jeśli nie wystarczy ich dla wszystkich ataków sojuszników? Gracz, który wywołał Bitwę, decyduje o kolejności, który z sojuszników musi zdecydować jako pierwszy o tym, gdzie położy znaczniki. Gracz może pozostawić sojusznikowi znaczniki, nie wykorzystując wszystkich mu przysługujących, ale nie może zostawić więcej, niż może zużyć jego sojusznik."
  :english
  "TODO")

(define-entry multiplayer.17 ()
  :polish
  "Inkubator + sieć + Agitator + Kwatermistrz (+Transformator Gaussa) – Czy zdolność sieciowania działa na odległość w przypadku ataku dystansowego sztabu? Nie."
  :english
  "TODO")

(define-entry multiplayer.18 ()
  :polish
  "Czy podczas gry drużynowej sojusznik DDM lub Mephisto może w swojej turze również obrócić jedną ze swoich jednostek? Co w przypadku sojuszu z Missisipi i odpychaniem. Nie."
  :english
  "TODO")

(define-entry multiplayer.19 ()
  :polish
  "Czy zasada Oddech śmierci działa również w grze drużynowej (w której bierze udział Missisipi), jeśli tak to w jaki sposób? Tak, ale tylko wtedy gdy w jednej Bitwie zginą wszystkie Sztaby (w przypadku Dancera wystarczy, że zginie jeden z Obiektów)."
  :english
  "TODO")

(define-entry multiplayer.20 ()
  :polish
  "Czy grając w sojuszu z Molochem, Agitatorzy przejmują jednostki Molocha? Nie."
  :english
  "TODO")

(define-entry multiplayer.21 ()
  :polish
  "Czy będąc w sojuszu z Molochem, można skorzystać z Roszady z przeciwnikiem, aby zamienić miejscami naszą jednostkę z jednostką Molocha? Nie."
  :english
  "TODO")

(define-entry multiplayer.22 ()
  :polish
  "Czy w grze drużynowej można używać żetonów: Roszady, Obrotu, Podwójnego ruchu, Podmiany, na jednostki sojusznicze? Nie."
  :english
  "TODO")

(define-entry multiplayer.23 ()
  :polish
  "Czy można Odepchnąć jednostkę przeciwnika od sojuszniczej? Nie."
  :english
  "TODO")

(define-entry multiplayer.24 ()
  :polish
  "Czy można korzystać z cech jednostek sojuszniczych w swojej turze (np. Mobilności, Obrotu, Odepchnięcia, (?), Łańcucha, Skanibalizowania)? Nie, chyba że wprost napisano inaczej."
  :english
  "TODO")

(define-entry multiplayer.25 ()
  :polish
  "Czy Centrum Rozpoznania działa na sojusznicze armie? Tak."
  :english
  "TODO")

(define-entry multiplayer.26 ()
  :polish
  "Czy zeskoperowany moduł działa na sojusznicze armie właściciela Skopera? Tak."
  :english
  "TODO")

(define-entry multiplayer.27 ()
  :polish
  "Czy Podziemia działają na sojusznika? Tak, ale nadal Sztab Sharrash może wykonywać Podziemną Roszadę tylko se swoimi jednostkami."
  :english
  "TODO")

(define-entry multiplayer.28 ()
  :polish
  "Czy moduły Mephista działają na sojusznika? Tak."
  :english
  "TODO")

(define-entry multiplayer.29 ()
  :polish
  "Czy Wszczepy działają na sojusznika? Nie."
  :english
  "TODO")

(define-entry multiplayer.30 ()
  :polish
  "Uwolnienie – Czy można uwolnić się z sieci Dancer (jeśli podlega się sieciowaniu) poprzez użycie Wszczepu? Nie."
  :english
  "TODO")

(define-entry multiplayer.31 ()
  :polish
  "Czy z jednego Kolca można skorzystać więcej niż raz na Bitwę, np. jak Mephisto posiada dwie inicjatywy ataku? Nie."
  :english
  "TODO")

(define-entry multiplayer.32 ()
  :polish
  "Czy Medyk dystansowy działa na jednostki sojusznicze? Tak."
  :english
  "TODO")

(define-entry multiplayer.33 ()
  :polish
  "Czy Vegas może używać zeskoperowanego Szefa przez sojusznika, bo zeskoperowanego przez własny Skoper powinien? Tak."
  :english
  "TODO")

(define-entry multiplayer.34 ()
  :polish
  "Czy Poświęcenie możne zostać zmodyfikowane przez moduły niewzmacniające ataku (np. Kwatermistrz, Śmietnisko, Bojler)? Nie."
  :english
  "TODO")

(define-entry multiplayer.35 ()
  :polish
  "Czy skoro Odbicie działa na Poświęcenie, to czy jeśli do Haka jest podpięty sojuszniczy normalny (nie dystansowy, którego działanie jest opcjonalne) Medyk, to czy musi on zadziałać i pochłonąć odbite obrażenie, czy nie skoro i tak Hak ginie na skutek wykonania Poświęcenia? Tak, musi zadziałać."
  :english
  "TODO")

(define-entry multiplayer.36 ()
  :polish
  "Czy jeśli Egzekutor wykona egzekucję na jednostce zasieciowanej siecią specjalną (np. Sieć dystansowa, Macki) wrogą wobec niego, to czy spada ten znacznik, czy przechodzi na niego? Spada."
  :english
  "TODO")

(define-entry multiplayer.37 ()
  :polish
  "Burza piaskowa – Czy po zagraniu na jednostkę Neodżungli będącą pod wpływem przejęcia Burzy piaskowej zacznie ona z powrotem przewodzić Macierz? Tak."
  :english
  "TODO")

(define-entry multiplayer.38 ()
  :polish
  "Bestie – Czy można grać armią Bestie na powiększonym obszarze gry? Nie."
  :english
  "TODO")

(define-entry multiplayer.39 ()
  :polish
  "Pola ze znacznikami Sąsiedztwa Wodnego a Strzały + Transformator Gaussa – Co się dzieje w przypadku podłączenia do Strzelca Transformatora Gaussa skoro Strzały mogą korzystać z więcej niż jednego znacznika Sąsiedztwa Wodnego? Czy zaatakuje wszystkie wrogie jednostki znajdujące się na Polach Wodnych nieskończenie wiele razy? Atak takiej jednostki wykona jedno okrążenie Pól Wodnych."
  :english
  "TODO")
