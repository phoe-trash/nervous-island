;;;; src/rules/faq/gameplay.lisp

(in-package #:nervous-island.rules)

(define-entry faq.gameplay.1 (:net)
  :polish
  "Układ żetonów na planszy należy rozpatrywać jako całość. Przed ~
   wprowadzeniem zmian (w żetonach czy znacznikach) na planszy należy upewnić ~
   się, czy na pewno one zajdą po uwzględnianiu pozostałych działań, które ~
   oddziałują w danym momencie (np. Sieciarz, który jest sieciowany przez ~
   innego Sieciarza, nie będzie sieciował, chyba, że kieruje on swoją Sieć na ~
   jednostkę umożliwiającą sieciowanie drugiemu Sieciarzowi). Należy ~
   pamiętać, że Bitwa pomimo rozbudowanej struktury jest jednym układem."
  :english
  "The whole state of the board should be examined as a whole. Before ~
   making any changes (in tiles or tokens) to the board one should assert ~
   that these changes will occur after taking all other actions that affect ~
   the board at the moment (e.g. a Netter which is itself netted by another ~
   Netter will not be netting unless it directs its net at a unit that allows ~
   the other Netter to use its net). It's important to remember that a Battle, ~
   despite its complex structure, is a single state of the board.")

(define-entry faq.gameplay.2 (:mine :execution)
  :polish
  "Kolejność dodawania/odejmowania żetonów do/od układu na planszy nie ma ~
   znaczenia dla rozpatrywania układu żetonów na planszy. Układ żetonów na ~
   planszy zawsze jest rozpatrywany tak samo (poza przypadkami, kiedy gracze ~
   mają do podjęcia jakieś decyzje), niezależnie od kolejności dołączania do ~
   lub odłączania od niego poszczególnych żetonów oraz przyczyny wywołującej ~
   zmiany (zagranie nowego żetonu czy poruszenie już obecnego na planszy). ~
   Należy pamiętać, że część zdolności działa w trakcie dodawania do układu ~
   (np. detonacja Miny czy zdolność Egzekutora), więc ich działanie należy ~
   rozpatrzyć, zanim zaczniemy rozpatrywać układ."
  :english
  "The order of adding/removing tiles to the state of the board does not ~
   matter with regard to examining the state of the board. The state of the ~
   board is always examined the same (except for situations in which the ~
   players have some choices to make) regardless of the order in which ~
   particular tiles were added or removed from it and how exactly the state ~
   was changed (e.g. playing a new tile or moving one that was already present ~
   on it). Remember that some skills work while a tile is added to the state
   (e.g. detonating a Mine or the Executor's skill), so the effects of these ~
   should be considered before the whole state is examined.")

(define-entry faq.gameplay.3 (:hq :lair :mississippi :push-back
                              :steel-police :net-of-steel :vegas :takeover
                              :dancer :object)
  :polish
  "Rozmieszczanie Sztabów nie jest traktowane jako tura gracza i nie można ~
   wtedy używać zdolności specjalnych sztabów (z wyjątkiem cechy Siedlisko). W ~
   szczególności, Missisipi nie może wtedy odpychać Sztabu/Obiektu ~
   przeciwnika, Stalowa Policja zasieciować Obiektu, a Vegas przejąć i obrócić ~
   Obiektu. Vegas przejmuje automatycznie, ewentualny obrót przejętej ~
   jednostki jest wykonywany na koniec rozmieszczania Sztabów."
  :english
  "The placement of HQs is not treated as a player's turn and HQ's abilities ~
   may not be used at that time (with the exception of Lair). In particular, ~
   Mississippi may not push back the enemy HQs/Objects, Steel Police may not ~
   net an Object, and Vegas may not take over or rotate an Object. Note that ~
   Vegas takes over automatically and any rotation of a taken over unit is ~
   performed after the HQs are placed.")

(define-entry faq.gameplay.4 (:revival :tentacles)
  :polish
  "Momentu Ożywiania nie można traktować jako tury gracza Death Breath i nie ~
   można korzystać wtedy z cech żetonów na planszy, np. Macek."
  :english
  "The moment of Revival cannot be treated as the turn of Death Breath's ~
   player and they may not use the skills of units on their board, e.g. ~
   Tentacles, during that time.")

(define-entry faq.gameplay.5 (:battle)
  :polish
  "Kolejność działań zagrywania żetonów oraz wykorzystywania zdolności czy ~
   cech już zagranych żetonów jest następująca: po dobraniu żetonów i ~
   odrzuceniu jednego z nich (jeśli wymagane) do czasu zakończenia tury (w tym ~
   przez wywołanie Bitwy), gracz sam decyduje o kolejności podejmowanych ~
   działań takich jak zagrywanie żetonów czy wykorzystywanie zdolności i cech ~
   już zagranych żetonów (w tym można te kolejności w dowolny sposób mieszać)."
  :english
  "The order of taking actions during a turn, such as playing tiles and ~
   using skills of already played tiles, is as follows: after drawing the ~
   tiles and discarding one of them (if required), until the turn is over ~
   (including via causing a Battle), the player can decide about the order ~
   of actions that they perform, such as playing tiles and using skills of ~
   already present tiles (and the order of these can be mixed in any order).")

(define-entry faq.gameplay.6 (:battle :full-board :boarding)
  :polish
  "Można korzystać ze zdolności (w tym cech) Żetonu planszy wystawionego w ~
   danej turze, chyba, że jego wystawienie wywołuje Bitwę."
  :english
  "It is possible to use the skills (including HQ abilities) of a board tile ~
   placed in a given turn, unless the act of placing it causes a Battle.")

(define-entry faq.gameplay.7 (:full-board)
  :polish
  "Nie można korzystać ze zdolności jednostki dopełniającej planszę lub innej ~
   po zapełnieniu planszy."
  :english
  "When a unit fills the board, it is not allowed to use any of its skills or ~
   any skills of other units present on the board.")

(define-entry faq.gameplay.8 (:battle :net :toughness :full-board)
  :polish
  "Dopełnienie planszy Sieciarzem, który sieciuje zewnętrzne źródło ~
   dodatkowej Wytrzymałości jakiejś jednostki, pozbawiając ją tym samym ~
   dostatecznej jej ilości do pozostania na planszy, nie wywołuje Bitwy."
  :english
  "Filling the board with a Netter, which nets an external source of some ~
   unit's Toughness and therefore deprives it of enough Toughness to stay ~
   on the board, does not cause a Battle.")

(define-entry faq.gameplay.9 (:battle :agitator :takeover :toughness
                              :full-board)
  :polish
  "Dopełnienie planszy Agitatorem, który przejmie jednostkę ~
   podtrzymywaną przy życiu na planszy przez dodatkową wytrzymałość albo który ~
   przejmie moduł dodatkowej wytrzymałości jakiejś jednostki, którą on ~
   podtrzymywał przy życiu na planszy, nie wywołuje Bitwy."
  :english
  "Filling the board with an Agitator, which takes over a unit which is on the ~
   board only due to an external source of Toughness, or which takes over an ~
   external source of some unit's Toughness and therefore deprives it of ~
   enough Toughness to stay on the board, does not cause a Battle.")

(define-entry faq.gameplay.10 (:battle)
  :polish
  "Bitwa wywołana przez jakiegoś z graczy nie należy do jego tury."
  :english
  "A Battle caused by a player is not a part of their turn.")

(define-entry faq.gameplay.11 (:battle
                               :clown :explosion :medic :rocket-launcher
                               :sharpshooter :quartermaster
                               :gauss-transformer :damage)
  :polish
  "Gracz, który wywołał Bitwę, decyduje o kolejności deklaracji ~
   podczas Bitwy w jednym segmencie Inicjatywy: Klaun, Medyk, Wyrzutnia ~
   rakiet, Strzał snajperski, Kwatermistrz, Transformator Gaussa, itp., przy ~
   czym przed deklaracją działania Medyków, powinny zostać zdeklarowane ~
   wszystkie ataki oraz inne działania zadające Obrażenia lub mające na nie ~
   wpływ."
  :english
  "The player who caused the Battle decides about the order of declarations ~
   during the Battle in a single Initiative segment, e.g. Clown, Medic, Rocket ~
   Launcher, Sharshooting, Quartermaster, Gauss Transformer, etc., except ~
   all attacks and other effects dealing Damage or related to dealing Damage ~
   should be declared before any Medic effects.")

(define-entry faq.gameplay.12 (:last-battle :ending-sequence)
  :polish
  "Gracz, któremu skończyły się jako pierwszemu żetony, wywołuje Ostatnią ~
   Bitwę, gdyż rozpoczął Sekwencję Końcową, a Ostatnia Bitwa jest integralną ~
   częścią Sekwencji Końcowej. Panowanie nad liczbą posiadanych żetonów jest ~
   częścią gry."
  :english
  "The player who ran out of tiles the first begins the Last Battle, because ~
   they started the Ending Sequence and the Last Battle is an integral part of ~
   the Ending Sequence. Keeping the number of remaining tiles under control is ~
   a part of the game.")

(define-entry faq.gameplay.13 (:battle :overtime-battle)
  :polish
  "Gracz, który jako ostatni miał swoją turę przed Ostatnią Bitwą, wywołuje ~
   Bitwę Dogrywkową (Bitwę, którą się przeprowadza w razie remisu po Ostatniej ~
   Bitwie)."
  :english
  "The player whose turn was last before the Last Battle causes the Overtime ~
   Battle (a Battle which is played in case of a tie after the Last Battle).")

(define-entry faq.gameplay.14 (:battle :overtime-battle)
  :polish
  "W przypadku rozgrywki, w której bierze udział Dancer, również przeprowadza ~
   się rundę dogrywkową w przypadku remisu po Ostatniej Bitwie."
  :english
  "In case of a game in which Dancer participates, if there is a tie after the ~
   Last Battle, an overtime round is also played.")

(define-entry faq.gameplay.15 (:full-board)
  :polish
  "Zawsze, gdy po rozegraniu Bitwy wywołanej zapełnieniem planszy, plansza ~
   jest dalej zapełniona (na skutek niespadnięcia żadnej jednostki albo ~
   dopełnienia planszy przez któregoś z graczy zaraz po Bitwie), automatycznie ~
   rozgrywana jest kolejna Bitwa. Przed jej rozpoczęciem należy zdjąć ~
   wszystkie znaczniki, które powinny spaść na koniec albo po Bitwie oraz ~
   Vegas może obrócić jednostki, które przejęła podczas właśnie rozpatrzonej ~
   Bitwy. Ten proces jest powtarzany aż do momentu, gdy jakaś jednostka ~
   zostanie zniszczona, w tym wytrzymałość któregoś ze Sztabów (lub Obiektów) ~
   spadnie do zera. Jeśli podczas rozgrywania Bitwy żadna z jednostek nie ~
   otrzymała obrażeń ani nie zaszła zmiana na planszy (w Bitwie lub ~
   bezpośrednio po niej), która mogłaby doprowadzić do przerwania impasu w ~
   skończonej liczbie Bitew, to gra kończy się w tym momencie. Należy porównać ~
   poziom wytrzymałości Sztabów (lub Obiektów), by wyłonić zwycięzcę – w razie ~
   remisu nie przeprowadza się dodatkowej tury. Za wywołującego kolejną Bitwę ~
   uważany jest: jeśli nie spadnie żadna jednostka ten z graczy, który wywołał ~
   poprzednią Bitwę albo jeśli plansza została ponownie dopełniona po Bitwie, ~
   to ten z graczy, który tego dokonał."
  :english
  "Whenever, after finishing a Battle triggered by a full board, the board is ~
   still full (due to no unit leaving the board or the board becoming full ~
   due to some player filling it right after the Battle), another Battle is ~
   automatically started. Before beginning the new Battle, remove all markers ~
   which should be removed at the end of the previous Battle and Vegas may ~
   rotate units that it took over during the previous Battle. This process is ~
   repeated until a unit is destroyed, including a HQ whose hit points drop to ~
   zero. If, during the execution of a Battle, no unit is dealt Damage and ~
   no other change occurred (during the Battle or immediately after it) which ~
   could break the impasse in a finite number of Battles, then the game is ~
   over in that moment. The hit points of HQs (or Objects) should be compared ~
   to decide the winner - in case of a tie, no overtime round is played. The ~
   player triggering the Battle is: if no unit was destroyed, the player who ~
   triggered the previous Battle, and if the board was filled again ~
   immediately after the previous Battle, the player who did this.")

(define-entry faq.gameplay.16 (:full-board :revival)
  :polish
  "Jeśli w trakcie Bitwy z zapełnienia giną tylko jednostki przeciwnika Death ~
   Breath, a po Bitwie na ich miejsce zostaną ożywione nowe, zapełniając ~
   planszę ponownie, rozgrywana jest kolejna Bitwa, zgodnie z zasadą ~
   Zapełnionej planszy, aż do momentu zwolnienia co najmniej jednego pola ~
   planszy, a w przypadku braku zadanych obrażeń w któreś z Bitew gra się ~
   kończy zaraz po niej."
  :english
  "If, during a Battle caused by a full board, the only units to die are ~
   these belonging to Death Breath's opponent, and after that Battle new ~
   units are Revived in their place, then another Battle is played according ~
   to the Full Board rules, until at least one free space appears after a ~
   Battle or until the game ends because of a impasse.")

(define-entry faq.gameplay.17 (:full-board)
  :polish
  "Gracz, który kieruje Death Breath, wywołuje kolejną Bitwę, którą ~
   spowodowało dostawienie po Bitwie jednostek Death Breath powodujące ~
   zapełnienie planszy."
  :english
  "If a full board Battle is caused by adding Death Breath units after a ~
   previous Battle, the Death Breath player is the player causing this ~
   Battle.")

(define-entry faq.gameplay.18 (:last-battle :full-board)
  :polish
  "Gdy po tym, jak jeden z graczy dobierze ostatni żeton i wykona swoją turę, ~
   a następny gracz zapełni planszę i wywoła Bitwę, to zostaną rozegrane dwie ~
   Bitwy (z zapełnienia planszy oraz Ostatnia bitwa)."
  :english
  "After one player draws their last tile and finishes their turn, and the ~
   other player fills the board and causes a Battle, then a total of two ~
   Battles will be played (a Full Board Battle and the Last Battle).")

(define-entry faq.gameplay.19 (:last-battle)
  :polish
  "Przed Ostatnią Bitwą można zachować żetony na ręce (maks. dwa), tak żeby je ~
   później wykorzystać w trakcie dodatkowej tury po remisie."
  :english
  "Before the Last Battle a player can keep tiles on their hand (at most two) ~
   to be able to use them during the overtime round after a tie.")

(define-entry faq.gameplay.20 (:ending-sequence :bio-droid :return)
  :polish
  "Powracający Bio-droid nie może przerwać rozpoczętej Sekwencji Końcowej."
  :english
  "A Returning Bio-droid cannot interrupt a started Ending Sequence.")

(define-entry faq.gameplay.21 ()
  :polish
  "Stan (sieciowanie, przejęcie itp.) jednostek utrzymuje się przez cały ~
   segment inicjatywy. Zdejmowanie żetonów odbywa się na zakończenie danego ~
   segmentu, a nie po nim."
  :english
  "The state (being netted, taken over, etc.) of a unit remains throughout ~
   a whole Initiative segment. Removing tiles happens after a given segment,
   not after it.")

(define-entry faq.gameplay.22 (:hq :damage :battle)
  :polish
  "Zniszczony Sztab nie może wykonywać ataków oraz wspomagać sojuszniczych ~
   jednostek do końca Bitwy, w której został zniszczony. Sztab, jak każdą inną ~
   jednostkę, należy zdjąć na koniec segmentu inicjatywy, w którym został ~
   zniszczony."
  :english
  "A destroyed HQ may not make attacks or support allied units until the end ~
   of the Battle in which it was destroyed. The HQ, just like any other unit, ~
   should be removed at the end of the Initiative segment in which it was ~
   destroyed.")

(define-entry faq.gameplay.23 (:dancer :object :battle)
  :polish
  "Jeśli podczas Bitwy jeden z Obiektów straci wszystkie punkty wytrzymałości, ~
   to pozostałe Obiekty są zdejmowane dopiero na koniec Bitwy."
  :english
  "If during a Battle one of the Objects loses all hit points, all other ~
   Objects should be removed only at the end of the battle.")
