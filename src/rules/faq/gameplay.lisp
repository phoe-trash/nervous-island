;;;; src/rules/faq/gameplay.lisp

(in-package #:nervous-island.rules)

(define-entry faq.gameplay.1 (:net)
  :polish
  "Układ żetonów na planszy należy rozpatrywać jako całość, przed ~
   wprowadzeniem zmian (w żetonach czy znacznikach) na planszy należy upewnić ~
   się, czy na pewno one zajdą po uwzględnianiu pozostałych działań, które ~
   oddziałują w danym momencie (np. Sieciarz, który jest sieciowany przez ~
   innego Sieciarza, nie będzie sieciował, chyba, że kieruje on swoją Sieć na ~
   jednostkę umożliwiającą sieciowanie drugiemu Sieciarzowi). Należy ~
   pamiętać, że Bitwa pomimo rozbudowanej struktury jest jednym układem."
  :english
  "TODO")

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
  "TODO")

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
  "TODO")

(define-entry faq.gameplay.4 (:revival :tentacles)
  :polish
  "Momentu Ożywiania nie można traktować jako tury gracza Death Breath i nie ~
   można korzystać wtedy z cech żetonów na planszy, np. Macek."
  :english
  "TODO")

(define-entry faq.gameplay.5 (:battle)
  :polish
  "Kolejność działań zagrywania żetonów oraz wykorzystywania zdolności czy ~
   cech już zagranych żetonów jest następująca: po dobraniu żetonów i ~
   odrzuceniu jednego z nich (jeśli wymagane) do czasu zakończenia tury (w tym ~
   przez wywołanie Bitwy, gracz sam decyduje o kolejności podejmowanych ~
   działań takich jak zagrywanie żetonów czy wykorzystywanie zdolności i cech ~
   już zagranych żetonów (w tym można te kolejności w dowolny sposób mieszać)."
  :english
  "TODO")

(define-entry faq.gameplay.6 (:battle :full-board :boarding)
  :polish
  "Można korzystać ze zdolności (w tym cech) Żetonu planszy wystawionego w ~
   danej turze, chyba, że jego wystawienie wywołuje Bitwę."
  :english
  "TODO")

(define-entry faq.gameplay.7 (:full-board)
  :polish
  "Nie można korzystać ze zdolności jednostki dopełniającej planszę lub innej ~
   po zapełnieniu planszy."
  :english
  "TODO")

(define-entry faq.gameplay.8 (:battle :net :toughness)
  :polish
  "Dopełnienie planszy Sieciarzem, który sieciuje zewnętrzne źródło ~
   dodatkowej Wytrzymałości jakiejś jednostki, pozbawiając ją tym samym ~
   dostatecznej jej ilości do pozostania na planszy, nie wywołuje Bitwy."
  :english
  "TODO")

(define-entry faq.gameplay.9 (:battle :agitator :takeover :toughness)
  :polish
  "Dopełnienie planszy Agitatorem, który przejmie jednostkę ~
   podtrzymywaną przy życiu na planszy przez dodatkową wytrzymałość albo który ~
   przejmie moduł dodatkowej wytrzymałości jakiejś jednostki, którą on ~
   podtrzymywał przy życiu na planszy, nie wywołuje Bitwy."
  :english
  "TODO")

(define-entry faq.gameplay.10 (:battle)
  :polish
  "Bitwa wywołana przez jakiegoś z graczy nie należy do jego tury."
  :english
  "TODO")

(define-entry faq.gameplay.11 (:battle
                               :clown :explosion :medic :rocket-launcher
                               :sharpshooter :quartermaster
                               :gauss-transformer :damage)
  :polish
  "Gracz, który wywołał Bitwę, decyduje o kolejności deklaracji ~
   podczas Bitwy w jednym segmencie inicjatywy: Klaun, Medyk, Wyrzutnia ~
   rakiet, Strzał snajperski, Kwatermistrz, Transformator Gaussa, itp., przy ~
   czym przed deklaracją działania Medyków, powinny zostać zdeklarowane ~
   wszystkie ataki oraz inne działania zadające Obrażenia lub mające na nie ~
   wpływ."
  :english
  "TODO")

(define-entry faq.gameplay.12 (:last-battle :ending-sequence)
  :polish
  "Gracz, któremu skończyły się jako pierwszemu żetony, ~
   wywołuje Ostatnią Bitwę, gdyż rozpoczął Sekwencję Końcową, a Ostatnia Bitwa ~
   jest integralną częścią Sekwencji Końcowej. Panowanie nad liczbą ~
   posiadanych żetonów jest częścią gry."
  :english
  "TODO")

(define-entry faq.gameplay.13 (:battle :overtime-battle)
  :polish
  "Gracz, który jako ostatni miał swoją turę przed Ostatnią ~
   Bitwą, wywołuje Bitwę Dogrywkową (Bitwę, którą się przeprowadza w razie ~
   remisu po Ostatniej Bitwie)."
  :english
  "TODO")

(define-entry faq.gameplay.14 (:battle :overtime-battle)
  :polish
  "W przypadku rozgrywki, w której bierze udział Dancer, również przeprowadza ~
   się rundę dogrywkową w przypadku remisu po Ostatniej Bitwie."
  :english
  "TODO")

(define-entry faq.gameplay.15 (:full-board)
  :polish
  "Zawsze, gdy po rozegraniu Bitwy wywołanej zapełnieniem ~
   planszy, plansza jest dalej zapełniona (na skutek niespadnięcia żadnej ~
   jednostki albo dopełnienia planszy przez któregoś z graczy zaraz po ~
   Bitwie), automatycznie rozgrywana jest kolejna Bitwa (przed jej ~
   rozpoczęciem należy zdjąć wszystkie znaczniki, które powinny spaść na ~
   koniec albo po Bitwie oraz Vegas może obrócić jednostki, które przejęła ~
   podczas właśnie rozpatrzonej Bitwy) – aż do momentu, gdy jakaś jednostka ~
   zostanie zniszczona, w tym wytrzymałość któregoś ze Sztabów (lub Obiektów) ~
   spadnie do zera. Jeśli podczas rozgrywania Bitwy żadna z jednostek nie ~
   otrzymała obrażeń ani nie zaszła zmiana na planszy (w Bitwie lub ~
   bezpośrednio po niej), która mogłaby doprowadzić do przerwania impasu w ~
   skończonej liczbie Bitew, to gra kończy się w tym momencie (należy porównać ~
   poziom wytrzymałości Sztabów (lub Obiektów), by wyłonić zwycięzcę – w razie ~
   remisu nie przeprowadza się dodatkowej tury). Za wywołującego kolejną Bitwę ~
   uważany jest: jeśli nie spadnie żadna jednostka ten z graczy, który wywołał ~
   poprzednią Bitwę albo jeśli plansza została ponownie dopełniona po Bitwie, ~
   to ten z graczy, który tego dokonał."
  :english
  "TODO")

(define-entry faq.gameplay.16 (:full-board)
  :polish
  "Jeśli po Bitwie z zapełnienia giną tylko jednostki przeciwnika Death ~
   Breath, a na ich miejsce zostaną ożywione nowe, zapełniając plansze ~
   ponownie, rozgrywana jest kolejna Bitwa, zgodnie z zasadą Zapełnionej ~
   planszy, aż do momentu zwolnienia co najmniej jednego pola planszy, a w ~
   przypadku braku zadanych obrażeń w któreś z Bitew gra się kończy zaraz po ~
   niej."
  :english
  "TODO")

(define-entry faq.gameplay.17 (:full-board)
  :polish
  "Gracz, który kieruje Death Breath, wywołuje kolejną Bitwę, którą ~
   spowodowało dostawienie po Bitwie jednostek Death Breath powodujące ~
   zapełnienie planszy."
  :english
  "TODO")

(define-entry faq.gameplay.18 (:last-battle :full-board)
  :polish
  "Gdy po tym, jak jeden z graczy dobierze ostatni żeton i wykona swoją turę, ~
   a następny gracz zapełni planszę i wywoła Bitwę, to zostaną rozegrane dwie ~
   Bitwy (z zapełnienia planszy oraz Ostatnia bitwa)."
  :english
  "TODO")

(define-entry faq.gameplay.19 (:last-battle)
  :polish
  "Przed Ostatnią Bitwą można zachować żetony na ręce (maks. dwa), tak żeby je ~
   później wykorzystać w trakcie dodatkowej tury po remisie."
  :english
  "TODO")

(define-entry faq.gameplay.20 (:ending-sequence :bio-droid :return)
  :polish
  "Powracający Bio-droid nie może przerwać rozpoczętej Sekwencji Końcowej."
  :english
  "TODO")

(define-entry faq.gameplay.21 ()
  :polish
  "Stan (sieciowanie, przejęcie itp.) jednostek utrzymuje się przez cały ~
   segment inicjatywy, a zdejmowanie żetonów odbywa się na zakończenie danego ~
   segmentu, a nie po nim."
  :english
  "TODO")

(define-entry faq.gameplay.22 (:hq :damage :battle)
  :polish
  "Zniszczony Sztab nie może wykonywać ataków oraz wspomagać sojuszniczych ~
   jednostek do końca Bitwy, w której został zniszczony. Sztab, jak każdą inną ~
   jednostkę, należy zdjąć na koniec segmentu inicjatywy, w którym został ~
   zniszczony."
  :english
  "TODO")

(define-entry faq.gameplay.23 (:dancer :object :battle)
  :polish
  "Jeśli podczas Bitwy jeden z Obiektów straci wszystkie punkty wytrzymałości, ~
   to pozostałe Obiekty są zdejmowane dopiero na koniec Bitwy."
  :english
  "TODO")
