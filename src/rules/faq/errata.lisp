;;;; src/rules/faq/errata.lisp

(in-package #:nervous-island.rules)

(define-entry faq.errata.1 (:hq :initiative)
  :polish
  "Zniszczony Sztab jest zdejmowany na koniec segmentu inicjatywy, w której ~
   został zniszczony."
  :english
  "A destroyed HQ is removed from the board at the end of the initiative ~
   segment in which it was destroyed.")

(define-entry faq.errata.2 (:ending-sequence :battle)
  :polish
  "Jak któremukolwiek z graczy skończą się żetony do dobierania, powinno być: ~
   \"Nie można wywoływać Bitwy w inny sposób niż poprzez zapełnienie planszy\" ~
   zamiast: \"Nie można zagrywać żetonów Bitwy\"."
  :english
  "When any player runs out of tiles, the rules should say: \"It is not ~
   possible to cause a Battle to occur in any way other than filling up the ~
   board\" rather than \"no more Battle tiles can be played\".")

(define-entry faq.errata.3 (:battle)
  :polish
  "Bitwa nie należy do tury gracza, który ją wywołał."
  :english
  "Battle is not a part of the turn of the player who caused it.")

(define-entry faq.errata.4 (:multiplayer :hq)
  :polish
  "W trybie drużynowym nie należy zmieniać wytrzymałości Sztabów. Zasada ~
   zmniejszająca wytrzymałość do 15 zostaje usunięta."
  :english
  "In team mode, HQ hit points should not be changed. The rule lowering the ~
   hit points to 15 is removed.")

(define-entry faq.errata.5 (:multiplayer :dancer)
  :polish
  "W trybie pojedynczy gracz kontra drużyna, Dancer nie może grać w drużynie."
  :english
  "In the one-player-versus-team mode, Dancer cannot be a part of the team.")

(define-entry faq.errata.6 (:module :transport)
  :polish
  "Po zdaniu: \"Jeśli kilka Modułów działa jednocześnie na jedną jednostkę, ~
   wszystkie te działania kumulują się\" powinno znaleźć się uściślenie: \"Nie ~
   dotyczy to modułów Transportu, których działania nie sumują (kumulują) się ~
   i każdy z nich działa niezależnie\"."
  :english
  "The sentence \"If several modules affect one unit simultaneously, all of ~
   these effects cumulate\" should be further clarified: \"This does not cover ~
   Transport modules, whose effects do not sum (cumulate) and each of which ~
   works independently\".")

(define-entry faq.errata.7 (:death-breath :wound)
  :polish
  "Zdanie: \"Zawsze, gdy jednostka Death Breath zabije jednostkę przeciwnika, ~
   na pustym polu po zabitej jednostce należy położyć znacznik Rany\" zostaje ~
   zamieniona na \"Gdy jednostka Death Breath zada Obrażenie w segmencie ~
   Inicjatywy, w której zostaje zniszczona atakowana przez nią jednostka, na ~
   pustym polu po zniszczonej jednostce należy położyć znacznik Rany\"."
  :english
  "The sentence \"Whenever a Death Breath unit kills an enemy unit, a wound ~
   token should be placed on the empty space that the unit used to occupy\" ~
   is replaced with \"Whenever a Death Breath unit deals any damage in an ~
   initiative segment in which a unit attacked by it is destroyed, a wound ~
   token should be placed on the empty space that the unit used to occupy\".")

(define-entry faq.errata.8 (:scoper)
  :polish
  "Opis Skopera powinien brzmieć: \"Działa na wrogie Moduły. Dopóki wrogi ~
   Moduł jest podłączony do Skopera, tak długo Moduł ten wspomaga swoimi ~
   pozytywnymi cechami jednostki właściciela Skopera oraz jego sojuszników ~
   zamiast własnych, a negatywnymi oddziałuje na jego przeciwników zamiast na ~
   niego. Własność ani zasięg działania zeskoperowanego Modułu nie ulegają ~
   zmianie\"."
  :english
  "The Scoper description should say: \"Affects enemy Modules. As long as an ~
   enemy Module is connected to the Scoper, the enemy Module instead gives its ~
   positive effects to the units of Scoper's owner and their allies, and its ~
   negative effects instead to the Scoper's owner's enemies. The effect or ~
   range of the scoped module is unchanged.\"")

(define-entry faq.errata.9 (:additional-initiative :initiative)
  :polish
  "Opis Dodatkowej inicjatywy (np. Sztab Posterunku, Matka, Główny procesor ~
   bojowy, Sierżant, Podwajacz, Wódz) powinien brzmieć: \"Dodaje własnym i ~
   sojuszniczym jednostkom dodatkową Inicjatywę, o wartości o jeden niższej od ~
   obecnie najwyższej Inicjatywy danej jednostki, po której następuje wartość ~
   Inicjatywy, której nie posiada dana jednostka, jeśli taka istnieje\"."
  :english
  "The description of Additional Initiative (e.g. Outpost HQ, Mother, Main War ~
   Processor, Sergeant, Doubler, Chief) should be: \"Gives friendly units an ~
   additional Initiative with the value of one less than the currently highest ~
   Initiative of the unit that does not have an Initiative immediately ~
   following it in value, if it exists\".")

(define-entry faq.errata.10 (:medic :damage :venom)
  :polish
  "Opis Medyka powinien brzmieć: \"Jeśli własna bądź sojusznicza, będąca w ~
   zasięgu, jednostka otrzymuje obrażenia od innego żetonu (planszy czy ~
   natychmiastowego) albo znacznika (np. Jadu), wtedy ginie Medyk, chyba że w ~
   opisie dawcy albo biorcy Obrażeń napisano inaczej, a tamte rany wraz z ~
   dodatkowymi cechami są anulowane\"."
  :english
  "The description of Medic should be: \"If an own or allied unit being in ~
   range receives Damage from another tile (board tile or instant tile) or ~
   a token (e.g. Venom), then, unless the description of the source or ~
   recipient of the Damage says otherwise, the Medic dies instead and that ~
   Damage, along with any additional effects it may cause, is ignored\".")

(define-entry faq.errata.11 (:poisoning :venom :quartermaster)
  ;; TODO Where is this passage from? Can't find it in Mississippi's manuals.
  :polish
  "W opisie działania Zatrucia, następujący fragment jest zbędny: \"(chyba że ~
   atak zostanie zamieniony na dystansowy, np. przez odpowiedni moduł ~
   sojusznika, taki jak Kwatermistrz Hegemonii)\"."
  :english
  "In the description of Poisoning, the following passage is unnecessary: ~
  \"unless the attack is turned into a ranged one, e.g. via an appropriate ~
  allied module, such as Hegemony's Quartermaster\".")

(define-entry faq.errata.12 (:cannibalism :takeover :satiety)
  :polish
  "Opis Skanibalizowania przez jednostki pod wpływem Przejęcia kontroli ~
   powinien brzmieć: \"Jeżeli następuje Przejęcie kontroli nad jednostką ze ~
   znacznikiem Sytości, to ona zachowuje go. Jednostka z cechą Kanibalizmu, ~
   która została przejęta może Skanibalizować inne jednostki armii, w której ~
   posiadaniu jest obecnie niezależnie od ich pierwotnej przynależności, ~
   wykorzystuje przy tym znacznik Sytości armii, do której pierwotnie należała ~
   jednostka z cechą Kanibalizmu, z której skorzystano. Dalej obowiązuje limit ~
   jednego Skanibalizowania na turę\"."
  :english
  "The description of Cannibalism performed by units under Takeover should be: ~
   \"If a unit with a Satiety token comes under the influence of Takeover, the ~
   Satiety marker is retained. A unit with Cannibalism skill that has been ~
   taken over can Cannibalize other units of the army it currently belongs to ~
   regardless of the army they originally belonged to, but it uses the Satiety ~
   markers of the army that the unit with Cannibalism originally belonged to. ~
   The limit of one use of Cannibalism per turn is still in effect\".")

(define-entry faq.errata.13
    (:flying :melee :friendly-fire-melee :net :friendly-fire-net)
  :polish
  "Opis Latania powinien brzmieć: \"Jednostka ta nie jest podatna na Ciosy (w ~
   tym Friendly Fire), chyba że jest zasieciowana\"."
  :english
  "The description of Flying should be: \"The unit is not affected by Melee ~
   attacks (including Friendly Fire Melee), unless it is netted\".")
