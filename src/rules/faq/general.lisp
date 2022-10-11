;;;; src/rules/faq/general.lisp

(in-package #:nervous-island.rules)

(define-entry faq.general.1 ()
  :polish
  "Inicjatywa – Czy liczba symboli inicjatywy na jednostkach oznacza liczbę ataków danej jednostki, czy tylko inicjatywy, w których może wykonać atak (jeśli jej nie straci)? Tylko inicjatywy, w których może atakować."
  :english
  "TODO")

(define-entry faq.general.2 ()
  :polish
  "Inicjatywa – Czy jeśli jednostka w 3 inicjatywie, w której posiada atak była zasieciowana i z końcem tej inicjatywy albo później (zależnie od rodzaju modułu) została z niej uwolniona oraz równocześnie albo później (zależnie od rodzaju modułu) jej inicjatywa zostanie obniżona przez Dywersanta albo Strefę, to czy wykona swój atak w obniżonej inicjatywie? Nie."
  :english
  "TODO")

(define-entry faq.general.3 ()
  :polish
  "Inicjatywa – Strefa – Czy jednostka podłączona do Strefy posiadająca sama z siebie więcej niż jedną inicjatywę wykona tylko jeden atak w zerze? Czy, skoro nie można obniżyć inicjatywy ataku poniżej zera, to najniższa inicjatywa spada do zera (pozostałe ataki są odpowiednio wyższych inicjatywach (co z jednostkami, które nie mają kolejnych inicjatyw, tylko np. 2 i 0)), a może wykonuje kilka ataków w zerze? Tak, wykona tylko jeden atak w zerze."
  :english
  "TODO")

(define-entry faq.general.4 ()
  :polish
  "Inicjatywa – Strefa + Szarża – Czy jeśli jednostka z cechą Szarża i co najmniej dwiema własnymi inicjatywami ataku, po udanym ataku wykona szarżę i znajdzie się w zasięgu Strefy, to czy wykona kolejny atak w zerowej inicjatywie (jeśli jakiś jej pozostał), czy skoro już co najmniej raz zaatakowała, to takowy traci? Wykona."
  :english
  "TODO")

(define-entry faq.general.5 ()
  :polish
  "Inicjatywa – Jeśli mamy jednostkę o inicjatywach 1 i 0, której po wykonaniu ataku w 1 obniżono inicjatywę o 1, to wykona ona atak w 0? Tak."
  :english
  "TODO")

(define-entry faq.general.6 ()
  :polish
  "Inicjatywa – Czy jednostka posiadająca dwa swoje ataki w 1 i 0 (Sędzia, Plaga) traci drugi atak po przyłączeniu do niego wrogiego modułu dającego -1 do inicjatywy? Tak, wykona tylko jeden atak."
  :english
  "TODO")

(define-entry faq.general.7 ()
  :polish
  "Inicjatywa – Dodatkowa inicjatywa – Jeśli jednostka posiadająca inicjatywy 3 i 2 oraz podłączona jest do modułu Matki, po wykonaniu ataków w 3 i 2 inicjatywie znajdzie się pod wpływem Strefy, ale przed wykonaniem ataku w 1, to wykona atak w 0? Tak."
  :english
  "TODO")

(define-entry faq.general.8 ()
  :polish
  "Inicjatywa – Dodatkowa inicjatywa – Czy dodatkowy atak (Sztab Posterunku, Matka itp.) następuje w kolejnej inicjatywie po zaistnieniu pierwotnego ataku, czy o jeden niższej od obecnego poziomu inicjatywy tego ataku? O jeden niższej od obecnego. Przy czym nie może dać więcej niż jednej dodatkowej inicjatywy ataku na Bitwę."
  :english
  "TODO")

(define-entry faq.general.9 ()
  :polish
  "Inicjatywa – Matka – Czy dwie Matki podpięte do jednej jednostki dodają dwie kolejne inicjatywy, czy tylko jedną? Dwie, jeśli jest taka możliwość. W razie utraty jednej z nich traktuje się, że została utracona, ta co działa później."
  :english
  "TODO")

(define-entry faq.general.10 ()
  :polish
  "Inicjatywa – Matka (Sierżant, Sztab Posterunku itp.) – Co z jednostkami posiadającymi swoje ataki nie w kolejnych inicjatywach, tylko np. 2 i 0 czy 3 i 1, czy wtedy dodatkową inicjatywę otrzymuje po swoich inicjatywach, czy też pomiędzy nimi? Co, jeśli do takiej jednostki podłączone są dwa moduły dodające kolejną inicjatywę? Otrzymuje w pierwszej wolnej inicjatywie niższej od swojej najwyższej inicjatywy, drugi otrzymuje w kolejnej wolnej. W przypadku utraty jednego z bonusów traktuje się, że został stracony, ten co działa później."
  :english
  "TODO")

(define-entry faq.general.11 ()
  :polish
  "Nieskończone combo DDM – Czy jeśli wpada atak o sile 1, to w wyniku niego powstaje nieskończenie wiele ataków o sile 1 czy jeden atak o sile nieskończonej? Nieskończenie wiele ataków o sile 1, Pancerz chroni przed każdym z nich."
  :english
  "TODO")

(define-entry faq.general.12 ()
  :polish
  "Czy jednostka może wykonać więcej niż jeden atak z jednej ścianki w jednym segmencie inicjatywy? Skoro nieskończone combo DDM tak działa, a Roztrajacz i jednostki przekierowujące nie wykonują ataku – nie można ich sparaliżować. Nie."
  :english
  "TODO")

(define-entry faq.general.13 ()
  :polish
  "Więcej niż jeden atak w jednej inicjatywie z jednej ścianki a Medyk – czy Medyk może wchłonąć wszystkie ataki, czy tylko jeden z nich? Jest to wiele ataków, a pochłania tylko jeden, ale pochodzą one z jednej ścianki w jednej inicjatywie. Tylko jeden z nich."
  :english
  "TODO")

(define-entry faq.general.14 ()
  :polish
  "Czy atak jednostki posiadającej zarówno ikonę Strzału, jak i Ciosu na jednej ściance (np. Uniwersalnego żołnierza) jest traktowany jako dwa oddzielne ataki czy jako jeden o dwóch składowych? Jako jeden o dwóch składowych."
  :english
  "TODO")

(define-entry faq.general.15 ()
  :polish
  "Czy Wytrzymałość Sztabu może wykraczać poza zakres 0-20, a Obiektu 0-10? Nie."
  :english
  "TODO")

(define-entry faq.general.16 ()
  :polish
  "Czy śmierć jednego z Obiektów jest równoznaczna z wynikiem 0 dla Dancera? Tak."
  :english
  "TODO")

(define-entry faq.general.17 ()
  :polish
  "Czy jeśli w pojedynku Missisipi kontra Dancer w jednej Bitwie zginą jeden albo dwa z Obiektów oraz Sztab Missisipi, to czy taki remis jest traktowany jako 0:0 i Missisipi go wygrywa? Tak."
  :english
  "TODO")

(define-entry faq.general.18 ()
  :polish
  "Ruch – Czy można wykonać Obrót a dopiero po nim wykonać Ruch? Zagrywając żeton Ruchu albo korzystając z Mobilności, czy modułu Ruchu wykonujemy: Ruch, Obrót albo jednoczesny Ruch z Obrotem. Nie można obrócić np. Agitatora i przejąć nim i obrócić jednostkę a dopiero potem wykonać Ruch."
  :english
  "TODO")

(define-entry faq.general.19 ()
  :polish
  "Ruch, Obrót – Czy Obrót jest skokowy, czy ciągły? Np. czy można obrócić sieciarza, którego sieć jest skierowana na inną sieć i się znoszą o więcej niż jedną ściankę, czy od razu po minięciu się sieci zostanie zasieciowana. Skokowo, więc można dowolnie obrócić."
  :english
  "TODO")

(define-entry faq.general.20 ()
  :polish
  "Czy jednostka utrzymywana przy życiu przez Moduł (lub Sztab Nowego Jorku) dający wytrzymałość podczas ruchu (Ruch, Odepchnięcie, Roszada) ginie w trakcie ruchu czy dopiero jak przejdzie na nowe pole i zdąży np. zdetonować Minę? Ginie po wejściu na pole, zdąży zdetonować Minę."
  :english
  "TODO")

(define-entry faq.general.21 ()
  :polish
  "Czy można dokonać zamiany miejsc dwóch jednostek (np. Roszada, Roszada z przeciwnikiem, Zamiana), jeśli jedna z nich na skutek takiej zamiany zostanie zniszczona? Tak."
  :english
  "TODO")

(define-entry faq.general.22 ()
  :polish
  "Taniec – Czy Dancer może wykonać Taniec, jeśli na początku swojej tury nie posiada żadnego żetonu do odrzucenia? Może to nastąpić w turze pomiędzy Ostatnią Bitwą a Bitwą Dogrywkową. Tak."
  :english
  "TODO")

(define-entry faq.general.23 ()
  :polish
  "Taniec – Czy przeprowadzane działania na Obiektach podczas Tańca są uważane za równoczesne, czy jednak można je wykonywać po kolei, wykorzystując zaistniałe zmiany? Są równoczesne. Zaistniałe zmiany należy wprowadzić dopiero po przeprowadzaniu wszystkich działań wynikających z Tańca."
  :english
  "TODO")

(define-entry faq.general.24 ()
  :polish
  "Czy można żetonem natychmiastowym Paraliż (Sharrash), sparaliżować żeton podłoża? Nie można."
  :english
  "TODO")

(define-entry faq.general.25 ()
  :polish
  "Czy Promień (Uranopolis) może niszczyć żetony podłoża? Tak."
  :english
  "TODO")

(define-entry faq.general.26 ()
  :polish
  "Czy można wystawić żeton podłoża, kiedy któryś z innych graczy zagrał Terror? Nie można."
  :english
  "TODO")

(define-entry faq.general.27 ()
  :polish
  "Czy można za cel działania żetonu natychmiastowego zadającego obrażenia (np. Bomba, Mała bomba, Promień) wybrać Sztab (któremu nie może zadać obrażeń) zamiast żetonu podłoża, na którym stoi? Tak."
  :english
  "TODO")

(define-entry faq.general.28 ()
  :polish
  "Odbicie – Kto wykonuje atak, jednostka rzeczywiście atakująca czy jednostka odbijająca? Dla kogo liczą się obrażenia w grze na punkty? Jednostka wykonująca atak, Odbicie nie jest wykonaniem ataku. Nie otrzymuje się punktów za obrażenia zadane własnemu albo sojuszniczemu Sztabowi."
  :english
  "TODO")

(define-entry faq.general.29 ()
  :polish
  "Odbicie – Co stanie się z atakiem, który utkwi między dwoma Odbiciami? Jego efekt zostaje anulowany."
  :english
  "TODO")

(define-entry faq.general.30 ()
  :polish
  "Odbicie – Skoro Odbicie nie chroni przed odłamkami Klauna, to także nie chroni przed: Mad Bomberem, Pułapką, Toksyczną Bombą, Kwasomiotem, Fanatykiem? Tak."
  :english
  "TODO")

(define-entry faq.general.31 ()
  :polish
  "Odłamki – Czy odłamki np. Klauna, Mad Bombera, Pułapki, Kwasomiotu, Toksycznej Bomby, Fanatyka są atakiem? Nie."
  :english
  "TODO")

(define-entry faq.general.32 ()
  :polish
  "Odłamki – Skoro odłamki (np. Klaun, Kwasomiot, Toksyczna Bomba, Fanatyk, Mad Bomber) nie są atakiem, to czy Medyk chroni przed Obrażeniami zadanymi przez nie? Tak."
  :english
  "TODO")

(define-entry faq.general.33 ()
  :polish
  "Odłamki – Czy odłamki można dopakować jadem z Bojlera? Nie."
  :english
  "TODO")

(define-entry faq.general.34 ()
  :polish
  "Odłamki – Czy odłamki powodują eksplozję Pułapki i Mad Bombera? Tak."
  :english
  "TODO")

(define-entry faq.general.35 ()
  :polish
  "Odłamki – Czy odłamki mogą zadawać Obrażenia żetonom Podłoża? Nie."
  :english
  "TODO")

(define-entry faq.general.36 ()
  :polish
  "Paraliż – Czy Klaun, Mad Bomber, Pułapka, Kwasomiot, Fanatyk mogą eksplodować, będąc sparaliżowane? Nie. W przypadku Kwasomiotu Paraliż musi działać na jednostkę, która ma eksplodować, a nie na Inkubator."
  :english
  "TODO")

(define-entry faq.general.37 ()
  :polish
  "Paraliż a Pułapka – Czy np. Pułapka, Mad Bomber mogą eksplodować na skutek ataku z cechą Paraliż, wymierzonego w ściankę z Pułapką? Tak."
  :english
  "TODO")

(define-entry faq.general.38 ()
  :polish
  "Czy Medyk pochłania Obrażenia wywołujące eksplozję Pułapki (Mad Bombera w szczególnych okolicznościach), tym samym zapobiegając eksplozji? Nie."
  :english
  "TODO")

(define-entry faq.general.39 ()
  :polish
  "Odbicie a Pancerz – Czy skoro odbity atak traci zdolności dystansowe, to czy Pancerz przestaje zmniejszać siłę tego ataku? Nie."
  :english
  "TODO")

(define-entry faq.general.40 ()
  :polish
  "Odbicie a Latanie – Czy skoro odbity atak traci zdolności dystansowe, to czy jednostką z Cechą Latanie otrzyma Obrażenia od odbitego ataku dystansowego? Tak."
  :english
  "TODO")

(define-entry faq.general.41 ()
  :polish
  "Odbicie – Czy Paraliż wpływa na zdolność odbijania ataków? Nie."
  :english
  "TODO")

(define-entry faq.general.42 ()
  :polish
  "Odbicie a Shotgun – Jak zachowuje się strzał z Shotguna w kontakcie z Odbiciem? Odbity atak ma siłę równą atakowi, który otrzymać powinna jednostka z Odbiciem."
  :english
  "TODO")

(define-entry faq.general.43 ()
  :polish
  "Odbicie a Działko Wodne – Jak zachowuje się strzał z Działka Wodnego w kontakcie z Odbiciem? Odbity atak ma siłę równą atakowi, który otrzymać powinna jednostka z Odbiciem."
  :english
  "TODO")

(define-entry faq.general.44 ()
  :polish
  "Odbicie a Strzał snajperski – Czy jeśli zostanie wybrana za cel ataku Strzału snajperskiego jednostka stojąca za jednostką z Odbiciem, to czy Odbicie powstrzymuje taki atak? Nie."
  :english
  "TODO")

(define-entry faq.general.45 ()
  :polish
  "Strzał snajperski – Czy wybierając cel ataku, można nie wybrać żadnego celu, jeśli jakiś istnieje? Nie."
  :english
  "TODO")

(define-entry faq.general.46 ()
  :polish
  "Czy Egzekutor może dokonać egzekucji na polu, na którym będzie sparaliżowany, sieciowany lub przejęty? Tak."
  :english
  "TODO")

(define-entry faq.general.47 ()
  :polish
  "Czy Medyk może przyjąć obrażenie dla sztabu Stalowej policji za użycie zdolności Egzekutora? Nie."
  :english
  "TODO")

(define-entry faq.general.48 ()
  :polish
  "Cecha Szpieg – Czy zasieciowana jednostka z cechą Szpieg nadal korzysta z cudzych modułów? Nie korzysta."
  :english
  "TODO")

(define-entry faq.general.49 ()
  :polish
  "Cecha Szpieg – Czy jednostka z cechą Szpieg musi stykać się z modułem działającym na całej planszy, aby korzystać z jego zdolności, a może w ogóle nie może z takich korzystać? Musi się stykać."
  :english
  "TODO")

(define-entry faq.general.50 ()
  :polish
  "Cecha Szpieg – Czy jednostka z cechą Szpieg musi stykać się z modułem dalekiego zasięgu, czy wystarczy, że będzie w jego zasięgu? Musi się stykać."
  :english
  "TODO")

(define-entry faq.general.51 ()
  :polish
  "Cecha Szpieg – Czy Szpieg może korzystać ze zdolności Bojlera? Tak, ale nadal musi mieć znaczniki Jadu w swojej puli, aby skorzystać."
  :english
  "TODO")

(define-entry faq.general.52 ()
  :polish
  "Cecha Szpieg – Czy Szpieg może korzystać ze zdolności Śmietniska? Tak, ale nadal musi mieć znaczniki Paraliżu w swojej puli, aby skorzystać."
  :english
  "TODO")

(define-entry faq.general.53 ()
  :polish
  "Cecha Szpieg – Jak działa cecha Szpieg w przypadku negatywnych (karzących przeciwników) modułów (z rozróżnieniem, jeśli jest istotne na Agitatora i inne)? Działają one czy nie działają one na taką jednostkę? Cecha Szpieg nie ma z nimi nic wspólnego, ona pozwala korzystać tylko z tych dających bonusy."
  :english
  "TODO")

(define-entry faq.general.54 ()
  :polish
  "Szpieg a Skoper – Czy cecha Szpiega działa na zeskoperoowany własny bądź sojuszniczy moduł? Skoro nie jest wrogi, ale działa jakby był. Nie."
  :english
  "TODO")

(define-entry faq.general.55 ()
  :polish
  "Skoper – Jak działa Skoper? Przekazuje bonus swojej jednostce podpiętej do Skopera, umożliwia podpięcie się pod zeskoperowany Moduł, czy też daje skradziony bonus wszystkim jednostkom na planszy? Zeskoperowany Moduł odwraca swoje działanie, daje pozytywne cechy jednostkom właściciela Skopera oraz jego sojusznikom, a negatywne jego przeciwnikom, natomiast zasięg oddziaływania takiego Modułu nie zostaje zmieniony."
  :english
  "TODO")

(define-entry faq.general.56 ()
  :polish
  "Skoper – Czy Skoper oddziałuje na moduły negatywne (każące przeciwników)? Tak, moduły negatywne zaczynają oddziaływać na przeciwników właściciela Skopera."
  :english
  "TODO")

(define-entry faq.general.57 ()
  :polish
  "Skoper – Czy Skoper pozbawia Moduły cech ruchowych (Ruchu, Obrotu, Odepchnięcia), skoro w „normalny” sposób pozbawia cechy Podziemny? Nie."
  :english
  "TODO")

(define-entry faq.general.58 ()
  :polish
  "Skoper – Czy Skoper pozbawia moduły cechy wytrzymałość, skoro w „normalny” sposób pozbawia cechy Podziemny? Nie."
  :english
  "TODO")

(define-entry faq.general.59 ()
  :polish
  "Skoper – Czy zeskoperowane Pnącze oddziałuje na całą Macierz? Tak."
  :english
  "TODO")

(define-entry faq.general.60 ()
  :polish
  "Macierz a Sieć – Czy jeśli moduł Neodżungli podłączony jest do zasieciowanej jednostki będącej częścią Macierzy, to czy oddziałuje on na całą Macierz? Tak."
  :english
  "TODO")

(define-entry faq.general.61 ()
  :polish
  "Czy w jednej inicjatywie Medyk może uratować dwie jednostki, jeśli sam jest podpięty do innego Medyka? Tak."
  :english
  "TODO")

(define-entry faq.general.62 ()
  :polish
  "Czy Medyk może uratować jednostkę, jeśli równocześnie sam jest ratowany, przed zniszczeniem, przez innego Medyka? Tak."
  :english
  "TODO")

(define-entry faq.general.63 ()
  :polish
  "Czy Medyk może uratować jednostkę, jeśli równocześnie otrzymuje obrażenia niepowodujące jego zniszczenia? Tak."
  :english
  "TODO")

(define-entry faq.general.64 ()
  :polish
  "Czy jednostka podłączona do dwóch Modułów dających Mobilność może wykonać ruch o dwa pola, jeśli po pierwszym ruchu straci kontakt z oboma Modułami? Nie."
  :english
  "TODO")

(define-entry faq.general.65 ()
  :polish
  "Centrum Rozpoznania – Czy jednostka wykonująca podwójny (albo większy) ruch otrzymany jako całość, wykorzystując dodatkowo Centrum Rozpoznania, otrzymuje jeden dodatkowy Ruch, czy jeden za każdy standardowy, który posiadała? Jeden za każdy, który ma."
  :english
  "TODO")

(define-entry faq.general.66 ()
  :polish
  "Centrum Rozpoznania, Podwójny ruch, Podwójna mobilność – Czy jednostka wykonująca podwójny (albo większy) Ruch otrzymany jako całość (np. Centrum Rozpoznania, Podwójny ruch, Podwójna mobilność) może go wykonać, jeśli nie ma możliwości przemieścić się zgodnie z zasadami pole po polu pomiędzy startowym a docelowym polem (np. na co najmniej jednym z pól będzie: brak wolnego pola, znajdowała się Mina, sieciowana, Przejęta, bez zasilania, bez dodatkowej Wytrzymałości wymaganej do pozostawania na planszy)? Nie. Należy jednak pamiętać o wyjątkach dla różnych rodzajów jednostek oraz działaniu zdolności biernych przemieszczanej jednostki np. Sieci czy modułów i cech modułowych."
  :english
  "TODO")

(define-entry faq.general.67 ()
  :polish
  "Centrum Rozpoznania, Podwójny ruch, Podwójna mobilność – Czy jednostka wykonująca podwójny (albo większy) Ruch otrzymany jako całość (np. Centrum Rozpoznania, Podwójny ruch, Podwójna mobilność) może wrócić na pole, z którego rozpoczęła ów ruch? Tak. Centrum Rozpoznania, Podwójny ruch, Podwójna mobilność – Czy poruszając się o więcej niż jedno pole, z jednego źródła ruchu, można wykonać więcej niż jeden Obrót? Tak, jeden Obrót przysługuje nam za każdy punkt Ruchu."
  :english
  "TODO")

(define-entry faq.general.68 ()
  :polish
  "Czy jednostka wykonująca podwójny (albo większy) Ruch otrzymany jako całość (np. Centrum Rozpoznania, Podwójny ruch, Podwójna mobilność) może po wykonaniu części przysługującej jej Ruchów wykonać akcję, a następnie dalej się poruszać, jak czyni to Nocny Łowca? W szczególności czy Agitator może przejąć jednostkę podczas takiego ruchu, a sieciarz zasieciować? W takiej sytuacji (jeśli chce wykonać dalszy Ruch) jednostka wykonuje jedynie działania automatyczne, niewymagające podjęcia decyzji od gracza (np. działanie Modułu, cechy Modułu, sieciowanie, czy Przejęcie (choć w tym przypadku nie może dokonać Obrotu przejętą jednostką))."
  :english
  "TODO")

(define-entry faq.general.69 ()
  :polish
  "Centrum Rozpoznania, Podwójny ruch, Podwójna mobilność a Uwolnienie – Czy jednostka wykonująca podwójny (albo większy) Ruch otrzymany jako całość (np. Centrum Rozpoznania, Podwójny ruch, Podwójna mobilność) może po wykonaniu części przysługującej jej Ruchów wykonać Uwolnienie i poruszać się dalej? Tak."
  :english
  "TODO")

(define-entry faq.general.70 ()
  :polish
  "Czy Sztab Sharrash może podczas ruchu o dwa pola zacząć albo pomiędzy nimi zrobić Podziemną roszadę? Nie."
  :english
  "TODO")

(define-entry faq.general.71 ()
  :polish
  "Znaczniki jadu – Skoro pozostałe armie poza Missisipi i Neodżunglą posiadają zerowy limit znaczników jadu, to czy mogą po przejęciu/zeskoperowaniu jednostki jednej z tych dwóch armii kłaść znaczniki jadu? I czy może jest różnica, bo zeskoperowany Bojler jest nadal Missisipi a przejęta przez Agitatora jednostka już nie?. Nie. Nie ma różnicy."
  :english
  "TODO")

(define-entry faq.general.72 ()
  :polish
  "Znaczniki paraliżu – Co z armiami, które nie mają swoich znaczników, a przejmą lub zeskoperują moduły dające taką możliwość albo przejmą wojownika z taką zdolnością, to czy mogą korzystać ze znaczników Paraliżu? Czy jest różnica, czy zostały przejęte, czy zeskoperowane? Nie."
  :english
  "TODO")

(define-entry faq.general.73 ()
  :polish
  "Stalowa sieć – Czy Stalowa sieć może sieciować żetony podłoża? Nie."
  :english
  "TODO")

(define-entry faq.general.74 ()
  :polish
  "Stalowa sieć – Co dzieje się ze znacznikiem Stalowej sieci, gdy wraca do nas jednostka po przejęciu, która została wcześniej przez nas zasieciowana? Znacznik Stalowej sieci jest zdejmowany."
  :english
  "TODO")

(define-entry faq.general.75 ()
  :polish
  "Sieć dystansowa – Co dzieje się ze znacznikiem Sieci dystansowej, gdy wraca do nas jednostka po przejęciu, która została wcześniej przez nas zasieciowana? Znacznik Sieci dystansowej jest zdejmowany."
  :english
  "TODO")

(define-entry faq.general.76 ()
  :polish
  "Macki – Co dzieje się za znacznikiem Macek (do naszej tury, bo wtedy możemy go przełożyć), gdy wraca do nas jednostka po przejęciu, która została wcześniej przez nas zasieciowana? Znacznik Macek jest zdejmowany."
  :english
  "TODO")

(define-entry faq.general.77 ()
  :polish
  "Znaczniki jadu – Co dzieje się ze znacznikami jadu, gdy wraca do nas jednostka po przejęciu, która została wcześniej przez nas zatruta? Nic. Jednostka dalej jest zatruta."
  :english
  "TODO")

(define-entry faq.general.78 ()
  :polish
  "Macki – Czy Macki również pozostają na jednostce, na której leżały wcześniej, jeśli jedyny mackarz, który kieruje na nią swoje Macki, nie mógłby ich samodzielnie rzucić? Jego Macki skierowane są na aktywną sieć sieciarza. Tak, chyba że dana jednostka nie podlega sieciowaniu (np. Sztab Mephisto) wtedy nie."
  :english
  "TODO")

(define-entry faq.general.79 ()
  :polish
  "Macki – Co dzieje się ze znacznikiem Macek, leżącym na jednostce, po tym, gdy wszystkie kierujące na nią swoje macki jednostki mackujące zostały zasieciowane? Znacznik jest zdejmowany."
  :english
  "TODO")

(define-entry faq.general.80 ()
  :polish
  "Macki – Czy Macki spadają z Obiektu podczas tańca, jeśli dany Obiekt tańczył, czy jakikolwiek z Obiektów wykonał taniec? Tylko jeśli dany Obiekt tańczył."
  :english
  "TODO")

(define-entry faq.general.81 ()
  :polish
  "Macki – Czy Macki spadają po zagraniu żetonu Ruchu na Obiekt? Tak, jest tu analogicznie, jak w przypadku Stalowej sieci."
  :english
  "TODO")

(define-entry faq.general.82 ()
  :polish
  "Macki – Czy Macki może rzucić sparaliżowana jednostka? Tak."
  :english
  "TODO")

(define-entry faq.general.83 ()
  :polish
  "Macki – Czy można zdjąć dobrowolnie znacznik Macek z jednostki przeciwnika, nie przekładając go na inną? Nie."
  :english
  "TODO")

(define-entry faq.general.84 ()
  :polish
  "Macki – Czy można obrócić lub poruszyć jednostkę mackującą inną jednostkę? Tak, wtedy znacznik Macek zostaje zdjęty z obecnie mackowanej jednostki, jeśli był to jedyny mackarz kierujący na nią swoje macki."
  :english
  "TODO")

(define-entry faq.general.85 ()
  :polish
  "Przejęcie Kontroli a Sieć – Agitator/Sztab Vegas (pole A) przejął sieciarza (pole B) skierowanego na pole C sąsiadujące z Agitatorem/sztabem Vegas. Przeciwnik postawił na polu C sieciarza skierowanego na pole A. (Przy założeniu, że sieciarze są ze sobą sprzymierzeni (z jednej frakcji lub sojuszu)) Kto kogo będzie sieciował w takiej sytuacji? Sieciarz zasieciuje Agitatora/Sztab Vegas."
  :english
  "TODO")

(define-entry faq.general.86 ()
  :polish
  "Sieciarz kieruje swoją sieć na źródło dodatkowej Wytrzymałości, która utrzymuje na planszy innego Sieciarza, który kieruje swą sieć na pierwszego. Kto kogo będzie sieciował w takiej sytuacji? Pierwszy Sieciarz zasieciuje źródło Wytrzymałości, co spowoduje usunięcie z planszy drugiego Sieciarza."
  :english
  "TODO")

(define-entry faq.general.87 ()
  :polish
  "Sieciarz kieruje swoją sieć na moduł Paszczy, przy czym sam stoi obok ścianki, która otrzymuje właśnie atak, dodatkowo jest aktywny Inkubator ze znacznikiem Sieci. Kto kogo będzie sieciował w takiej sytuacji? Sieciarz zasieciuje moduł Paszczy."
  :english
  "TODO")

(define-entry faq.general.88 ()
  :polish
  "Sieciarz kieruje swoją sieć na Inkubator ze znacznikiem Sieci, przy czym sam stoi obok ścianki sztabu Mephisto, która posiada atak. Kto kogo będzie sieciował w takiej sytuacji? Sieciarz zasieciuje Inkubator."
  :english
  "TODO")

(define-entry faq.general.89 ()
  :polish
  "Sieciarz kieruje swoją sieć na Wszczep Kolca, którego znacznik na nim leży oraz aktywny jest Inkubator z Siecią. Kto kogo będzie sieciował w takiej sytuacji? Sieciarz zasieciuje Wszczep Kolca i jego znacznik zostanie zdjęty."
  :english
  "TODO")

(define-entry faq.general.90 ()
  :polish
  "Przejęcie Kontroli a Zasilanie – Czy któraś z jednostek będzie zasieciowana w następujących przykładach: Dwa sztaby Uranopolis i Vegas stoją koło siebie, a po obu stronach sąsiadująco do obu stoją elektrosieciarze, z których każdy kieruje sieć na inny sztab, a na tego kierującego na sztab Uranopolis jest skierowane przejęcie. ALBO Dwóch elektrosieciarzy stoi koło siebie, po obu stronach sąsiadująco do obu stoją sztaby Uranopolis i Vegas, sieciarze kierują swoje sieci na sztaby w taki sposób, aby na każdy ze sztabów była skierowana, chociażby jedna sieć oraz każdy z sieciarzy kieruje sieć na co najmniej jeden ze sztabów, a przejęcie jest skierowane na sieciarza, który na sieć skierowaną na sztab Uranopolis. w obu przykładach można zastąpić sztaby Agitatorem i jednostką ze zdolnością zasilania. Żadna z tych jednostek nie będzie zasieciowana."
  :english
  "TODO")

(define-entry faq.general.91 ()
  :polish
  "Przejęcie Kontroli a Zamrożenie – Co się stanie jeśli Agitator (pole A) kieruje Przejęcie Kontroli na Sieciarza (pole B), on natomiast kieruje Sieć na pole C na którym znajduje się jednostka z cechą Modułu Zamrożenie, która jest skierowana na pole A? Agitator zostanie Zamrożony."
  :english
  "TODO")

(define-entry faq.general.92 ()
  :polish
  "Agitator a Agitator + Skoper – Co dzieje się, gdy na jedną jednostkę skierowani są dwaj Agitatorzy w tym jeden zeskoperowany? A co jeśli trzech w tym jeden zeskoperowany? Dwie kontrole skierowane na tę samą jednostkę znoszą się wzajemnie (jednostka nie jest w tym przypadku kontrolowana przez żadnego Agitatora). Oczywiście, w przypadku trzech Agitatorów skierowanych na tę samą jednostkę dwie kontrole się zniosą, trzecia zadziała."
  :english
  "TODO")

(define-entry faq.general.93 ()
  :polish
  "Przejęcie Kontroli a Sieć – Czy w jakimkolwiek przypadku sieciarz może zasieciować jednostkę z aktywnym Przejęciem Kontroli skierowanym na niego? Nie. W tym w następujących szczególnych przypadkach: zniesienia Przejęcia Kontroli w przypadku dwóch wrogich Agitatorów kierujących przejęcia na danego sieciarza; przewagi w Przejęciach Kontroli jednej ze stron (dalej sieciarz nie może zasieciować słabszej strony); Przejęcia sieciarza przez zeskoperowanego Agitatora (ten wciąż jest wrogi wobec sieciarza). Należy pamiętać o tym, że jeśli sieciarz kieruje sieć na trzecią jednostkę pozwalającą Przejąć nad nim kontrolę jednostce z Przejęciem Kontroli, to dana jednostka nie będzie mogła Przejąć nad nim kontroli i w szczególnym przypadku większej liczny kierunków sieci będzie mógł ją zasieciować."
  :english
  "TODO")

(define-entry faq.general.94 ()
  :polish
  "Zasilanie – Czy można odpychać jednostki Uranoplis pozbawione Zasilania? Czy należy je traktować pod każdym względem jak zasieciowane? Nie można. Należy traktować je jakby były zasieciowane."
  :english
  "TODO")

(define-entry faq.general.95 ()
  :polish
  "Zasilanie – Czy zdolność Zasilania Mechanika jest traktowana jako Cecha Modułowa? Nie."
  :english
  "TODO")

(define-entry faq.general.96 ()
  :polish
  "Bio-droid a Sieć – Jeśli Bio-droid został zniszczony w czasie kiedy był zasieciowany, to czy wróci na wierzch stosu dobierania? Tak."
  :english
  "TODO")

(define-entry faq.general.97 ()
  :polish
  "Ładunek wybuchowy – Czy Ładunek wybuchowy może niszczyć żetony podłoża? Nie."
  :english
  "TODO")

(define-entry faq.general.98 ()
  :polish
  "Ładunek wybuchowy – Czy Ładunek wybuchowy jest atakiem? Tak."
  :english
  "TODO")

(define-entry faq.general.99 ()
  :polish
  "Ładunek wybuchowy – Jeśli Ładunek wybuchowy jest atakiem, to czy można jego atak dopakować Bojlerem lub Śmietniskiem? Tak."
  :english
  "TODO")

(define-entry faq.general.100 ()
  :polish
  "Zatrucie – Czy powoduje detonację: Pułapki, Mad Bombera? I czy ma na to wpływ limit znaczników Jadu, jeśli nie mam wolnego znacznika Jadu, to czy był atak, czy nie? Nie. Nie."
  :english
  "TODO")

(define-entry faq.general.101 ()
  :polish
  "Sztab Vegas atakuje w 5 czy 6 kierunkach? Skoro Paszcza nie daje mu efektu (dodania w brakującego kierunku ataku) oraz w przypadku Mephista jest informacja, iż atakuje jedynie w 3 kierunkach, czego nie ma w przypadku Vegas. Pięciu, nie atakuje ścianką z przejęciem."
  :english
  "TODO")

(define-entry faq.general.102 ()
  :polish
  "Czy jeśli jednostka poprzez Odepchnięcie zmienia Agitatorów, to może zostać powtórnie obrócona? Tak."
  :english
  "TODO")

(define-entry faq.general.103 ()
  :polish
  "Czy jeśli poprzez Roszadę zamieni się miejscami Agitatorów, tak że przejmują nawzajem te same jednostki, które były przejęte przez nich wcześniej, to czy można obrócić ponownie przejęte jednostki? Tak."
  :english
  "TODO")

(define-entry faq.general.104 ()
  :polish
  "Czy jeśli poprzez Roszadę zamieni się miejscami przejęte jednostki, to czy można obrócić ponownie przejęte jednostki? Tak."
  :english
  "TODO")

(define-entry faq.general.105 ()
  :polish
  "Czy jeśli zostanie obrócony Agitator o 360 stopni (np. poprzez moduł), to czy można obrócić powtórnie przejętą jednostkę. Nie."
  :english
  "TODO")

(define-entry faq.general.106 ()
  :polish
  "Uwolnienie – Jeśli na przejęty Obiekt skierowany jest sieciarz Vegas, to ile Obrażeń otrzymuje Obiekt w razie wykonania Tańca albo ruchu? Jedno (za Przejęcie Kontroli), bo swoich jednostek sieciarz nie sieciuje."
  :english
  "TODO")

(define-entry faq.general.107 ()
  :polish
  "Uwolnienie – Jeśli na przejęty Obiekt skierowana jest drugi Agitator, to ile ran otrzymuje Obiekt w razie wykonania tańca albo ruchu? 1 (za Przejęcie Kontroli), bo swoich jednostek Agitator nie przejmuje."
  :english
  "TODO")

(define-entry faq.general.108 ()
  :polish
  "Uwolnienie – Czy można uwolnić się z sieci Dancer albo Obiekt spod sieci/Macek poprzez zagranie żetonu Odepchnięcie? Nie."
  :english
  "TODO")

(define-entry faq.general.109 ()
  :polish
  "Uwolnienie – Czy można uwolnić się z sieci Dancer poprzez zagranie Roszady z przeciwnikiem? Tak."
  :english
  "TODO")

(define-entry faq.general.110 ()
  :polish
  "Uwolnienie a Polowanie – Czy można uwolnić się z sieci Dancer poprzez zagranie Polowania? Tak, w tym celu należy wykorzystać składową Ruchu, a następnie można wykonać Atak, jeśli jednostka nie jest sieciowana."
  :english
  "TODO")

(define-entry faq.general.111 ()
  :polish
  "Uwolnienie – Czy można uwolnić się z sieci Dancer poprzez użycie zdolności sztabu np. Odepchnięcie? Nie."
  :english
  "TODO")

(define-entry faq.general.112 ()
  :polish
  "Uwolnienie – Czy można uwolnić się z sieci Dancer poprzez użycie zdolności Sztabu np. Obrót, Ruch w celu poruszenia własnej jednostki, aby sieciowała Żółty Obiekt? Nie."
  :english
  "TODO")

(define-entry faq.general.113 ()
  :polish
  "Uwolnienie – Czy można uwolnić się z sieci Dancer poprzez wykorzystanie Podziemnej roszady? Nie."
  :english
  "TODO")

(define-entry faq.general.114 ()
  :polish
  "Uwolnienie – Czy można uwolnić się z sieci Dancer albo Obiekt z sieci poprzez wykorzystanie Modułu ruchu? Nie."
  :english
  "TODO")

(define-entry faq.general.115 ()
  :polish
  "Uwolnienie – Czy Obiekt żółty może uwolnić się poprzez dokonanie obrotu po zagraniu żetonu Ruchu w taki sposób, aby kierować swoją sieć na sieciarza, który go sieciuje? Tak."
  :english
  "TODO")

(define-entry faq.general.116 ()
  :polish
  "Uwolnienie – Czy można obrócić Obiekt przy pomocy żetonu Ruchu, kiedy jest zasieciowany, skoro można tego dokonać przy pomocy Tańca? Tak."
  :english
  "TODO")

(define-entry faq.general.117 ()
  :polish
  "Uwolnienie – Czy można zagrać na przeciwnika Odepchnięcie, Przyciągnięcie, Zasłonę dymną albo Roszadę z przeciwnikiem, kiedy jest pod siecią Dancer albo na Obiekt, który jest sieciowany? Nie."
  :english
  "TODO")

(define-entry faq.general.118 ()
  :polish
  "Uwolnienie – Jeśli na aktywnym Inkubatorze jest aktywowana Sieć, to ile Obrażeń otrzyma Obiekt podczas Uwolnienia, jeśli znajdują się na nim dwa Kolce albo co najmniej jeden Kolec i sąsiaduje ze ścianką Sztabu Mephisto z aktywnym atakiem? Jedno, bo Kolce są traktowane jako atak Sztabu."
  :english
  "TODO")

(define-entry faq.general.119 ()
  :polish
  "Uwolnienie – Jeśli na zamoackowany Obiekt kieruje swoje Macki więcej niż jedna jednostka, to ile Obrażeń otrzyma taki Obiekt podczas Uwolnienia? Jedno, bo tylko jedne Macki mogą być aktywne."
  :english
  "TODO")

(define-entry faq.general.120 ()
  :polish
  "Uwolnienie a Medyk – W którym momencie jest zdejmowany Medyk, który pochłonął Obrażenie spowodowane Uwolnieniem, przed czy po wykonaniu przemieszczenia uwalnianego Sztabu? Po."
  :english
  "TODO")

(define-entry faq.general.121 ()
  :polish
  "Uwolnienie a Roszada z Medykiem – Czy można równocześnie wykorzystać Medyka do wykonania z nim Roszady w celu Uwolnienia oraz pochłonięcia Obrażenia wynikającego z niego, jeśli Sztab był w zasięgu jego działania przed Uwolnieniem? Tak."
  :english
  "TODO")

(define-entry faq.general.122 ()
  :polish
  "Przejęcie Kontroli a zapełnienie planszy – Jeśli jednostka zapełniająca planszę jest kładziona na polu, na którym jest przejmowana, to czy na zakończenie Bitwy jest ona obracana, zgodnie z wolą właściciela Agitatora, zgodnie z zasadą, że jeśli Przejęcie nastąpi w czasie Bitwy, to na jej koniec można obrócić? Analogicznie, jeśli to Agitator zapełni planszę i ścianką z przejęciem będzie zwrócony na jednostkę, którą może przejąć, to obróci ją na koniec Bitwy? Tak."
  :english
  "TODO")

(define-entry faq.general.123 ()
  :polish
  "Przejęcie Kontroli a Obiekt – Co się dzieje, gdy ginie przejęty Obiekt? Przegrywa gracz grający Dancerem."
  :english
  "TODO")

(define-entry faq.general.124 ()
  :polish
  "Przejęcie Kontroli a Obiekt – Co się dzieje, gdy wszystkie trzy Obiekty zostaną przejęte? Nic, gra toczy się dalej w normalny sposób."
  :english
  "TODO")

(define-entry faq.general.125 ()
  :polish
  "Przejęcie Kontroli a Obiekt – Czy na przejęty Obiekt oddziałują Moduły gracza, który go przejął? Tak, jak na każdą inną jego jednostkę."
  :english
  "TODO")

(define-entry faq.general.126 ()
  :polish
  "Ożywianie a Przejęcie – Co następuje najpierw Ożywianie jednostek czy obrócenie jednostek Przejętych podczas Bitwy, bo w obu przypadkach jest napisane „natychmiast po zakończeniu bitwy”? Co z nowo dostawianymi jednostkami (na polach, na których są przejmowane), bo one pojawiły się już po Bitwie? Najpierw Vegas ma możliwość obrócić przejęte jednostki podczas Bitwy, potem Death Breath dostawia jednostki, następnie, jeśli dostawienie jednostek nie wywołało kolejnej Bitwy, Vegas może obrócić przejęte właśnie jednostki."
  :english
  "TODO")

(define-entry faq.general.127 ()
  :polish
  "Terror a Ożywianie – Czy zagrany Terror blokuje możliwość wystawiania jednostek z cechy Sztabu Ożywianie? Nie."
  :english
  "TODO")

(define-entry faq.general.128 ()
  :polish
  "Ożywianie – Czy wykładanie znaczników ran na pola zabitych przez Death Breath przeciwników jest zdolnością Sztabu, czy cechą armii? Czy jeśli sztab Death Breath jest zasieciowany, to czy wykłada się znaczniki ran na polach, na których jednostki Death Breath zabiły przeciwnika? Jest to cecha sztabu. Nie wykłada się."
  :english
  "TODO")

(define-entry faq.general.129 ()
  :polish
  "Ożywianie – Co dzieje się z już wyłożonymi znacznikami ran na polach, jeśli sztab Death Breath zostanie zasieciowany? Pozostają na planszy do końca Bitwy i jeśli sztab Death Breath zostanie odsieciowany do czasu przywracania jednostek, to może je wykorzystać."
  :english
  "TODO")

(define-entry faq.general.130 ()
  :polish
  "Ożywianie – Czy można Ożywić na polu, na którym znajduje się żeton Podłoża? Tak."
  :english
  "TODO")

(define-entry faq.general.131 ()
  :polish
  "Powrót – Czy można wystawić jednostkę na polu, na którym znajduje się żeton Podłoża za pomocą żetonu Powrót? Tak."
  :english
  "TODO")

(define-entry faq.general.132 ()
  :polish
  "Powrót – Czy można zagrać żeton Powrót, jeśli sztab Death Breath jest zasieciowany? Tak."
  :english
  "TODO")

(define-entry faq.general.133 ()
  :polish
  "Odbicie a Ożywianie – Czy gdy jednostka Death Breath zabije siebie samą (albo sojuszniczą) poprzez Odbicie, to kładziemy na jej miejsce znacznik rany? Tak."
  :english
  "TODO")

(define-entry faq.general.134 ()
  :polish
  "Ożywianie – Czy jeśli tylko cześć śmiertelnych ran zada jednostką Death Breath, to kładziemy znacznik rany na polu? Tak, ale przynajmniej jedno obrażenie musi być zadane przez Death Breath w inicjatywie śmierci jednostki."
  :english
  "TODO")

(define-entry faq.general.135 ()
  :polish
  "Wchłanianie – Czy jeśli tylko cześć śmiertelnych ran zada jednostką z cechą Wchłanianie (Pożarcie (Death Breath)), to czy zostanie przywrócony punkt Wytrzymałości Sztabu? Tak, ale przynajmniej jedno obrażenie musi być zadane przez jednostkę z cechą Wchłanianie (Pożarcie (Death Breath)) w inicjatywie śmierci jednostki."
  :english
  "TODO")

(define-entry faq.general.136 ()
  :polish
  "Ożywianie a Atak dystansowy Death Breath – Czy po zabiciu jednostki przeciwnika takim atakiem jest kładziony znacznik rany? Tak."
  :english
  "TODO")

(define-entry faq.general.137 ()
  :polish
  "Atak dystansowy Death Breath + Wchłanianie (Pożarcie (Death Breath)) – Czy jednostka zabita z takiego ataku może zostać Wchłonięta (Pożarta)? Tak."
  :english
  "TODO")

(define-entry faq.general.138 ()
  :polish
  "Wchłanianie – Co przyznajemy najpierw dodatnie punkty Wytrzymałości Sztabu ze Wchłaniania czy ujemne za obrażenia? Na koniec danego segmentu inicjatywy przyznajmy różnice w odzyskanych i utraconych punktach Wytrzymałości."
  :english
  "TODO")

(define-entry faq.general.139 ()
  :polish
  "Wchłanianie – Czy jeśli Wchłoniecie nastąpiło w późniejszym segmencie Inicjatywy niż zniszczenie Sztabu, to czy przywróci ono Sztab do gry? Nie."
  :english
  "TODO")

(define-entry faq.general.140 ()
  :polish
  "Odbicie – Czy Mephisto sam się rani poprzez Odbicie? Tak."
  :english
  "TODO")

(define-entry faq.general.141 ()
  :polish
  "Świder (Mephisto) – Czy przenoszona jednostka może się Obrócić, jeśli tak to czy robi to przed przeniesieniem, po przeniesieniu, czy może w jego trakcie? Tak, ewentualny Obrót jest dokonywany w trakcie przenoszenia."
  :english
  "TODO")

(define-entry faq.general.142 ()
  :polish
  "Świder (Mephisto) – Czy można wykorzystać działanie Świdra tylko do Obrócenia jednostki, bez przenoszenia jej na inne pole? Tak."
  :english
  "TODO")

(define-entry faq.general.143 ()
  :polish
  "Czy Mephisto może użyć Szczęk (wywołać Bitwę) po tym, jak który którykolwiek gracz dobrał ostatni żeton? Nie."
  :english
  "TODO")

(define-entry faq.general.144 ()
  :polish
  "Kolec – Czy jeśli jednostka z Kolcem jest atakowana również bezpośrednio przez Sztab Mephisto, to czy można wybrać, aby otrzymała zamiast normalnego ataku ze ścianki, atak z Kolca? Nie, pierwszeństwo ma zwykły atak Sztabu, dotyczy to również Sieci (jednostka jest sieciowana raz z odpowiedniej ścianki Sztabu)."
  :english
  "TODO")

(define-entry faq.general.145 ()
  :polish
  "Kolec – Czy znacznik Kolca spada z jednostki na koniec Bitwy, jeśli Mephisto nie wykona podczas tej Bitwy ataku? Tak."
  :english
  "TODO")

(define-entry faq.general.146 ()
  :polish
  "Kolec a Przejęcie – Jak działa Kolec po Przejęciu, bo w jego opisie są odniesienia do sztabu Mephisto? Działa normalnie, można użyć właściwego znacznika Kolca, a o parametrach ataku decyduje Sztab obecnego właściciela."
  :english
  "TODO")

(define-entry faq.general.147 ()
  :polish
  "Inkubator – Czy po aktywowaniu go dodaje daną zdolność wszystkim jednostkom danej armii, czy tylko Sztabowi? Wszystkim jednostkom z wyjątkiem tych znaczników, przy których w opisie jest wprost napisane, że daje tylko sztabowi."
  :english
  "TODO")

(define-entry faq.general.148 ()
  :polish
  "Inkubator + sieć – Co dzieje się ze znacznikiem znajdującym się na Inkubatorze, który został zasieciowany? Zostaje na nim, do końca najbliższej Bitwy albo usunięcia z planszy danego Inkubatora."
  :english
  "TODO")

(define-entry faq.general.149 ()
  :polish
  "Inkubator + sieć + Ożywianie – Co następuje najpierw zdjęcie znacznika sieci z Inkubatora czy Ożywianie? Zdjęcie znacznika sieci z Inkubatora."
  :english
  "TODO")

(define-entry faq.general.150 ()
  :polish
  "Inkubator + sieć + Vegas – Czy zdolność sieciowania działa na sztaby przeciwnika, skoro sztaby nie mogą się wzajemnie atakować, z drugiej strony sieć nie jest atakiem? Tak."
  :english
  "TODO")

(define-entry faq.general.151 ()
  :polish
  "Czy można zagrać żeton Podmiana po tym, jak ktoś zagrał Terror? Tak."
  :english
  "TODO")

(define-entry faq.general.152 ()
  :polish
  "Czy można wykorzystać zdolność jednostek ze znakiem zapytania np. Transporter (Smart) czy Pacyfikator (Stalowa Policja), jeśli został zagrany Terror? Tak."
  :english
  "TODO")

(define-entry faq.general.153 ()
  :polish
  "Czy po przejęciu jednostki ze znakiem zapytania np. Transporter (Smart) czy Pacyfikator (Stalowa Policja) można skorzystać z ich cechy (?)? Tak."
  :english
  "TODO")

(define-entry faq.general.154 ()
  :polish
  "Czy można korzystając ze zdolności jednostki ze znakiem zapytania np. Transporter (Smart) czy Pacyfikator (Stalowa Policja) położyć żeton Podłoża? Nie."
  :english
  "TODO")

(define-entry faq.general.155 ()
  :polish
  "Odpady – Czy dają dodatkowe obrażenie do każdego ataku albo innego źródła (jak jest w instrukcji w tym np. strzał o sile 1 i pancerz; Zatrucie; itp.), czy tylko takiego ataku, który zadaje danej jednostce jakiekolwiek obrażenia? Jeśli tak, to czy wówczas podpięty Bojler pod jednostkę z Zatruciem, doda kolejny znacznik jadu? Czy wtedy taki atak zostaje w całości wchłonięty przez medyka (np. Zatrucie)? Do takiego, który zadaje sam jakiekolwiek obrażenia."
  :english
  "TODO")

(define-entry faq.general.156 ()
  :polish
  "Odpady – Czy Obiekt otrzyma dodatkowe obrażenie z Odpadów, jeśli otrzymuje obrażenie od żetonu Natychmiastowego? Tak."
  :english
  "TODO")

(define-entry faq.general.157 ()
  :polish
  "Czy jeśli Odpady stoją obok sztabu Stalowej Policji i jest rzucana Stalowa sieć (bez Wyrzutni stalowej sieci) albo używamy zdolności Egzekutora (do zniszczenia jednostki przeciwnika), to sztab otrzymuje dwa obrażenia? Nie."
  :english
  "TODO")

(define-entry faq.general.158 ()
  :polish
  "Transformator Gaussa – Czy transformator Gausa może w jednej Bitwie zmienić atak dwóm podłączonym jednostkom, czy tylko jednej? Dwóm."
  :english
  "TODO")

(define-entry faq.general.159 ()
  :polish
  "Transformator Gaussa / Kwatermistrz – Ograniczenie zamiany jednego kierunku i inicjatywy ataku na Bitwę jest dla danej jednostki czy kierunku działania modułu? Dla jednostki."
  :english
  "TODO")

(define-entry faq.general.160 ()
  :polish
  "Mutacja (sztab NY) – czy coś się dzieje, jeśli jednostka ze znacznikiem rany zostanie poruszona (Ruch, Odepchnięcie, Przyciągnięcie, Świder) z pola sąsiadującego z modułem dającym jej Wytrzymałość na sąsiednie pole z innym modułem dającym jej Wytrzymałość albo na inne pole z tym samym modułem, czy nic? Nic, bo tu i tu ma dodatkową wytrzymałość."
  :english
  "TODO")

(define-entry faq.general.161 ()
  :polish
  "Mutacja (sztab NY) – czy coś się dzieje, jeśli jednostka dająca wytrzymałość innym zostanie poruszona na inne pole sąsiednie z jednostką, która tylko dzięki niej pozostaje na planszy, w taki sposób, że będzie do niej nadal podłączona, czy nic? Nic, bo nadal ma dodatkową wytrzymałość."
  :english
  "TODO")

(define-entry faq.general.162 ()
  :polish
  "Czy moduły (oraz inne jednostki bądź kafle) zwiększające wytrzymałość jednostek działają na Obiekty Dancer? Nie."
  :english
  "TODO")

(define-entry faq.general.163 ()
  :polish
  "Żetony podłoża – Czy można postawić żeton podłoża na innym żetonie podłoża? Nie."
  :english
  "TODO")

(define-entry faq.general.164 ()
  :polish
  "Zdolność Szpieg – Czy jednostka ze zdolnością Szpieg może korzystać z nie-modułowych zdolności sztabów: Obrót (DDM, Mephisto)? Nie."
  :english
  "TODO")

(define-entry faq.general.165 ()
  :polish
  "Zdolność Szpieg – Czy jednostka ze zdolnością Szpieg może korzystać z nie-modułowych zdolności sztabów: Odepchnięcie (Missisipi)? Nie."
  :english
  "TODO")

(define-entry faq.general.166 ()
  :polish
  "Cecha Szpieg – Czy jednostka z cechą Szpieg może korzystać ze zdolności Podziemna roszada (jeśli druga jednostka z cechą Szpieg uzyskała cechę podziemny lub moduł Podziemia został zeskoperowany przez sojusznika)? Nie."
  :english
  "TODO")

(define-entry faq.general.167 ()
  :polish
  "Przejęcie + Skoper – Co się dzieje, gdy dwóch Agitatorów kieruje na siebie przejęcie, a jeden z nich jest zeskoperowany przez Posterunek? Zeskoperowany Agitator przejmuje drugiego Agitatora, bo ten drugi nie może przejąć swojej jednostki."
  :english
  "TODO")

(define-entry faq.general.168 ()
  :polish
  "Czy wystrzelona rakieta z Wyrzutni Rakiet może wystrzelić na pole, a następnie zawrócić, przelecieć nad Wyrzutnią Rakiet i trafić w cel? Tak."
  :english
  "TODO")

(define-entry faq.general.169 ()
  :polish
  "Czy Wyrzutnia Rakiet może wystrzelić poza obszar gry, a następnie zawrócić i trafić w cel? Nie. Pola poza obszarem danej gry nawet jak są (zewnętrzny krąg), to nie liczą się, Wyrzutnia Rakiet musi mieć przed sobą pole."
  :english
  "TODO")

(define-entry faq.general.170 ()
  :polish
  "Wyrzutnia Rakiet – Czy kierując rakietą, można tak nią pokierować, aby nie trafiła w żaden cel, jeśli jest jakiś w jej zasięgu? Tak."
  :english
  "TODO")

(define-entry faq.general.171 ()
  :polish
  "Cecha Szpieg – Czy jednostka z cechą Szpieg może korzystać ze zdolności nietypowych Sztabów przeciwnika jak: tworzenie Macierzy, rzucanie Stalowej Sieci, Przejmowanie, Ożywianie? Nie."
  :english
  "TODO")

(define-entry faq.general.172 ()
  :polish
  "Czy medyk pochłania Jad lub Paraliż razem z obrażeniami? Tak. Medyk pochłania cały atak pod warunkiem, że zostaną zadane jakiekolwiek obrażenia."
  :english
  "TODO")

(define-entry faq.general.173 ()
  :polish
  "Czy Pancerz i Wytrzymałość działają po zasieciowaniu? Tak."
  :english
  "TODO")

(define-entry faq.general.174 ()
  :polish
  "Czy siła ataku ma wpływ na liczbę kładzionych znaczników Jadu? Nie."
  :english
  "TODO")

(define-entry faq.general.175 ()
  :polish
  "Czy Medyk przejmuje cały atak, czy tylko jedno obrażenie? Cały."
  :english
  "TODO")

(define-entry faq.general.176 ()
  :polish
  "Czy sieć przenoszona przez Kolec działa przez cały czas, czy tylko w segmencie inicjatywy ataku Sztabu? Przez cały czas."
  :english
  "TODO")

(define-entry faq.general.177 ()
  :polish
  "Czy jeśli jednostka Death Breath ginie po raz kolejny, to trafia do Puli zombi? Tak."
  :english
  "TODO")

(define-entry faq.general.178 ()
  :polish
  "Czy jedna jednostka może skorzystać wielokrotnie z tego samego modułu ruchu w jednej turze? Czy jest różnica, jeśli zmieni łapkę modułu? Nie."
  :english
  "TODO")

(define-entry faq.general.179 ()
  :polish
  "Transport (Uranopolis) – Czy możliwa jest następująca sekwencja wydarzeń: wyłożenie Transportu i podłączenie go do jednostki A -> obrót/ruch jednostką A -> obrócenie Transportu w kierunku jednostki B -> obrót/ruch jednostką B? Tak."
  :english
  "TODO")

(define-entry faq.general.180 ()
  :polish
  "Wchłonięcie (Pożarcie (Death Breath)) a Klaun – Czy Klaun może zostać Wchłonięty (Pożarty (Death Breath)), jeśli w tej samej inicjatywie eksploduje? Tak, ale Obrażenia, które otrzymuje z zewnątrz, muszą być śmiertelne."
  :english
  "TODO")

(define-entry faq.general.181 ()
  :polish
  "Ożywianie – Czy na miejsce zabitego Klauna przez jednostkę Death Breath, jest kładziony znacznik Rany, jeśli w tej samej inicjatywie eksploduje? Tak, ale obrażenia, które otrzymuje z zewnątrz, muszą być śmiertelne."
  :english
  "TODO")

(define-entry faq.general.182 ()
  :polish
  "Ożywianie a Pułapka – Czy jeśli na skutek zadania Obrażeń przez jednostkę Death Breath następuje detonacja jednostki ze zdolnością Pułapki (np. Pułapka, Mad Bomber), to na jej miejsce jest kładziony znacznik Rany? Tak."
  :english
  "TODO")

(define-entry faq.general.183 ()
  :polish
  "Ożywianie a Pułapka – Czy skoro atak Death Breath powoduje detonację jednostki z Pułapką, to czy zabite jednostki przez jej odłamki są traktowane jako zabitą przez jednostkę Death Breath? Nie."
  :english
  "TODO")

(define-entry faq.general.184 ()
  :polish
  "Czy można zdetonować żeton leżący przy brzegu planszy (np. Klaun, Kwasomiot, Eksplozja, Pułapka, Toksyczna bomba)? Tak."
  :english
  "TODO")

(define-entry faq.general.185 ()
  :polish
  "Kwasomiot – Czy wybór jednostki do detonacji jest obligatoryjny? Nie."
  :english
  "TODO")

(define-entry faq.general.186 ()
  :polish
  "Czy znaczniki Ran kładzione przez Death Breath na polach po zabitych jednostkach przeciwnika mogą zostać zdjęte podczas Bitwy z innego powodu niż pojawienie się na tym polu jednostki? Czy w szczególności są zdejmowane z Dziury albo Toksycznej bomby, gdy eksploduje? Nie."
  :english
  "TODO")

(define-entry faq.general.187 ()
  :polish
  "Co się dzieje, gdy zraniona jednostka stojąca przy Module lub Sztabie dającym jej dodatkową wytrzymałość, która utrzymuje się tylko dzięki nim przy życiu, zostanie odepchnięta (tracąc bonus), ale natychmiast dostanie się pod działanie własnego Medyka albo była podłączona przed poruszeniem? W tym przypadku jednostka natychmiast ginie i Medyk nie ma prawa zadziałać, ponieważ jednostka ta nie otrzymuje w danej chwili żadnej rany, a jedynie traci bonus (w tym wypadku bonus utrzymywał ją przy życiu). To ranna jednostka, a nie Medyk, jest zdejmowana z planszy."
  :english
  "TODO")

(define-entry faq.general.188 ()
  :polish
  "Czy po użyciu żetonu Podmiana albo po podmianie jednostki Transportera, albo Pacyfikatora zdjęta jednostka wraca na rękę? Nie."
  :english
  "TODO")

(define-entry faq.general.189 ()
  :polish
  "Kwatermistrz a Sztab – Czy Kwatermistrz działa na sztaby? Tak, ale dalej nie mogą ranić innych sztabów."
  :english
  "TODO")

(define-entry faq.general.190 ()
  :polish
  "Czy ze zdolności Dziury można skorzystać więcej niż raz na turę? Nie."
  :english
  "TODO")

(define-entry faq.general.191 ()
  :polish
  "Czy Dziura może wciągnąć jednostkę zasieciowaną? Nie. Nie można poruszyć jednostki zasieciowanej."
  :english
  "TODO")

(define-entry faq.general.192 ()
  :polish
  "Czy można położyć dwa Kolce na jednej jednostce? Tak."
  :english
  "TODO")

(define-entry faq.general.193 ()
  :polish
  "Czy znacznik Stalowej sieci spada z Obiektu tylko po jego obróceniu wyniku zagrania na niego żetonu Ruchu? Tak, w szczególności można wykonać pełny obrót."
  :english
  "TODO")

(define-entry faq.general.194 ()
  :polish
  "Czy stan Uwolnienia Obiektów utrzymuje dłużej, czy tylko na moment wykonywania Tańca? Tylko na moment wykonania Tańca, wyjątek stanowi Obiekt Żółty, który jeśli obróci się swoją siecią w stronę jedynego sieciarza, który go sieciuje to nie będzie sieciowany."
  :english
  "TODO")

(define-entry faq.general.195 ()
  :polish
  "Czy znacznik Paraliżu spadnie z Obiektu po zagraniu na niego żetonu Akcji? Nie."
  :english
  "TODO")

(define-entry faq.general.196 ()
  :polish
  "Czy Skoper oddziałuje na Motocyklistę, pozbawiając go cechy modułu, jak w przypadku Obiektów? Tak."
  :english
  "TODO")

(define-entry faq.general.197 ()
  :polish
  "Czy Szpieg może korzystać z cech modułowych Motocyklisty jak w przypadku Obiektów? Tak."
  :english
  "TODO")

(define-entry faq.general.198 ()
  :polish
  "Łańcuch – Czy cecha specjalna sztabu Iron Gang Łańcuch pozwala innym jednostkom z cechą Łańcucha rzucić Łańcuch, czy pozwala do użycia Łańcucha pomiędzy sztabem a inną jednostką z cechą Łańcuch? Działa dokładnie tak samo, jak na innych jednostkach, pozwala do użycia Łańcucha pomiędzy sztabem a inną własną jednostką z cechą Łańcuch."
  :english
  "TODO")

(define-entry faq.general.199 ()
  :polish
  "Łańcuch – Czy do użycia Łańcucha obie jednostki nie mogą być zasieciowane lub sparaliżowane, czy wystarczy tylko jedna? Tak, obie nie mogą być zasieciowane lub sparaliżowane."
  :english
  "TODO")

(define-entry faq.general.200 ()
  :polish
  "Łańcuch – Czy można rzucić Łańcuch pomiędzy swoją a cudzą jednostką? Nie."
  :english
  "TODO")

(define-entry faq.general.201 ()
  :polish
  "Łańcuch a Medyk – Co dzieje się z Medykiem Death Breath, jeśli przejmie obrażenia z Łańcucha? Trafia do Puli Zombie."
  :english
  "TODO")

(define-entry faq.general.202 ()
  :polish
  "Łańcuch – Czy Łańcuch można zmodyfikować innymi modułami niż zwiększenie siły (np. Bojler, Śmietnisko)? Nie, nie można modyfikować go za pomocą modułów."
  :english
  "TODO")

(define-entry faq.general.203 ()
  :polish
  "Łańcuch a Przekierowanie – Czy na działanie Łańcucha mają wpływ jednostki Przekierowujące? Nie."
  :english
  "TODO")

(define-entry faq.general.204 ()
  :polish
  "Łańcuch – Czy przed działaniem Łańcucha chroni Pancerz jeśli znajduje się z obu stron linii Łańcucha? Nie."
  :english
  "TODO")

(define-entry faq.general.205 ()
  :polish
  "Łańcuch – Czy przed działaniem Łańcucha chroni Odbicie jeśli znajduje się z obu stron linii Łańcucha? Nie."
  :english
  "TODO")

(define-entry faq.general.206 ()
  :polish
  "Medyk dystansowy – Czy Medyk dystansowy może przejąć obrażenia, jeśli sam ginie w tej samej inicjatywie? Nie."
  :english
  "TODO")

(define-entry faq.general.207 ()
  :polish
  "Medyk dystansowy – Czy Medyk dystansowy może przejąć obrażenia, jeśli jednostka, którą ratuje, otrzymuje więcej niż: jeden atak w jednym segmencie inicjatywy? Tak, ale przejmuje tylko jeden atak zadający obrażenia, a pozostałe są skuteczne."
  :english
  "TODO")

(define-entry faq.general.208 ()
  :polish
  "Sieć dystansowa – Czy na jedną jednostkę można rzucić więcej niż jedną Sieć dystansową? Tak."
  :english
  "TODO")

(define-entry faq.general.209 ()
  :polish
  "Sieć dystansowa a Taniec – Czy podczas Tańca Obiekt, który wychodzi spod Sieci dystansowej, otrzymuje rany za każdy jej znacznik? Tak."
  :english
  "TODO")

(define-entry faq.general.210 ()
  :polish
  "Czy jednostki Death Breath trafiają do Puli Zombie, jeśli zginą od Żetonu natychmiastowego? Tak."
  :english
  "TODO")

(define-entry faq.general.211 ()
  :polish
  "Sieć dystansowa – Czy Vegas może użyć Sieci dystansowej? Nie."
  :english
  "TODO")

(define-entry faq.general.212 ()
  :polish
  "Burza piaskowa a Terror – Czy Terror blokuje zagranie Burzy piaskowej, skoro blokuje zagranie żetonu natychmiastowego Powrót powodującego położenie żetonu na planszy? Nie."
  :english
  "TODO")

(define-entry faq.general.213 ()
  :polish
  "Burza piaskowa a Przejęcie – Czy jeśli Burza piaskowa była zagrana na pole z Agitatorem albo na przejętą jednostkę, to skoro Burza piaskowo odcina działanie modułów, to czy po jej zdjęciu można ponownie obrócić właśnie ponownie przejmowaną jednostkę? Tak, można taką jednostkę ponownie obrócić, ewentualny obrót dokonywany jest w tym samym momencie co pozostałych jednostek przejętych podczas Bitwy."
  :english
  "TODO")

(define-entry faq.general.214 ()
  :polish
  "Burza piaskowa a Macki, Stalowa sieć – Czy znaczniki Macek lub Stalowej sieci są zdejmowane z jednostki z powodu położenia na niej żetonu Burzy piaskowej? Nie."
  :english
  "TODO")

(define-entry faq.general.215 ()
  :polish
  "Burza piaskowa a Macki – Czy znacznik Macek wraca do gracza, jeśli Burza piaskowa została zagrana na pole z jednostką mackującą, a na mackowaną jednostkę nie jest zwrócona mackami żadna inna jednostka? Tak."
  :english
  "TODO")

(define-entry faq.general.216 ()
  :polish
  "Burza piaskowa a Wchłanianie – Czy Wchłanianie może przywrócić punkt Wytrzymałośći Sztabowi, jeśli jest on pod wpływem Burzy piaskowej i w teorii jest wyłączony ze wszystkich działań podczas Bitwy? Nie."
  :english
  "TODO")

(define-entry faq.general.217 ()
  :polish
  "Burza piaskowa – Czy jeśli na jednostce leży żeton Burzy piaskowej, ale dzięki innym jednostkom otrzymuje dodatkowe zdolności i możliwości (np. dzięki Wszczepom Inkubatora i Kolca), to czy możne przeprowadzić takie działania? Nie, Burza piaskowa blokuje wszelkie działania wobec żetonu, nad którym się znajduje oraz jego wobec innych żetonów (poza wprost opisanymi wyjątkami), nawet te, które nie są nią przesłonięte, bo znajdują się na innych żetonach."
  :english
  "TODO")

(define-entry faq.general.218 ()
  :polish
  "Burza piaskowa – Czy znaczniki ran w liczbie co najmniej wyrównującą własną wytrzymałość jednostki w Burzy piaskowej powoduje jej zniszczenie, czy nie? Nie."
  :english
  "TODO")

(define-entry faq.general.219 ()
  :polish
  "Burza piaskowa – Czy skutki położenia żetonu Burzy piaskowej liczą się jakby zaszły w czasie Bitwy? Tak."
  :english
  "TODO")

(define-entry faq.general.220 ()
  :polish
  "Burza piaskowa a Ruchome piaski – Kiedy należy rozpatrzyć skutki położenia żetonu Burzy piaskowej na danym polu przed czy po wykonaniu ewentualnego ruchu Ruchomymi piaskami? Przed."
  :english
  "TODO")

(define-entry faq.general.221 ()
  :polish
  "Burza piaskowa a Ruchome piaski – Czy po zagraniu Burzy piaskowej można poruszyć oba żetony Ruchomych piasków, czy tylko jeden? Oba."
  :english
  "TODO")

(define-entry faq.general.222 ()
  :polish
  "Ruchome piaski – Czy jeśli w którymś z późniejszych segmentów inicjatywy na Ruchomych piaskach znajdzie się wroga jednostka (np. szarżujący Zombiak), czy jest niszczona? Nie."
  :english
  "TODO")

(define-entry faq.general.223 ()
  :polish
  "Ruchome piaski – Czy sojuszniczy Sztab może zniszczyć Ruchome piaski? Nie."
  :english
  "TODO")

(define-entry faq.general.224 ()
  :polish
  "Skanibalizowanie – Czy po udanym Skanibalizowaniu trzeba położyć znacznik Sytości, czy tylko można? Trzeba, jeśli jest taka możliwość."
  :english
  "TODO")

(define-entry faq.general.225 ()
  :polish
  "Skanibalizowanie – Czy jeśli w puli dostępnych znaczników Sytości nie ma znacznika, który może zostać położony w danej sytuacji, to czy można wtedy dokonać Skanibalizowania? Tak."
  :english
  "TODO")

(define-entry faq.general.226 ()
  :polish
  "Skanibalizowanie – Czy jeśli na jednostce zamierzającej dokonać Skanibalizowania jest znacznik Większej Sytości, ale podczas niego nie będą spełnione wymagania położenia takiego znacznika, to czy może go zatrzymać, czy jednak musi go wymienić na znacznik Mniejszej Sytości? Co, jeśli już takich znaczników nie ma w puli dostępnych? Musi wymienić na znacznik Mniejszej Sytości, w razie jego braku w puli dostępnych pozostanie bez jakiegokolwiek znacznika Sytości."
  :english
  "TODO")

(define-entry faq.general.227 ()
  :polish
  "Skanibalizowanie – Czy można Skanibalizować (jednostką posiadającą już znacznik Sytości) i wybrać jeszcze raz ten sam znacznik? Jeśli tak, to czy można po wybraniu Mięśni wykonać jeszcze raz z Mobilności, którą dają? Można (przy czym dla znacznika Większej Sytości muszą być spełnione normalne warunki jego położenia), ale nie można skorzystać powtórnie w jednej turze z Mobilności dawanej przez Mięśnie."
  :english
  "TODO")

(define-entry faq.general.228 ()
  :polish
  "Skanibalizowanie a Paraliż – Czy można Skanibalizować sparaliżowaną jednostką? Tak."
  :english
  "TODO")

(define-entry faq.general.229 ()
  :polish
  "Skanibalizowanie wroga a Odbicie – Czy Odbicie chroni przed Skanibalizowaniem wroga? Nie."
  :english
  "TODO")

(define-entry faq.general.230 ()
  :polish
  "Skanibalizowanie wroga a Medyk – Co w przypadku, gdy jednostka ze znacznikiem Sytości podejmie nieskuteczną próbę Skanibalizowania Wroga, to czy traci swój obecny znacznik Sytości? Nie, obecny znacznik pozostaje na niej."
  :english
  "TODO")

(define-entry faq.general.231 ()
  :polish
  "Skanibalizowanie wroga a Sieć – Czy jeśli sąsiadująca ze Sztabem Troglodytów jednostka Skanibalizuje wrogiego sieciarza sieciującego Sztab Troglodytów może skorzystać ze znaczników Większej Sytości? Nie."
  :english
  "TODO")

(define-entry faq.general.232 ()
  :polish
  "Skanibalizowanie a Przejęcie kontroli – Czy Vegas może Skanibalizować Przejętą przez siebie jednostkę Troglodytów inną Przejętą przez siebie jednostką z cechą Kanibalizm? Tak."
  :english
  "TODO")

(define-entry faq.general.233 ()
  :polish
  "Zamrożenie a Przejęcie kontroli – Czyje działanie ma większy priorytet, jeśli oddziałują na siebie wzajemnie? Przejęcie kontroli."
  :english
  "TODO")

(define-entry faq.general.234 ()
  :polish
  "Zamrożenie a Skoper – Czyje działanie ma większy priorytet, jeśli oddziałują na siebie wzajemnie? Żadne, ich wzajemnie działania się znoszą, ale normalnie"
  :english
  "TODO")

(define-entry faq.general.235 ()
  :polish
  "oddziałują w pozostałych kierunkach."
  :english
  "TODO")

(define-entry faq.general.236 ()
  :polish
  "Zamrożenie a Skoper – Wg polskiej instrukcji Skoper normalnie oddziałuje na Cechy Modułowe Frosta, natomiast w angielskiej uczyniono dla tego wojownika wyjątek, która wersja jest prawidłowa? Polska."
  :english
  "TODO")

(define-entry faq.general.237 ()
  :polish
  "Zamrożenia a Pułapka – Czy Zamrożenie wyłącza Pułapkę (zablokuję jej eksplozję) jak każdy inny moduł? Tak."
  :english
  "TODO")

(define-entry faq.general.238 ()
  :polish
  "Zamrożenie – Jak działają na siebie dwa wrogie wobec siebie Zamrożenia wzajemnie na siebie skierowane? Ich wzajemnie działania się znoszą, ale normalnie oddziałują w pozostałych kierunkach."
  :english
  "TODO")

(define-entry faq.general.239 ()
  :polish
  "Zamrożenie a Cecha Podziemny – Czy Zamrożenie oddziałuje na cechę Podziemny znajdującą się na Module, jak ma to miejsce w przypadku Skopera? Tak."
  :english
  "TODO")

(define-entry faq.general.240 ()
  :polish
  "Zamrożenie a Medyk dystansowy – Czy Zamrożenie oddziałuje na cechę Medyka dystansowego znajdującą się na Wojowniku? Nie."
  :english
  "TODO")

(define-entry faq.general.241 ()
  :polish
  "Skoper a Medyk dystansowy – Czy Skoper oddziałuje na cechę Medyka dystansowego znajdującą się na Wojowniku? Nie."
  :english
  "TODO")

(define-entry faq.general.242 ()
  :polish
  "Miotacz – Czy na Miotacza działają efekty zwiększające siłę ataku strzeleckiego lub ciosu? Nie."
  :english
  "TODO")

(define-entry faq.general.243 ()
  :polish
  "Miotacz – Czy można modyfikować atak Miotacza za pomocą modułów Kwatermistrza lub Transformatora Gaussa? Nie."
  :english
  "TODO")

(define-entry faq.general.244 ()
  :polish
  "Miotacz a Burza piaskowa – Czy Burza piaskowa blokuje linię strzału Miotaczowi, jeśli znajduje się na jednym z pól, między którymi przelatuje strzał? Nie."
  :english
  "TODO")

(define-entry faq.general.245 ()
  :polish
  "Miotacz a Pułapka – Czy trafienie strzałem Miotacza w róg Pułapki, Mad Bombera sąsiadujący z ikoną Pułapki powoduje eksplorację? Tak."
  :english
  "TODO")

(define-entry faq.general.246 ()
  :polish
  "Czy Frost, jeśli kieruje Zamrożenie na moduł Paszczy, a sam znajduje się przy ściance sztabu Mephisto bez kierunku ataku, a na Inkubatorze jesz znacznik Sieci, to czy Frost będzie zasieciowany? Nie."
  :english
  "TODO")

(define-entry faq.general.247 ()
  :polish
  "Lawina – Czy Lawina niszczy żetony Podłoża? Tak."
  :english
  "TODO")

(define-entry faq.general.248 ()
  :polish
  "Lawina – Czy wszystkie trzy pola obszaru rażenia Lawiny znajdujące się przy krawędzi mają być na jednej linii bocznej? Tak."
  :english
  "TODO")

(define-entry faq.general.249 ()
  :polish
  "Friendly Fire – Czy wszystkie efekty modyfikujące zwykłe Ciosy działają również na ten typ ataku? Tak. Jedyną różnicą pomiędzy Ciosem a Friendly Fire jest podatność sojuszniczych jednostek na taki Atak."
  :english
  "TODO")

(define-entry faq.general.250 ()
  :polish
  "Pajęcza Sieć (Sieć Friendly Fire) a Przejęcie Kontroli – Co się stanie, gdy jednostka z Pajęczą Siecią (Siecią FF) kieruję ją w stronę ścianki, z której działa na nią Przejęcie Kontroli? Jednostka z Pajęczą Siecią (Siecią FF) będzie Przejęta i nie będzie sieciować jednostki z Przejęciem Kontroli skierowanym w jej stronę."
  :english
  "TODO")

(define-entry faq.general.251 ()
  :polish
  "Paraliż a Agonia – Czy jeśli jednostka zostaje sparaliżowana na skutek ataku (cecha Paraliż), to czy może skorzystać z Agonii, która jest skutkiem tego ataku? Tak."
  :english
  "TODO")

(define-entry faq.general.252 ()
  :polish
  "Siedlisko a Sieć – Czy cecha Siedlisko działa, jak Cerber jest zasieciowany? Tak. Zasieciowany Cerber nadal musi sąsiadować ze Sztabem, aby pozostać na planszy."
  :english
  "TODO")

(define-entry faq.general.253 ()
  :polish
  "Siedlisko a Sieć – Czy cecha Siedlisko działa, jak Sztab Bestii jest zasieciowany? Tak. Zasieciowany Sztab nadal daje sąsiedztwo Cerberowi."
  :english
  "TODO")

(define-entry faq.general.254 ()
  :polish
  "Siedlisko a Burza piaskowa – Czy cecha Siedlisko działa, jak Sztab Bestii jest pod żetonem Burzy piaskowej? Nie. Sztab pod Burzą piaskową nie daje sąsiedztwa Cerberowi."
  :english
  "TODO")

(define-entry faq.general.255 ()
  :polish
  "Siedlisko a Przejęcie – Czy po przejęciu Cerbera zostaje on zniszczony, czy Agitator utrzymuje go przy życiu, jak zasila jednostki Uranopolis? Nie, Cerber nie zostanie zniszczony, cały czas utrzymuje go sąsiedztwo ze Sztabem Bestii."
  :english
  "TODO")

(define-entry faq.general.256 ()
  :polish
  "Siedlisko a Mina – Czy Cerber oddalający się od Sztabu Bestii wchodząc na pole z Miną, zdąży ją zdetonować, czy nie? Tak, zdąży ja zdetonować, zanim zostanie zniszczony z uwagi na oddalenie się od Sztabu Bestii."
  :english
  "TODO")

(define-entry faq.general.257 ()
  :polish
  "Siedlisko a Polowanie – Czy Cerber oddalający się od Sztabu Bestii na skutek Ruchu spowodowanego Polowaniem, zdąży zaatakować zanim zginie? Nie."
  :english
  "TODO")

(define-entry faq.general.258 ()
  :polish
  "Latanie a żetony Podłoża – Czy na jednostkę z cechą Latanie oddziałują normalnie żetony Podłoża i vice versa? Tak."
  :english
  "TODO")

(define-entry faq.general.259 ()
  :polish
  "Latanie a żetony Natychmiastowe – Czy na jednostkę z cechą Latanie oddziałują normalnie żetony Natychmiastowe? Tak."
  :english
  "TODO")

(define-entry faq.general.260 ()
  :polish
  "Latanie a cechy przemieszczające – Czy na jednostkę z cechą Latanie oddziałują cechy jednostek np. Odpychania, Przyciąganie? Tak."
  :english
  "TODO")

(define-entry faq.general.261 ()
  :polish
  "Latanie a Odłamki – Czy Odłamki zadają Obrażenia jednostce z cechą Latanie? Tak."
  :english
  "TODO")

(define-entry faq.general.262 ()
  :polish
  "Latanie a Cień – Czy atak Cienia (zdelokalizowany Cios) zada Obrażenia jednostce z cechą Latanie? Nie, chyba że dana jednostka jest zasieciowana."
  :english
  "TODO")

(define-entry faq.general.263 ()
  :polish
  "Latanie a Ładunek wybuchowy – Czy Ładunek wybuchowy niszczy jednostkę z cechą Latanie? Tak."
  :english
  "TODO")

(define-entry faq.general.264 ()
  :polish
  "Latanie a Zatrucie – Czy Zatrucie działa na jednostki z cechą Latanie? Tak."
  :english
  "TODO")

(define-entry faq.general.265 ()
  :polish
  "Latanie a Egzekutor – Czy Egzekutor może skorzystać ze swojej zdolności specjalnej na jednostce z cechą Latanie? Tak."
  :english
  "TODO")

(define-entry faq.general.266 ()
  :polish
  "Latanie a Łańcuch – Czy Łańcuch zadaje Obrażenie jednostce z cechą Latanie? Tak."
  :english
  "TODO")

(define-entry faq.general.267 ()
  :polish
  "Latanie a Kolec – Czy można na jednostce z cechą Latanie położyć znacznik Kolca? Nie, chyba że jednostka z tą cechą jest zasieciowana. Sieć przenoszona przez dany Kolec nie ma wpływu na możliwość położenia odpowiadającego mu znacznika Kolca."
  :english
  "TODO")

(define-entry faq.general.268 ()
  :polish
  "Latanie a Macki – Czy można na jednostce z cechą Latanie położyć znacznik Macek? Tak."
  :english
  "TODO")

(define-entry faq.general.269 ()
  :polish
  "Latanie a Wyburzacz – Czy atak Wyburzacza oddziałuje na jednostki z cechą Latanie? Tak."
  :english
  "TODO")

(define-entry faq.general.270 ()
  :polish
  "Latanie a Skanibalizowanie – Czy można Skanibalizować jednostkę z cechą Latanie? Tak."
  :english
  "TODO")

(define-entry faq.general.271 ()
  :polish
  "Padlinożerca – Czy podczas przenoszenia jednostki z cechy Padlinożerca można dokonać nią Obrót? Tak."
  :english
  "TODO")

(define-entry faq.general.272 ()
  :polish
  "Padlinożerca – Czy jednostkę z cechą Padlinożerca można Obrócić bez przenoszenia jej na inne pole? Tak."
  :english
  "TODO")

(define-entry faq.general.273 ()
  :polish
  "Padlinożerca a znacznik Rany – Czy można przemieścić jednostkę z cechą Padlinożerca na pole ze znacznikiem Rany, jeśli tak, to co się z nim dzieje? Można, jest zdejmowany z planszy."
  :english
  "TODO")

(define-entry faq.general.274 ()
  :polish
  "Padlinożerca a żeton Podłoża – Czy można przemieścić jednostkę z cechą Padlinożerca na pole z żetonem Podłoża? Tak."
  :english
  "TODO")

(define-entry faq.general.275 ()
  :polish
  "Polowanie – Czy atak musi nastąpić ze ścianki „wskazanej” na żetonie, czyli przeciwległej do tej, z której nastąpiło przemieszczenie czy z wybranej przez gracza? Nie, z wybranej."
  :english
  "TODO")

(define-entry faq.general.276 ()
  :polish
  "Przyciągnięcie a Przejęcie – Kto decyduje, jeśli jest wybór, na które pole zostanie Przyciągnięta i jak Obrócona jednostka, która jest pod wpływem Przejęcia kontroli? Wyboru dokonuje ten z graczy, który sprawuje kontrolę nad daną jednostką w momencie inicjacji Przyciągnięcia."
  :english
  "TODO")

(define-entry faq.general.277 ()
  :polish
  "Pola Wodne a żetony Podłoża – Czy na Polach Wodnych mogą znajdować się żetony Podłoża? Nie."
  :english
  "TODO")

(define-entry faq.general.278 ()
  :polish
  "Pola Wodne a Promień – Jak działa Promień w przypadku gry z Polami Wodnymi? Normalnie, obejmuje jedną linie na planszy, zarówno taką zawierającą Pola Zwykłe i Pola Wodne oraz taką zadziwiającą tylko Pola Wodne (w tym przypadku dwa pola)."
  :english
  "TODO")

(define-entry faq.general.279 ()
  :polish
  "Pola Wodne a Lawina – Jak działa Lewina w przypadku gry z Polami Wodnymi? Normalnie, jej zasięg to trzy pola na jednej krawędzi podstawowego obszaru gry oraz dwa pola na wewnętrznym kręgu podstawowego obszaru gry."
  :english
  "TODO")

(define-entry faq.general.280 ()
  :polish
  "Pola Wodne a Sieć Dystansowa – Czy skoro Stalowa Sieć nie działa na Pola Wodne, to czy można na nich użyć Sieci Dystansowej? Tak."
  :english
  "TODO")

(define-entry faq.general.281 ()
  :polish
  "Pola Wodne a Macki – Czy skoro Stalowa Sieć nie działa na Pola Wodne, to czy można na nich użyć Macek? Tak."
  :english
  "TODO")

(define-entry faq.general.282 ()
  :polish
  "Pola Wodne a Latanie – Czy jednostka z cecha Latanie może znajdować się na Polu Wodnym? Nie."
  :english
  "TODO")

(define-entry faq.general.283 ()
  :polish
  "Pola Wodne a Egzekutor – Czy Egzekutor może dokonać egzekucji na Polu Wodnym, a następnie sam być zdjęty z planszy? Nie."
  :english
  "TODO")

(define-entry faq.general.284 ()
  :polish
  "Pola ze znacznikami Sąsiedztwa Wodnego a Wyrzutnia rakiet – Czy Wyrzutnia rakiet może wystrzelić na pole ze znacznikiem Sąsiedztwa Wodnego (z innej strony niż Pola Wodnego)? Nie."
  :english
  "TODO")

(define-entry faq.general.285 ()
  :polish
  "Pola ze znacznikami Sąsiedztwa Wodnego a Wyrzutnia rakiet – Czy wystrzelona rakieta może przemieszczać się przez pola ze znacznikami Sąsiedztwa Wodnego (ze strony Pola Wodnego), jeśli tak to czy są one wliczane do limitu jej zasięgu? Może i nie są wliczane do jej limitu zasięgu."
  :english
  "TODO")

(define-entry faq.general.286 ()
  :polish
  "Pola ze znacznikami Sąsiedztwa Wodnego a Sieciowanie – Czy skoro pola połączone znacznikiem Sąsiedztwa Wodnego są traktowana jako sąsiednie, to czy sieciowanie działa pomiędzy nimi, jakby sąsiadowały normalnie odpowiednimi ściankami? Tak."
  :english
  "TODO")

(define-entry faq.general.287 ()
  :polish
  "Pola ze znacznikami Sąsiedztwa Wodnego a Strzały – Czy skoro pola połączone znacznikiem Sąsiedztwa Wodnego są traktowana jako sąsiednie, to czy Strzały atakują pomiędzy nimi, jakby sąsiadowały normalnie odpowiednimi ściankami? Tak, przy czym należy pamiętać, że Strzały mogą skorzystać z więcej niż jednego takiego sąsiedztwa."
  :english
  "TODO")

(define-entry faq.general.288 ()
  :polish
  "Pola ze znacznikami Sąsiedztwa Wodnego a Ciosy – Czy skoro pola połączone znacznikiem Sąsiedztwa Wodnego są traktowana jako sąsiednie, to czy Ciosy atakują pomiędzy nimi, jakby sąsiadowały normalnie odpowiednimi ściankami? Tak."
  :english
  "TODO")

(define-entry faq.general.289 ()
  :polish
  "Dryf – Czy zakaz poruszania jednostek z cechą Dryf (pozav Dryfem) dotyczy także negatywnych efektów żetonów Natychmiastowych przeciwnika w tym Obrotów (np. Zasłona"
  :english
  "TODO")

(define-entry faq.general.290 ()
  :polish
  "dymna)? Tak."
  :english
  "TODO")

(define-entry faq.general.291 ()
  :polish
  "Dryf a Przejęcie kontroli – Czy w momencie Przejęcia kontroli nad jednostką z cechą Dryf można ją Obrócić? Tak."
  :english
  "TODO")

(define-entry faq.general.292 ()
  :polish
  "Dryf a Przejęcie kontroli – Czy jeśli jednostka jest Przejęta, to czy otrzymuje Obrażenie podczas Dryfu, jak podczas Uwolnienia? Nie. Przejęta jednostka podczas Dryfu normalnie się przemieszcza, o jej Obrocie decyduje gracz posiadający nad nią kontrole w momencie rozpoczęcia rozpatrywania Dryfu."
  :english
  "TODO")

(define-entry faq.general.293 ()
  :polish
  "Dryf a Skanibalizowanie wroga – Czy można Skanibalizować wrogą jednostkę z cechą Dryf? Nie."
  :english
  "TODO")

(define-entry faq.general.294 ()
  :polish
  "Dryf a Wchłanianie (Pożarcie Death Breath) – Czy można Wchłonąć wrogą jednostkę z cechą Dryf? Tak."
  :english
  "TODO")

(define-entry faq.general.295 ()
  :polish
  "Abordaż – Czy można skorzystać z tej cechy gdy jednostka ją posiadająca jest zasieciowana? Nie."
  :english
  "TODO")

(define-entry faq.general.296 ()
  :polish
  "Abordaż – Czy można skorzystać z tej cechy gdy jednostka ją posiadająca jest Przejęta? Nie."
  :english
  "TODO")

(define-entry faq.general.297 ()
  :polish
  "Abordaż – Czy można skorzystać z tej cechy gdy jednostka ją posiadająca jest sparaliżowana? Tak."
  :english
  "TODO")
