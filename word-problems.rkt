#lang racket

;;; ABC-sort problem
;;; ================================================================

;;; v4.2.2

(require "misc.rkt")

(define *dictionary*
  '(*handle* ability able above accepted access acid added address adult advice after age
             ahead airplane airport alien alive alone amount angle angry apple apply
             away awesome
             babe baby babies ball ballet banana barn basket battery beast bee beef
             belly big bill birds blanket block blood bomb bone boss boy brain brake
             brick broad brother bug busy
             cafe cage cake calm candy cap card cash cell chair change chart chat chick
             circle circus citizen clean click climb club colon column comedy common
             compile comply contact cool corn cradle crowd
             dam dame data date day deal dear deck define dense desert desk dial dinner
             dirty disc dive dock doll dome doom door drive duck dust
             eagle ear earn ease echo economy edge edit editor effect egg elegant
             element eleven elite empty enemy engine equal equation error event
             execute exempt exit expert
             fact factor false family farm fast fear feeling fellow fever final fine
             fire fish flag flexible flower follow fool force fox free fresh frog future
             gain game gang garage garden ghost girl glow gold grain grammar granny
             grass gun guy gym garlic gender gentle giant glad global gloves good
             hammer hand happy harbor hate head health hello hidden hide hotel hour
             house howto how huge hull human hunter husband hunt 
             ice icon idea idle image impact income infant ink into isle issue
             ideal identical identity ignore immune implied import impose issued
             inbox inform informal initial injured injury inner iron irrigation issues 
             jeep jet joke join july just justify jade jaguar jeans jersey job joint
             journal journey joy judge jump jungle juvenile jury
             keep keyboard kick king know keeping keeps keys key killed killer kingdom
             knight knife knock keyword knowledge 
             lady lamb land laser lazy leader leave lecture legend level library life
             lock logic look loss love lung
             machine made magic magnet male manner maybe meal meat middle might model mold
             moon mother movie museum
             name neck nail never nobody nose nude nurse
             oak oasis object occur occurs old once onion organ organic origin our oxygen
             page pain paint palm panel paper parent penny people permit pet phone pillow
             plant plate police pork poster pound pray price puppy push puzzle python
             quad quarter question queue quality queen query quest quick quiet quiz quote
             race rally random ratio reach real record reject rent result review rich ride
             rise road rock roof room rush
             safe sale salt sandy scan school score scroll search secret seed senate set
             shade shame ship show silly sister sleep snow solution sorry spell state
             teen test text thread throw ticket today tooth train trash tree trust truth
             tube tunnel type
             unlock urban upset urgent utility union unit united unless unwrap update
             use used user using utilize 
             vaccine valve variable vendor verbal victim village violin vote vacuum
             value vampire variety vast vegetable verify version video viral
             walker wallet wanted watch weak wedding whale while willow wisdom wizard
             week weekly welcome wheel which wife wine winner
             year yeast yellow yield yoga yarn yesterday youth yours young
             zero zinc zone zoo zoom zip bookend
             ))

;;; ========================================================================
;;; Text problems
;;; ========================================================================

(define t-func (lambda x #t))

(define word+problems1 ; addition only
  (list
   (list "There are ~a birds sitting on a tree, and then ~a more birds fly in.~n~n\
 How many birds are sitting on a tree now?"
         '((30 125) (20 80))
         t-func
         (lambda (a b)
           (let ((formula `(,a + ,b)))
             (values formula formula))))
           
   (list "There are ~a cars, and ~a motorcycles on a parking lot.~n~n\
 How many vehicles are there altogether?"
         '((500 2000) (700 2000))
         t-func
         (lambda (a b)
           (let ((formula `(,a + ,b)))
             (values formula formula))))
          
   (list "Marco has ~a cookies, Tina has ~a cookies, and Ana has ~a cookies.~n~n\
 How many cookies do they have altogether?"
         '((30 125) (30 125) (30 125))
         t-func
         (lambda (a b c)
           (let ((formula `(,a + ,b + ,c)))
             (values formula formula))))
   
   (list "A pirate has found a treasure chest. Inside are ~a gold coins, ~a \
silver coins, and ~a copper coins.~n~n How many coins has he found altogether?"
         '((500 1000) (700 1500) (1000 2000))
         t-func
         (lambda (a b c)
           (let ((formula `(,a + ,b + ,c)))
             (values formula formula))))
   
   (list "Jasna has ~a php in her purse. Ken has ~a php in his wallet.~n~n\
 How much money do they have altogether?"
         '((1000 2000) (500 1200))
         t-func
         (lambda (a b)
           (let ((formula `(,a + ,b)))
             (values formula formula))))
   
   (list "Lila has ~a dollars in her wallet, Ben has ~a dollars, and Symone has ~a \
dollars in her purse.~n~n How much money do they have altogether?"
         '((100 1000) (100 1000) (100 1000))
         t-func
         (lambda (a b c)
           (let ((formula `(,a + ,b + ,c)))
             (values formula formula))))
   
   (list "Our town has four elementary schools. There are ~a pupils in school A, \
~a pupils in school B, ~a pupils in school C, and ~a pupils in school D.~n~n\
 How many pupils are there altogether in our town?"
         '((1000 2000) (600 1500) (700 2000) (500 1000))
         t-func
         (lambda (a b c d)
           (let ((formula `(,a + ,b + ,c + ,d)))
             (values formula formula))))
   
   (list "Father went to the shop and he bought bread for ~a php, cheese for \
~a php, a carton of eggs for ~a php, and a bottle of wine for ~a php.~n~n How much \
money did he spend altogether?"
         '((80 120) (800 2000) (120 220) (350 600))
         t-func
         (lambda (a b c d)
           (let ((formula `(,a + ,b + ,c + ,d)))
             (values formula formula))))
   
   (list "Our family went fishing. Father caught ~a fish, my brother caught ~a \
fish, and I caught ~a fish.~n~n How many fish did we catch altogether?"
         '((10 30) (15 30) (20 35))
         t-func
         (lambda (a b c)
           (let ((formula `(,a + ,b + ,c)))
             (values formula formula))))
   
   (list "I had ~a coins. Karina gave me ~a coins, Marcus gave me ~a coins, and \
Lily gave me ~a coins more.~n~n How many coins do I have now?"
         '((10 30) (20 100) (30 100) (20 120))
         t-func
         (lambda (a b c d)
           (let ((formula `(,a + ,b + ,c + ,d)))
             (values formula formula))))
   
   (list "Jasna's family went to a restaurant. Mother ordered pizza \
for ~a dollars. Father ordered hamburger for ~a dollars, \
and Jasna ordered tuna pasta for ~a dollars.~n~n How much money did they spend?"
         '((30 50) (10 20) (15 30))
         t-func
         (lambda (a b c)
           (let ((formula `(,a + ,b + ,c)))
             (values formula formula))))
   
   (list "Our school played basketball with another school. \
We scored ~a points, and another school scored ~a points.~n~n\
 How many points were scored altogether?"
         '((40 80) (30 70))
         t-func
         (lambda (a b)
           (let ((formula `(,a + ,b)))
             (values formula formula))))
   
   (list "A mango tree has 4 branches. On the first branch, there are ~a green leaves. \
On second, there are ~a green leaves. On third, there are ~a yellow leaves, \
and on fourth, there are ~a green leaves.~n~n How many green leaves are there on a tree?"
         '((100 1000) (200 1000) (500 1000) (100 1000))
         t-func
         (lambda (a b _ d)
           (let ((formula `(,a + ,b + ,d)))
             (values formula formula))))
   
   (list "There are ~a trucks, ~a motorcycles, ~a cars, and ~a tricycles \
on a parking lot.~n~n How many four-wheel vehicles are there altogether?"
         '((200 1200) (300 2000) (250 1200) (50 250))
         t-func
         (lambda (a _b c _d)
           (let ((formula `(,a + ,c)))
             (values formula formula))))
   
   (list "There were ~a girls and ~a boys present at the start of a school \
celebration. Then ~a boys, and ~a girls joined in.~n~n How many boys were there \
at the celebration?"
         '((200 500) (300 600) (100 300) (200 500))
         t-func
         (lambda (_a b c _d)
           (let ((formula `(,b + ,c)))
             (values formula formula))))
   
   (list "The famous explorer, Indiana Jones found two jars filled with gems in a lost \
Mayan city. First jar contained ~a rubies, ~a emeralds, and ~a diamonds. \
Second jar contained ~a diamonds, ~a rubies, and ~a emeralds.~n~n\
 How many emeralds did he find altogether?"
         '((50 150) (60 200) (20 50) (30 50) (50 100) (50 200))
         t-func
         (lambda (_a b _c _d _e f)
           (let ((formula `(,b + ,f)))
             (values formula formula))))
   
   (list "KTM completed a three day rally with these times: \
~a, ~a, and ~a minutes. Honda completed the same rally with these times: ~a, ~a, \
but failed to complete the third day. Yamaha completed with these times: \
~a, ~a, and ~a minutes.~n~n What was the total time in minutes, of all teams \
that completed the rally?"
         '((120 150) (130 180) (125 160) (140 170) (140 190) (130 160) (140 200) (130 165))
         t-func
         (lambda (a b c _d _e f g h)
           (let ((formula `(,a + ,b + ,c + ,f + ,g + ,h)))
             (values formula formula))))

   (list "We went on a weekend trip from our home to Sorsogon. We drove ~a kilometers to \
Guinobatan, then we drove ~a kilometers more to Legazpy city, and finally we drove ~a \
kilometers to Irosin. We had dinner there, and then we drove ~a kilometers back to \
Legazpy city.~n~n How many kilometers did we drive today?"
         '((60 95) (20 30) (120 150) (120 150))
         (lambda (a b c d) (= c d))
         (lambda (a b c d)
           (let ((formula `(,a + ,b + ,c + ,d)))
             (values formula formula))))
   ))

(define word+problems2
  (list
   (list "A florist shop sold ~a orchids, ~a roses, and ~a carnations.~n~n \
How many flowers did they sell altogether?"
         '((10 40) (20 50) (15 60))
         t-func
         (lambda (a b c)
           (let ((formula `(,a + ,b + ,c)))
             (values formula formula))))
   
   (list "Clara has ~a cards. Lisa has ~a more cards than Clara. Ariel has \
~a more cards than Lisa.~n~n \
How many cards does Ariel have?"
         '((50 150) (20 50) (40 70))
         t-func
         (lambda (a b c)
           (let ((formula `(,a + ,b + ,c)))
             (values formula formula))))

   (list "Garry has ~a seashells. His sister has ~a seashells more than he \
does.~n~n How many seashells do they have altogether?"
         '((100 250) (50 200))
         t-func
         (lambda (a b)
           (let ((formula `(,a + ,b + ,a)))
             (values formula formula))))

   (list "Grandmother baked some cookies. She gave ~a cookies to my mother, \
and ~a cookies to her neighbours. She had ~a cookies left.~n~n \
How many cookies did she bake?"
         '((100 250) (200 500) (50 100))
         t-func
         (lambda (a b c)
           (let ((formula `(,a + ,b + ,c)))
             (values formula formula))))
   ))

(define word-problems2
  (list
   (list "Ivan puts ~a cookies into boxes A and B. If there are ~a cookies in \
box A, how many cookies are there in box B?"
         '((10 20) (5 10))
         t-func
         (lambda (a b)
           (let ((formula `(,a - ,b)))
             (values formula formula))))

   (list "Jasna had ~a playing cards. She gave ~a cards to her friend Elise, \
and ~a cards to me.~n~n How many cards did she have left?"
         '((160 250) (30 75) (30 75))
         t-func
         (lambda (a b c)
           (let ((formula `(,a - ,b - ,c)))
             (values formula formula))))

   (list "Zia got a ~a page book as a present from her mother. She read ~a \
pages on the first day, and ~a pages on the second day.~n~n \
How many more pages does she have to read to finish the book?"
         '((300 450) (100 150) (80 120))
         t-func
         (lambda (a b c)
           (let ((formula `(,a - ,b - ,c)))
             (values formula formula))))

   (list "There were ~a persons at the movie theater. ~a were men, and \
~a were women. The rest were children.~n~n \
How many children were there?"
         '((180 250) (40 75) (50 75))
         t-func
         (lambda (a b c)
           (let ((formula `(,a - ,b - ,c)))
             (values formula formula))))

   (list "Rachel collected ~a stamps. Frank collected ~a fewer stamps than \
Rachel. Charmien collected ~a fewer than Frank.~n~n \
How many stamps did Charmien collect?"
         '((250 300) (40 80) (40 80))
         t-func
         (lambda (a b c)
           (let ((formula `(,a - ,b - ,c)))
             (values formula formula))))

   (list "There are ~a buttons in the box. ~a of then are round, and the rest \
are square.~n~n How many more square buttons than round ones are there?"
         '((500 700) (180 230))
         t-func
         (lambda (a b)
           (let ((formula `(,a - ,b - ,b)))
             (values formula formula))))
   ))

(define word+-problems2
  (list
   (list "A fruit vendor sold ~a mangoes on Sunday. He sold ~a fewer mangoes \
on Saturday than on Sunday.~n~n \
How many mangoes did he sell on both days?"
         '((300 500) (50 100))
         t-func
         (lambda (a b)
           (let ((formula `(,a - ,b + ,a)))
             (values formula formula))))

   (list "There were ~a girls, and ~a fewer boys.~n~n \
How many children were there altogether?"
         '((100 150) (20 40))
         t-func
         (lambda (a b)
           (let ((formula `(,a - ,b + ,a)))
             (values formula formula))))

   (list "Rope A is ~a cm long. Rope B is ~a cm longer than rope A. \
Rope C is ~a cm shorter than rope B.~n~n \
How long is rope C?"
         '((200 350) (50 100) (70 120))
         t-func
         (lambda (a b c)
           (let ((formula `(,a + ,b - ,c)))
             (values formula formula))))

   (list "Jasna Elise, and Symone went collecting seashells on the beach. \
Jasna collected ~a more shells than Elise. Elise collected ~a shells fewer than \
Symone. Symone collected ~a shells.~n~n \
How many shells did Jasna collect?"
         '((10 25) (5 15) (20 30))
         t-func
         (lambda (a b c)
           (let ((formula `(,c - ,b + ,a)))
             (values formula formula))))

   (list "Father is thinking of two numbers. The smaller one is ~a. \
Their difference is ~a.~n~n What is the sum of these two numbers?"
         '((60 100) (30 50))
         t-func
         (lambda (a b)
           (let ((formula `(,a + ,b + ,a)))
             (values formula formula))))
   ))
  
(define word-problems1 ; subtraction only
  (list
   (list "There are ~a birds sitting on a tree. ~a birds fly away.~n~n\
 How many birds are left sitting on a tree?"
         '((30 120) (10 70))
         t-func
         (lambda (a b)
           (let ((formula `(,a - ,b)))
             (values formula formula))))
   
   (list "Karen is holding ~a coins in her hand. She stumbles and loses ~a coins.~n~n\
 How many coins does she have left?"
         '((20 50) (10 30))
         t-func
         (lambda (a b)
           (let ((formula `(,a - ,b)))
             (values formula formula))))
   
   (list "Samantha has ~a php. She gives ~a to Lucy and ~a to Danilo.~n~n\
 How much money has she got left?"
         '((800 2000) (100 1000) (50 700))
         (lambda (a b c) (> a b))
         (lambda (a b c)
           (let ((formula `(,a - ,b - ,c)))
             (values formula formula))))
   
   (list "We caught ~a fish, but ~a escaped. Then we gave ~a fish to our \
neighbors.~n~n How many do we have left?"
         '((30 100) (20 40) (10 30))
         (lambda (a b c) (> a b))
         (lambda (a b c)
           (let ((formula `(,a - ,b - ,c)))
             (values formula formula))))
   
   (list "Marina has ~a dollars in her purse. She bought groceries for ~a \
dollars, fruit for ~a dollars, and milk for ~a dollars. The rest of the money, \
she gave to her mother.~n~n How much money did she give to her mother?"
         '((200 500) (30 60) (20 40) (10 30))
         t-func
         (lambda (a b c d)
           (let ((formula `(,a - ,b - ,c - ,d)))
             (values formula formula))))
   
   (list "There are ~a goats in the stable. ~a jumped over the fence, and ~a \
run away through the open door.~n~n How many goats are left in the stable?"
         '((50 120) (20 50) (10 20))
         t-func
         (lambda (a b c)
           (let ((formula `(,a - ,b - ,c)))
             (values formula formula))))
   
   (list "~a ducks started swimming across the river. ~a ducks were eaten by \
crocodiles, and ~a ducks turned around and swam back.~n~n How many ducks arrived to \
the other side?"
         '((70 150) (20 40) (10 30))
         t-func
         (lambda (a b c)
           (let ((formula `(,a - ,b - ,c)))
             (values formula formula))))
   
   (list "Nicole collected ~a pebbles on the beach. She lost ~a pebbles running to the \
hotel, and gave ~a to her friend Zia.~n~n How many pebbles does she have left?"
         '((50 100) (10 30) (20 40))
         t-func
         (lambda (a b c)
           (let ((formula `(,a - ,b - ,c)))
             (values formula formula))))
   
   (list "Our cat Marla, caught ~a mice. She eat ~a for breakfast, and the rest run\
 away.~n~n How many mice did run away?"
         '((10 25) (2 8))
         t-func
         (lambda (a b)
           (let ((formula `(,a - ,b)))
             (values formula formula))))
   
   (list "We found a wallet containing ~a php. I bought an ice-cream for \
~a php, and my brother bought a chocolate bar for ~a php. \
The rest of the money from the wallet, we gave to our parents.~n~n\
 How much money did we give to our parents?"
         '((1000 2000) (100 200) (150 300))
         t-func
         (lambda (a b c)
           (let ((formula `(,a - ,b - ,c)))
             (values formula formula))))
   
   (list "There was a birthday celebration in our class. We had ~a pieces of \
candy. We gave ~a pieces to class B, ~a pieces to class C, and ~a pieces to \
class D.~n~n How many pieces of candy do we have left?."
         '((1000 2000) (200 300) (200 300) (200 400))
         t-func
         (lambda (a b c d)
           (let ((formula `(,a - ,b - ,c - ,d)))
             (values formula formula))))
   
   (list "Mayla picked ~a mangoes from a mango tree. She gave ~a to her \
classmates, and carried home ~a to her family. She ate all the rest.~n~n\
 How many mangoes did she eat?"
         '((60 120) (30 50) (20 50))
         t-func
         (lambda (a b c)
           (let ((formula `(,a - ,b - ,c)))
             (values formula formula))))
   
   (list "Jasna's parents gave her ~a dollars for amusement park. She had a Ferris \
wheel ride for ~a dollars, then bumble bee ride for ~a dollars, and finally, bumper \
to bumper ride for ~a dollars.~n~n How much money does she have left?"
         '((200 300) (20 30) (10 25) (15 30))
         t-func
         (lambda (a b c d)
           (let ((formula `(,a - ,b - ,c - ,d)))
             (values formula formula))))
   
   (list "Jerry has ~a dollars, but he owes Nina ~a dollars. He pays her back, \
and then buys a toy for ~a dollars.~n~n How many dollars has he got left?"
         '((1000 2000) (500 800) (100 250))
         t-func
         (lambda (a b c)
           (let ((formula `(,a - ,b - ,c)))
             (values formula formula))))
   
   (list "David has ~a pieces of candy. He eats ~a pieces, then gives ~a pieces \
to his son. If he wants to eat ~a more,~n~n how many pieces can he give to Jagoda?"
         '((100 150) (20 30) (20 40) (10 20))
         t-func
         (lambda (a b c d)
           (let ((formula `(,a - ,b - ,c - ,d)))
             (values formula formula))))
           
   (list "During swimming competition, Mia swam breaststroke in ~a \
seconds, and freestyle stroke in ~a seconds, while Lucy swam the same in \
~a seconds, and ~a seconds.~n~n How many seconds was Mia slower in freestyle stroke?"
         '((55 80) (50 70) (50 70) (40 60))
         (lambda (_a b _c d) (> b d))
         (lambda (_a b _c d)
           (let ((formula `(,b - ,d)))
             (values formula formula))))
   
   (list "During a 400 meters race on a race track, Dario finished in ~a seconds, \
while Flavio finished in ~a seconds.~n~n How many seconds was Dario faster than Flavio?"
         '((120 150) (130 170))
         t-func
         (lambda (a b)
           (let ((formula `(,b - ,a)))
             (values formula formula))))
   
   (list "During a MotoGP race Ducati had a fastest lap of ~a seconds, \
Yamaha had a fastest lap of ~a seconds, while Honda had a fastest \
lap of ~a seconds.~n~n How many seconds was Ducati faster than Honda?"
         '((99 120) (105 130) (110 135))
         t-func
         (lambda (a _ c)
           (let ((formula `(,c - ,a)))
             (values formula formula))))
   
   (list "During a MotoGP race Ducati had a fastest lap of ~a seconds, \
Yamaha had a fastest lap of ~a seconds, while Honda had a fastest \
lap of ~a seconds.~n~n How many seconds was Ducati faster than Yamaha?"
         '((99 120) (110 135) (105 130))
         t-func
         (lambda (a b _)
           (let ((formula `(,b - ,a)))
             (values formula formula))))
   ))

(define word+-problems1 ; addition & subtraction
  (list
   (list "Cristina has ~a cookies, and Jasna has ~a cookies. They give ~a \
cookies to Gerry.~n~n How many cookies do Cristina and Jasna have left altogether?"
         '((20 50) (30 50) (40 50))
         t-func
         (lambda (a b c)
           (let ((formula `(,a + ,b - ,c)))
             (values formula formula))))
   
   (list "Tina has ~a cookies, and Jasna has ~a cookies. Tina gives ~a \
cookies to Gerry, and Jasna gives ~a cookies to Danny.~n~n How many cookies do \
Cristina and Jasna have left altogether?"
         '((20 50) (30 60) (10 40) (20 50))
         (lambda (a b c d) (and (>= a c) (>= b d)))
         (lambda (a b c d)
           (let ((formula `(,a - ,c + ,b - ,d)))
             (values formula formula))))
   
   (list "We are hired to count customers entering and leaving a supermarket. ~a \
customers enter, then ~a leave, then ~a enter, and finally ~a leave.~n~n\
 How many customers are left in the supermarket now?"
         '((300 1000) (100 300) (200 500) (200 700))
         t-func
         (lambda (a b c d)
           (let ((formula `(,a - ,b + ,c - ,d)))
             (values formula formula))))
   
   (list "There are ~a vehicles on a parking lot. ~a vehicles drive away, \
then ~a more drive away. Then ~a drive in, and finally ~a drive away.~n~n\
 How many vehicles are left on the parking lot?"
         '((500 2000) (200 500) (100 300) (300 1000) (200 1000))
         (lambda (a b c d e) (>= a (+ b c)))
         (lambda (a b c d e)
           (let ((formula `(,a - ,b - ,c + ,d - ,e)))
             (values formula formula))))
   
   (list "Myra gets ~a php from her father. Mother gives her ~a php more. \
Then Myra buys a new tablet for ~a php.~n~n How much money has she got left?"
         '((1000 2000) (1000 2000) (2000 3000))
         t-func
         (lambda (a b c)
           (let ((formula `(,a + ,b - ,c)))
             (values formula formula))))
   
   (list "In our mall, there are three levels of parking. On level 1 \
there are ~a cars. On level 2 there are ~a cars, and on level 3 there are ~a cars. \
Then, ~a cars exit the mall.~n~n How many cars are left parked altogether?"
         '((500 1000) (500 1000) (500 1000) (1000 1500))
         t-func
         (lambda (a b c d)
           (let ((formula `(,a + ,b + ,c - ,d)))
             (values formula formula))))
   
   (list "There are three floors in our school. ~a pupils are on the first floor, \
~a pupils are on second, and ~a pupils are on the third. ~a pupils from first, \
and ~a pupils from second went home.~n~n How many pupils are left on first \
and second floors altogether?"
         '((300 500) (300 500) (200 500) (100 300) (100 300))
         t-func
         (lambda (a b c d e)
           (let ((formula `(,a - ,d + ,b - ,e)))
             (values formula formula))))
   
   (list "Mother bought ~a apples, and ~a oranges. My brother eat ~a apples, and \
~a oranges.~n~n How many pieces of fruit are left altogether?"
         '((11 50) (11 40) (5 11) (4 11))
         t-func
         (lambda (a b c d)
           (let ((formula `(,a - ,c + ,b - ,d)))
             (values formula formula))))
   
   (list "While playing basketball with Emma, we shot the ball ~a times, \
and then later again ~a times more. I scored ~a times, while Emma scored ~a times.~n~n\
 How many times did we miss altogether?"
         '((30 75) (30 75) (10 30) (10 30))
         t-func
         (lambda (a b c d)
           (let ((formula `(,a + ,b - ,c - ,d)))
             (values formula formula))))
   
   (list "When climbing the stairs, starting on step ~a, Ely went ~a \
steps up, then ~a steps down, then ~a steps up, then again ~a steps up, and finally \
~a steps down.~n~n On what step did he end up?"
         '((10 20) (20 30) (20 30) (10 30) (10 20) (10 20))
         t-func
         (lambda (a b c d e f)
           (let ((formula `(,a + ,b - ,c + ,d + ,e - ,f)))
             (values formula formula))))
   
   (list "We had ~a goats, ~a pigs, and ~a chicken in the yard. Then we sold ~a \
animals.~n~n How many animals have we got left altogether?"
         '((15 30) (10 30) (20 40) (30 60))
         t-func
         (lambda (a b c d)
           (let ((formula `(,a + ,b + ,c - ,d)))
             (values formula formula))))
   
   (list "Bob went to a birthday party. There were ~a boys there, including him, \
and ~a girls. Later ~a girls, and ~a boys went home.~n~n How many children were \
left at the party?"
         '((20 50) (20 50) (15 30) (15 30))
         (lambda (a b c d) (and (>= a d) (>= b c)))
         (lambda (a b c d)
           (let ((formula `(,a - ,d + ,b - ,c)))
             (values formula formula))))
   
   (list "We took a bus trip. ~a passengers boarded the bus with us. Then \
~a passengers got off, and ~a got on, and then finally, ~a more got on.~n~n\
 When we left the bus, how many passengers were still on the bus?"
         '((30 40) (20 35) (10 20) (15 30))
         (lambda (a b c d) (>= a b))
         (lambda (a b c d)
           (let ((formula `(,a - ,b + ,c + ,d)))
             (values formula formula))))
   
   (list "Jasna got ~a dollars for amusement park. She took a giant wheel ride for \
~a dollars, and a plane ride for ~a dollars. Elsa gave her ~a dollars more, so Jasna \
went to horror tunnel for ~a dollars.~n~n How many dollars does she have left?"
         '((50 100) (15 30) (10 20) (20 40) (10 20))
         t-func
         (lambda (a b c d e)
           (let ((formula `(,a - ,b - ,c + ,d - ,e)))
             (values formula formula))))
   
   (list "Our neighbors had ~a dollars in the bank. They bought a car for ~a \
dollars. Then they won ~a dollars on lottery, so they bought TV for ~a dollars, \
and a scooter for ~a dollars.~n~n How much money do they have left?"
         '((1200 2000) (1000 2000) (1000 2000) (200 300) (500 700))
         (lambda (a b c d e) (>= a b))
         (lambda (a b c d e)
           (let ((formula `(,a - ,b + ,c - ,d - ,e)))
             (values formula formula))))
   
   (list "Efren has ~a dollars, but he owes Jeff ~a dollars. He pays him back, \
and then borrows ~a dollars from Tom. Then he buys TV for ~a dollars, and a jacket \
for ~a dollars.~n~n How much money has he got left?"
         '((500 1000) (250 600) (500 900) (200 300) (50 150))
         (lambda (a b c d e) (>= a b))
         (lambda (a b c d e)
           (let ((formula `(,a - ,b + ,c - ,d - ,e)))
             (values formula formula))))
   
   (list "Tammy wrote ~a words in her copy-book, but she made mistakes, so she \
erased ~a words. Then she wrote ~a words more, but she changed her mind, \
and erased ~a again.~n~n How many words are on the paper now?"
         '((555 2000) (200 500) (555 1000) (300 500))
         t-func
         (lambda (a b c d)
           (let ((formula `(,a - ,b + ,c - ,d)))
             (values formula formula))))
   
   (list "While playing basketball, I shot the ball ~a times, while Elise shot \
the ball ~a times. I scored ~a times, and Elise scored ~a times.~n~n\
 How many times did we miss altogether?"
         '((25 50) (30 50) (20 40) (20 40))
         (lambda (a b c d) (and (>= a c) (>= b d)))
         (lambda (a b c d)
           (let ((formula `(,a - ,c + ,b - ,d)))
             (values formula formula))))
   
   (list "On a school trip to forest we picked ~a strawberries, ~a blueberries, and \
~a raspberries. Ivan eat ~a blueberries, Jasna ~a strawberries, and Clara eat \
~a raspberries.~n~n How many berries do we have left?"
         '((30 100) (30 100) (30 100) (20 40) (20 40) (20 40))
         (lambda (a b c d e f) (and (>= b d) (>= a e) (>= c f)))
         (lambda (a b c d e f)
           (let ((formula `(,a - ,e + ,b - ,d + ,c - ,f)))
             (values formula formula))))
   
   (list "We staid in Sogo hotel for ~a php, and ordered dinner \
for ~a php. Then we went to Vista hotel, where we paid ~a php \
for a room and ~a php for dinner.~n~n How much more did we pay in Sogo hotel?"
         '((2000 2500) (700 1000) (1000 2000) (500 700))
         t-func
         (lambda (a b c d)
           (let ((formula `(,a + ,b - ,c - ,d)))
             (values formula formula))))
   
   (list "During rally, KTM completed track 1 in ~a minutes, and track \
2 in ~a minutes. Yamaha completed same tracks in ~a and ~a minutes.~n~n\
 How many minutes was KTM faster than Yamaha overall?"
         '((50 85) (60 90) (60 90) (70 100))
         t-func
         (lambda (a b c d)
           (let ((formula `(,c + ,d - ,a - ,b)))
             (values formula formula))))
   
   (list "Class A picked ~a apples, and ~a mangoes. Class B picked \
~a oranges, and class C picked ~a pears, and ~a apples.~n~n\
 How many pieces of fruit did class C collect more than class A?"
         '((99 200) (99 200) (100 200) (120 300) (120 300))
         t-func
         (lambda (a b c d e)
           (let ((formula `(,d + ,e - ,a - ,b)))
             (values formula formula))))

   (list "We went on a weekend trip from our home to Sorsogon. We drove ~a kilometers to \
Guinobatan, then we drove ~a kilometers more to Legazpy city, and finally we drove ~a \
kilometers to Irosin. We had dinner there, and then we drove ~a kilometers back to \
Legazpy city.~n~n How far from our home are we now?"
         '((60 80) (20 30) (135 150) (135 150))
         (lambda (a b c d) (= c d))
         (lambda (a b c d)
           (let ((formula `(,a + ,b + ,c - ,d)))
             (values formula formula))))
   ))

(define word*problems ; multiplication problems
  (list
   (list "We had a New Year party. Charlie brought with him ~a coolers \
with ~a cans of coke each. Missy brought ~a coolers with ~a cans of fanta \
each. During the party we all drank ~a cans of soda.~n~n How many cans of soda \
were left after the party?"
         '((3 11) (6 11) (3 11) (6 11) (30 70))
         (lambda (a b c d e) (and (not (= a c)) (not (= b d))))
         (lambda (a b c d e)
           (let ((formula `(,a * ,b + ,c * ,d - ,e)))
             (values formula formula))))

   (list "Ayla had ~a php in her wallet. She went to the market and bought \
~a kilos of potato, ~a kilos of rice, and one loaf of sliced bread, which \
she paid ~a php. Rice costs ~a php per kilo, and potatoes cost ~a php per kilo.\
~n~n How much money does she have left in her wallet?"
         '((1500 2000) (2 11) (2 11) (80 120) (50 100) (110 150))
         (lambda (a b c d e f) (not (= b c)))
         (lambda (a b c d e f)
           (let ((formula `(,a - ,b * ,f - ,c * ,e - ,d)))
             (values formula formula))))

   (list "Jasna went to the supermarket with her family. Father placed ~a cartons of milk \
in the shopping cart. Mother placed ~a packets with Nescafe 3 in 1 sachets in the \
cart, and Jasna placed ~a ice-creams in the cart. A carton of milk has ~a boxes of milk, \
and a packet of 3 in 1 Nescafe has ~a sachets in it.~n~n\
 How many individual items did they have in the cart?"
         '((2 7) (3 8) (5 10) (6 13) (6 11))
         t-func
         (lambda (a b c d e)
           (let ((formula `(,a * ,d + ,b * ,e + ,c)))
             (values formula formula))))
   
   (list "We had a Christmas party. Bob brought with him ~a coolers \
with ~a bottles of beer each. We all drank ~a bottles. Then Rodney brought ~a \
coolers with ~a bottles of beer each. After that we all drank ~a bottles more.~n~n\
 How many bottles of beer were left after the party?"
         '((3 11) (6 11) (10 20) (3 11) (6 11) (30 70))
         (lambda (a b c d e f) (and (not (= a d)) (not (= b e)) (>= (* a b) c)))
         (lambda (a b c d e f)
           (let ((formula `(,a * ,b - ,c + ,d * ,e - ,f)))
             (values formula formula))))
   
   (list "Our condominium has ~a floors. To get from one floor to the other, Ricky must \
climb ~a stairs. He lives on the ~ath floor.~n~n How many stairs does he have to climb \
every day, when he comes back from school?"
         '((10 15) (15 20) (6 13))
         (lambda (a b c) (>= a c))
         (lambda (_ b c)
           (let ((formula `(,b * ,c)))
             (values formula formula))))

   (list "There are ~a trains with ~a wagons each at the train station. Each wagon \
is packed with ~a boxes on it. ~a trains leave the station.~n~n How many boxes are \
left on the train station now?"
         '((5 11) (10 15) (30 50) (2 7))
         (lambda (a b c d) (> a d))
         (lambda (a b c d)
           (let ((formula `((,a - ,d) * ,b * ,c)))
             (values formula formula))))

   (list "Tina's truck makes ~a kilometers per 1 liter of fuel consumed. \
She went on a long trip today, and her truck used ~a liters of fuel.~n~n\
 How many kilometers did she travel in total?"
         '((11 20) (30 50))
         t-func
         (lambda (a b)
           (let ((formula `(,a * ,b)))
             (values formula formula))))

   (list "Zia invited ~a kids to her party. If she wants to give ~a pieces of \
candy to each kid at the party,~n~n how many pieces of candy does she have to buy?"
         '((15 30) (11 30))
         t-func
         (lambda (a b)
           (let ((formula `(,a * ,b)))
             (values formula formula))))

   (list "Jessie is riding on his motorcycle at the average speed of ~a kph (kilometers \
per hour). He is riding for ~a hours, and then he stops for refreshments.~n~n\
 How many kilometers has he covered so far?"
         '((55 80) (5 10))
         t-func
         (lambda (a b)
           (let ((formula `(,a * ,b)))
             (values formula formula))))

   (list "We have many models of regular polygons in our class. We have ~a \
polygons with ~a sides, ~a polygons with ~a sides, and ~a polygons with ~a sides. \
All polygons have the same side length, ~a cm (centimeters).~n~n\
 What is the total length of all polygon sides?"
         '((2 10) (3 9) (2 10) (3 9) (2 10) (3 9) (10 20))
         (lambda (a b c d e f g) (not (or (= b d) (= d f) (= b f))))
         (lambda (a b c d e f g)
           (let ((formula `(,a * ,b * ,g + ,c * ,d * ,g + ,e * ,f * ,g)))
             (values formula formula))))

   (list"Our class went hiking into the forest. There were ~a pupils, and \
each one was asked to collect ~a mushrooms. At the end of a hike, the teacher \
counted the mushrooms. ~a pupils managed the required number of mushrooms, but \
the rest managed to collect only ~a mushrooms each.~n~n how many mushrooms \
did the teacher count?"
        '((20 40) (5 10) (10 30) (2 8))
        (lambda (a b c d) (and (< (+ 5 c) a) (< d b)))
        (lambda (a b c d)
          (let ((formula `(,c * ,b + (,a - ,c) * ,d)))
            (values formula formula))))

   (list "David went shopping in a supermarket. He bought ~a boxes of Lipton \
tea with ~a teabags in each, and ~a boxes of coffee with ~a sachets each. \
But then he realized that he doesn't have enough money, so he \
returned ~a boxes of tea.~n~n How many individual items did he buy?"
         '((4 8) (20 50) (3 6) (10 30) (2 5))
         (lambda (a b c d e) (and (not (= a c)) (not (= b d)) (> a e)))
         (lambda (a b c d e)
           (let ((formula `((,a - ,e) * ,b + ,c * ,d)))
             (values formula formula))))

   (list "The red rectangle measures ~a cm by ~a cm, while the blue rectangle is \
~a cm by ~a cm. How many square centimeters do they have altogether?"
         '((10 15) (12 18) (5 11) (10 20))
         (lambda (a b c d) (and (not (= a c)) (not (= b d))))
         (lambda (a b c d)
           (let ((formula `(,a * ,b + ,c * ,d)))
             (values formula formula))))

   (list "Marla has ~a boxes of pencils. Each box has ~a pencils in it.~n~n\
 How many pencils does Marla have?"
         '((5 12) (6 13))
         (lambda (a b) (not (= a b)))
         (lambda (a b)
           (let ((formula `(,a * ,b)))
             (values formula formula))))

   (list "Alex and Nikki were collecting empty plastic bottles. Alex collected \
~a bottles and Nikki collected ~a bottles. Then they sold them to a collection yard, \
and were paid ~a php for each bottle.~n~n\
 How much money did they get in all?"
         '((30 70) (30 70) (2 6))
         (lambda (a b c) (not (= a b)))
         (lambda (a b c)
           (let ((formula `((,a + ,b) * ,c)))
             (values formula formula))))         
   ))

(define word/problems ; division problems
  (list
   (list "When we started our drive in the morning, we first drove ~a kilometers to \
Pili, where we topped up fuel tank. Then we drove ~a kilometers to Legazpy, \
where we had lunch, and then continued ~a kilometers to Matnog. When we arrived, \
we topped up the fuel tank again with ~a liters of diesel.~n~n\
 How many kilometers is our SUV managing per 1 liter of fuel?"
         '((10 20) (60 100) (120 150) (15 25))
         t-func
         (lambda (_ a b c)
           (let ((formula `((,a + ,b) ,// ,c)))
             (values formula formula))))

   (list "Our neighbors family has ~a kids. They went to the mall today and bought \
~a chocolate bars. When they returned home they divided evenly chocolate bars \
between the kids.~n~n How many bars did each kid get?"
         '((2 6) (40 80))
         (lambda (a b) (zero? (modulo b a)))
         (lambda (a b)
           (let ((formula `(,b ,// ,a)))
             (values formula formula))))

   (list "We have many models of regular polygons in our class. We have ~a \
polygons with ~a sides, ~a polygons with ~a sides, and ~a polygons with ~a sides. \
If all the polygons have the same side length, and the total length of their sides is \
~a cm (centimeters).~n~n What is the length of a polygon side?"
         '((2 10) (3 9) (2 10) (3 9) (2 10) (3 9) (1000 3000))
         (lambda (a b c d e f g) (and (not (or (= b d) (= d f) (= b f)))
                                      (zero? (modulo g (+ (* a b) (* c d) (* e f))))))
         (lambda (a b c d e f g)
           (let ((formula `(,g ,// (,a * ,b + ,c * ,d + ,e * ,f))))
             (values formula formula))))

   (list "Symone has ~a php in her purse. She wants to buy a lot of chocolate bars.~n~n\
How many can she buy, if each one costs ~a php?"
         '((1000 2000) (35 85))
         (lambda (a b) (zero? (modulo a b)))
         (lambda (a b)
           (let ((formula `(,a ,// ,b)))
             (values formula formula))))

   (list "Our class of ~a pupils went on a picnic to the park. Our teacher \
brought ~a cookies with her. Then another class of ~a pupils joined us. Our teacher \
decided to distribute evenly the cookies among all the pupils.~n~n\
 How many cookies did each pupil get?"
         '((25 40) (100 300) (25 40))
         (lambda (a b c) (and (not (= a c)) (zero? (modulo b (+ a c)))))
         (lambda (a b c)
           (let ((formula `(,b ,// (,a + ,c))))
             (values formula formula))))

   (list "David has ~a barrels filled with wine. There are ~a liters of wine in each \
barrel. He wants to transfer all the wine into the plastic containers of ~a \
liters each.~n~n How many containers does he need?"
         '((2 10) (50 100) (3 11))
         (lambda (a b c) (and (not (= a c)) (zero? (modulo (* a b) c))))
         (lambda (a b c)
           (let ((formula `(,a * ,b ,// ,c)))
             (values formula formula))))

   (list "Captain Jack Sparrow is in command of ~a pirate ships sailing to the port \
of Barranquilla. Each ship is filled with a bullion of ~a gold bars. They run into a \
hurricane and ~a ships disappear, never to be seen again! But just before the port \
another ~a pirate ships loaded with ~a gold bars each join. In the port, a caravan of \
ox driven carts is waiting for the ships to transport the gold further on, to the \
capital city.~n~n If each cart can carry ~a gold bars, how many carts are needed in all?"
         '((8 15) (50 100) (3 7) (5 10) (50 100) (11 20))
         (lambda (a b c d e f) (and (not (= b e))
                                    (zero? (modulo (+ (* (- a c) b) (* d e)) f))))
         (lambda (a b c d e f)
           (let ((formula `(((,a - ,c) * ,b + ,d * ,e) ,// ,f)))
             (values formula formula))))

   (list "There are ~a pirate ships sailing to the port of Cartagena. Each ship \
is filled with a bullion of ~a gold bars. They run into a hurricane and ~a ships disappear, \
never to be seen again! In the port, a caravan of ox driven carts is waiting for the ships \
to transport the gold further on, to the capital city.~n~n\
 If each cart can carry ~a gold bars, how many carts are needed in all?"
         '((8 15) (50 100) (3 7) (10 20))
         (lambda (a b c d) (zero? (modulo (* (- a c) b) d)))
         (lambda (a b c d)
           (let ((formula `((,a - ,c) * ,b ,// ,d)))
             (values formula formula))))
   
   (list "My brother has ~a php, and I have ~a php. Mother sends us to buy \
bread. With the rest of the money, we can buy as many ice-creams as we can. \
Bread costs ~a php, and ice-creams cost ~a php.~n~n\
 How many ice-creams can we buy?"
         '((100 300) (200 400) (80 120) (50 80))
         (lambda (a b c d) (zero? (modulo (- (+ a b) c) d)))
         (lambda (a b c d)
           (let ((formula `((,a + ,b - ,c) ,// ,d)))
             (values formula formula))))

   (list "We were driving a car at average speed of ~a kph, for ~a \
kilometers before we stopped for dinner.~n~n\
 How many hours did we drive?"
         '((35 75) (300 750))
         (lambda (a b) (zero? (modulo b a)))
         (lambda (a b)
           (let ((formula `(,b ,// ,a)))
             (values formula formula))))

   (list "Cristina is cooking pork & chicken curry with rice. She has ~a grams of \
lean pork meat, and ~a grams of chicken meat. It takes on the average ~a grams of meat \
for a meal per person.~n~n How many persons can she feed with the meat she's got?"
         '((1000 2000) (1000 2000) (100 250))
         (lambda (a b c) (zero? (modulo (+ a b) c)))
         (lambda (a b c)
           (let ((formula `((,a + ,b) ,// ,c)))
             (values formula formula))))

   (list "Teacher has prepared ~a pink booklets for girls, and ~a blue booklets \
for boys.~n~n If each girl gets ~a booklets, and each boy gets ~a booklets, \
how many pupils are in the class in all?"
         '((60 120) (60 120) (4 8) (4 8))
         (lambda (a b c d) (and (not (= a b)) (not (= c d))
                                (zero? (modulo a c)) (zero? (modulo b d))))
         (lambda (a b c d)
           (let ((formula `(,a ,// ,c + ,b ,// ,d)))
             (values formula formula))))

   (list "Our grandmother has ~a kilos of rice. She wants to give equal share to all \
of her ~a children. How many grams of rice is each one getting?"
         '((20 50) (7 11))
         (lambda (a b) (zero? (modulo (* a 1000) b)))
         (lambda (a b)
           (let ((formula `(,a * 1000 ,// ,b)))
             (values formula formula))))

   (list "When we started our trip, we topped up our fuel tank, and we zeroed \
our partial trip meter. After traveling ~a kilometers, we stopped at a gas station, \
and topped our tank again with ~a liters of diesel.~n~n How many liters is our \
truck consuming per 100 km?"
         '((80 500) (25 50))
         (lambda (a b) (and (> (/ a b) 6) (< (/ a b) 25)))
         (lambda (a b)
           (let ((formula `(,b ,// (,a ,// 100))))
             (values formula formula))))

   (list "Train left station A at ~a o'clock AM. It is traveling at \
average speed of ~a kph.~n~n At what time (o'clock PM) did the train arrive at station B, \
which is ~a km away from station A?"
         '((8 12) (40 90) (250 400))
         (lambda (a b c) (and (zero? (modulo c b)) (> (/ c b) 4) (< (/ c b) 12)))
         (lambda (a b c)
           (let ((formula `(,a + ,c ,// ,b - 12)))
             (values formula formula))))
   ))

(define word+problems (append-shuffle word+problems1 word+problems2))
(define word-problems (append-shuffle word-problems1 word-problems2))
(define word+-problems (append-shuffle word+-problems1 word+-problems2))

;;; tests
;;; ==========================================================

(module+ test
  (require rackunit) 
  (check-equal?
   (caar (memf (lambda (x) (string=? (car x)
                                     "Garry has ~a seashells. His sister has ~a seashells more than he \
does.~n~n How many seashells do they have altogether?")) word+problems))
   "Garry has ~a seashells. His sister has ~a seashells more than he \
does.~n~n How many seashells do they have altogether?")
  (check-equal?
   (caar (memf (lambda (x) (string=? (car x)
                                     "Ivan puts ~a cookies into boxes A and B. If there are ~a cookies in \
box A, how many cookies are there in box B?")) word-problems))
   "Ivan puts ~a cookies into boxes A and B. If there are ~a cookies in \
box A, how many cookies are there in box B?") 
  (check-equal?
   (caar (memf (lambda (x) (string=? (car x)
                                     "Rope A is ~a cm long. Rope B is ~a cm longer than rope A. \
Rope C is ~a cm shorter than rope B.~n~n \
How long is rope C?")) word+-problems))  "Rope A is ~a cm long. Rope B is ~a cm longer than rope A. \
Rope C is ~a cm shorter than rope B.~n~n \
How long is rope C?")
  (check-=
   (+ (length word+problems1) (length word+problems2)) (length word+problems) 0)
  (check-=
   (+ (length word-problems1) (length word-problems2)) (length word-problems) 0)
  (check-=
   (+ (length word+-problems1) (length word+-problems2)) (length word+-problems) 0))

;;; Circumference & Area of Polygons
;;; ===========================================================

(define case0 #f) ; all equal size
(define case1 #f) ; two same size
(define choice (random 4))

(define (reset-cases)
  (set! case0 #f)
  (set! case1 #f))

(define (reset-choice)
  (set! choice (random 4)))

(define circumference1
  (list
   (list "We are given a triangle with following sides:~n\
a = ~a cm, b = ~a cm, and c = ~a cm. ~n
 What is the perimeter of the polygon?"
         '((5 20) (5 20) (5 20))
         (lambda (a b c)
           (reset-cases)
           (case choice
             ((0) (and (= a b c) (set! case0 #t) #t))
             ((1) (and (and (not (= a b c))
                            (or (and (= a b) (> (+ a b) c))
                                (and (= a c) (> (+ a c) b))
                                (and (= b c) (> (+ b c) a))))
                       (set! case1 #t) #t))
             (else (and (> (+ a b) c)
                        (> (+ a c) b)
                        (> (+ b c) a)
                        (not (= a b))
                        (not (= a c))
                        (not (= b c))))))
         (lambda (a b c)
           (let ((formula
                  (cond
                    (case0 `(,a * 3))
                    (case1 (or (and (= a b) `(,a * 2 + ,c))
                               (and (= a c) `(,a * 2 + ,b))
                               `(,b * 2 + ,a)))
                    (else `(,a + ,b + ,c)))))
             (reset-choice)
             (values formula formula))))

   (list "We are given a rectangle with following sides:~n\
a = ~a cm, and b = ~a cm. ~n
 What is the perimeter of the polygon?"
         `((5 20) (10 30))
         (lambda (a b)
           (reset-cases)
           (case choice
             ((0 1) (and (= a b) (set! case0 #t) #t))
             (else (not (= a b)))))
         (lambda (a b)
           (let ((formula (if case0 `(,a * 4) `(,a * 2 + ,b * 2))))
             (reset-choice)
             (values formula formula))))
   
   (list "We are given a pentagon with the side of ~a cm.~n
 What is the perimeter of the polygon?"
         '((3 11))
         t-func
         (lambda (a)
           (let ((formula `(,a * 5)))
             (values formula formula))))

   (list "We are given a hexagon with the side of ~a cm.~n
 What is the perimeter of the polygon?"
         '((3 11))
         t-func
         (lambda (a)
           (let ((formula `(,a * 6)))
             (values formula formula))))

   (list "We are given irregular pentagon with following sides:~n\
a = ~a cm, b = ~a cm, c = ~a cm, d = ~a cm, and e = ~a cm. ~n
 What is the perimeter of the polygon?"
         '((10 20) (10 20) (10 20) (10 20) (10 20))
         t-func
         (lambda (a b c d e)
           (let ((formula `(,a + ,b + ,c + ,d + ,e)))
             (values formula formula))))
   ))

(define circumference2
  (list
   (list "We are given a circle with radius of r = ~a cm.~n
 What is the perimeter of the circle?~n
Use 3.14 for pi"
         '((3 15))
         t-func
         (lambda (r)
           (let ((formula `(2 * ,r * 3.14)))
             (values formula formula))))

   (list "We are given a triangle with perimeter of ~a cm. Side a is \
~a cm, and side b is ~a cm.~n
 How long is the side c?"
         '((15 34) (5 12) (5 12))
         (lambda (o a b) (let ((c (- o (+ a b))))
                           (and (< c (+ a b)) (< a (+ b c)) (< b (+ a c)))))
         (lambda (o a b)
           (let ((formula `(,o - ,a - ,b)))
             (values formula formula))))

   (list "We are given a rectangle with perimeter of ~a cm. Side a is \
~a cm.~n
 How long is the side b?"
         '((20 60) (5 16))
         (lambda (c a) (let ((b2 (- c (* a 2))))
                         (and (even? b2) (> b2 1))))
         (lambda (c a)
           (let ((formula `((,c - ,a * 2) ,// 2)))
             (values formula formula))))

   (list "We are given a circle with perimeter of ~a cm.~n
 What is the radius of the circle?~n
Use 3.14 for pi"
         '((15 100))
         t-func
         (lambda (a)
           (let ((formula `(,a ,// (3.14 * 2))))
             (values formula formula))))

   (list "We are given an irregular pentagon with perimeter of ~a cm. Side \
a is ~a cm, side b is ~a cm, side c is ~a cm, and side d is ~a cm.~n
 How long is the side e?"
         '((30 80) (5 16) (5 16) (5 16) (5 16))
         t-func
         (lambda (o a b c d)
           (let ((formula `(,o - (,a + ,b + ,c + ,d))))
             (values formula formula))))
   ))

(define area1
  (list
   (list "We are given a triangle with side a of ~a cm, side b of ~a cm, \
and side c of ~a cm.~n
 What is the area of the triangle?"
         '((5 13) (5 13) (5 13))
         (lambda (a b c)
           (and (< a (+ b c)) (< b (+ a c)) (< c (+ a b))))
         (lambda (a b c)
           (let ((s `((,a + ,b + ,c) ,// 2))
                 (sv (/ (+ a b c) 2)))
             (values (format "s    = ~a~narea = ~a" (list2string s)
                             (list2string `(2,V (s * (s - ,a) * (s - ,b) * (s - ,c)))))
                     `(2,V (,sv * (,sv - ,a) * (,sv - ,b) * (,sv - ,c)))))))

   (list "We are given a rectangle with side a of ~a cm, and side b of ~a cm.~n
 What is the area of the rectangle?"
         `((5 20) (5 20))
         (lambda (a b)
           (reset-cases)
           (case choice
             ((0 1) (and (= a b) (set! case0 #t) #t))
             (else (not (= a b)))))
         (lambda (a b)
           (let ((formula (if case0 `(,a ^ 2) `(,a * ,b))))
             (reset-choice)
             (values formula formula))))

   (list "We are given a circle with radius = ~a cm.~n
 What is the area of the circle?~n
Use 3.14 for pi"
         '((5 20))
         t-func
         (lambda (r)
           (let ((formula `(,r ^ 2 * 3.14)))
             (values formula formula))))

   (list "We are given a Right triangle with legs a = ~a cm, and b = ~a cm.~n
 What is the length of side c (hypotenuse)?"
         '((3 15) (3 15))
         t-func
         (lambda (a b)
           (let ((formula `(2,V (,a ^ 2 + ,b ^ 2))))
             (values formula formula))))

   (list "We are given a circle with area = ~a cm².~n
 What is the radius of the circle?~n
Use 3.14 for pi"
         '((25 250))
         t-func
         (lambda (a)
           (let ((formula `(2,V (,a ,// 3.14))))
             (values formula formula))))

   (list "We are given a Right triangle with legs: a = ~a cm, and b = ~a cm.~n
 What is the area of that triangle?"
         '((5 20) (8 25))
         t-func
         (lambda (a b)
           (let ((formula `(,a * ,b ,// 2)))
             (values formula formula))))

   (list "We are given a Right triangle with area = ~a cm², \
and leg a = ~a cm.~n
 What is the length of leg b?"
         '((30 150) (5 20))
         t-func
         (lambda (a b)
           (let ((formula `(,a * 2 ,// ,b)))
             (values formula formula))))

   (list "We are given a Right triangle with hypotenuse c = ~a cm, and \
leg a = ~a cm.~n What is the length of leg b?"
         '((10 25) (5 15))
         (lambda (c a) (> c (add1 a)))
         (lambda (c a)
           (let ((formula `(2,V (,c ^ 2 - ,a ^ 2))))
             (values formula formula))))

    (list "We are given a Right triangle with hypotenuse c = ~a cm, and \
leg a = ~a cm.~n What is the area of that triangle?"
         '((10 25) (5 15))
         (lambda (c a) (> c (add1 a)))
         (lambda (c a)
           (let* ((b `(2,V (,c ^ 2 - ,a ^ 2)))
                  (bv (evaluate b)))
             (values (format "b    = ~a~narea = ~a" (list2string b)
                             (list2string `(,a * b ,// 2)))
                     `(,a * ,bv ,// 2)))))
    ))
                                                          
;;; export
;;; ===========================================================

;;; data
(provide *dictionary* word+problems word-problems word+-problems word*problems
         word/problems circumference1 circumference2 area1)

