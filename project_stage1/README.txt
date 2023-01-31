/* Tema 1 PP - Anghel Ionela-Carmen 322CB */

/* Task 1 */

- am concatenat noul rand cu denimirile coloanelor la tabelul nou calculat
- write_table_steps adauga numele si numarul de pasi la tabelul final
- get_average_steps calculeaza media de pasi pentru un rand dat folosind foldr

- am definit doua functii auxiliare to_float si to_int pentru convertirea
stringurilor, utilizate pentru majoritatea taskurilor

/* Task 2 */

- am definit o functie care returneaza numarul total de pasi
- folosind functia auxiliara am calculat numaurl de oameni care si-au atins scopul
- am calculat procentul de oameni trecuti cu ajutorul functiei anteriare -impartirea
numarului de oameni trecuti la numarul total de oameni (fara numararea randului
cu denumirile coloanelor)
- am calculat media de pasi folosind foldr, unde op calculeaza numarul de fasi in
float.

- privind acum inapoi la cod, sesizez ca as fi putut simplifica prin scrierea functiei
total_steps astfel incat sa returneze float

/* Task 3 */
- am aplicat cate un sablon pentru fiecare coloana diferita
- m-am gandit si la crearea unei functii de trunchiare a unui tabel astfel incat
sa rezulte un tabel cu prima coloana cea ceruta, pentru a scapa de aceste sabloane
aproape identice (functia ar fi fost utila si la task 4)

/* Task 4 */
- am definit 3 functii auxiliare, una pentru fiecare variatie de dificultate care
intoarce randul corespunzator pentru tabelul de sumar

/* Task 5 */
- am definit o functie auxiliara sort_by_second_collum care sorteaza un tabel dupa
a doua coloana din tabel, respectiv dupa prima in cazul egalitatii
- am creat aceasta functie pentru a reutiliza bucatica de cod si la task 6 unde
este necesara o sortare

/* Task 6 */
- hDiference returneaza un tabel in care se afla datele cerute, cu diferenta ca
diferenta dintre cele 2 se afla in coloana 2
- rearange rearanjeaza tabelul pentru a interschimba coloana 2 cu coloana 4
- dupa calcularea tabelului, acesta este sortat cu functia sort_by_second_collum
si apoi rearanjat la forma ceruta

/* Task 7 */
- am utilizat map pentru implementare

/* Task 8 */
- am folosit float pentru calcularea timpului total dormit