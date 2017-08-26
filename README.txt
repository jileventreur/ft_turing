formatage de la machine :

alphabet|blank|states|init|finals|transition|_input
                                     cursor -^

exemple : 1+=.|.|ABC|A|C|A1B.RA.C+RB1A.RB.C=R|_1..1

formatage d'execution de la machine :
	
|registre|finals|transition|input|

exemple : |A1|.|C|A1B.RA.C+RB1A.RB.C=R|01.1|~~

Registre : Etat char_lu

Etat : A B C

	char lu : 0 .
	next : A B C
	char write : 0 . y n
	action : R L
		
-----------------------------------------------

input de la machine ispair : 
	./ft_turing machines/complete_turing.json "0.yn|.|ABC|A|C|A0B.RA.CyRB0A.RB.CnR|_000"

La machine accepte 3 etats differents (ABC) pour en ajouter il faut les ajouter dans state_range

pour compiler le json sh script_compil.sh