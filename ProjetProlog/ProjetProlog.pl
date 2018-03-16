%-------------------------------------
%             RUSH HOUR
%-------------------------------------

% Projet d'intelligence artificielle en langage Prolog 
% Réalisé par Lauren Baillot et Claire Smal
% Etudiantes de deuxième année, promotion 2019 de l'ENSC

%---------------------------------------
% REGLES DU JEU ET OPERATEURS PERMANENTS
%---------------------------------------

findujeu :- voiture(j,_,_,(_,_,3,6)),  %Lorsque le joueur a sa dernière composante sur la case 3,6 il a gagné
    write('\e[2J'),
    write("\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\nFelicitations ! Vous avez gagne !!!\n\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n"),
    demarrer().

:- dynamic(voiture/4).        %définition dynamiques des voitures car on va en enlever et en créer
:- dynamic(compteurmvmt/1).

%---------------
%GESTION DU JEU
%--------------

demarrer():-
    write('\e[2J'),
    retractall(voiture(_,_,_,_)),
    write("Bienvenue au demarrage du jeu Rush Hour ! \n\nREGLES DU JEU \nVotre voiture, marquee par la lettre j, est coincee dans les embouteillages. \nLe but est de l'en faire sortir en rejoignant la case dont le contenu est ecrit en rouge. \n\nPour cela, vous devez deplacer les voitures qui vous bloquent. \nLeur deplacement est permis en suivant les instructions a l'ecran.\nAjoutez un point '.' et appuyez sur entree pour valider chaque saisie \n\nIl vous est impossible de vous deplacer en dehors des limites de la grille, de taille 6*6. \nVous ne pouvez pas non plus vous deplacer sur une case deja occupee par une voiture. \nLes voitures de direction verticale ne peuvent se deplacer que vers le haut ou le bas. \nDe meme les voitures de direction horizontales ne peuvent se deplacer que vers la droite ou la gauche. \n\nPour commencer, choisissez le niveau de jeu en ecrivant 'niveau(X).' avec X le niveau choisi entre 1,2, 3 ou 4.").

compteurmvmt(-1).

tour() :-   
        findujeu.

tour() :-
    retract(compteurmvmt(X)),
    Y is X+1,
    assert(compteurmvmt(Y)),
    write('\e[2J'),
    write("\n Votre score : " +Y),
    write("\n\nGrille de jeu\n"),
    affichageGrille(1),
    repeat,
        (repeat,
            write("\nSaisissez l'identifiant de la voiture a bouger (numeros sur la grille)"),
            read(Identifiant),
            ((Identifiant==j; Identifiant==1; Identifiant==2 ; Identifiant==3 ; Identifiant==4 ; Identifiant==5 ; Identifiant==6 ; Identifiant==7 ; Identifiant==8 ; Identifiant==9 ; Identifiant==a)->!
            ; Identifiant==stop -> demarrer() %TROUVER UN MOYEN DE SORTIR DU TOUR !!!!
            ; write("\nL'identifiant entre n'est pas valide, recommencez "), 
            fail
            ),        
        repeat,
            write("Saisissez la direction du deplacement souhaite (haut,bas,gauche,droite)"),
            read(Direction),
            ((Direction==droite ; Direction==gauche ; Direction==haut ; Direction==bas)->!
            ; Direction==stop -> demarrer() 
            ; write("\nLa direction de deplacement saisie n'est pas valide, recommencez "), 
            fail
            ),
        bouger(Identifiant,Direction)->!
        ; write("\nLe deplacement entre n'est pas autorise, recommencez"), fail
        ),
    tour().

%----------------------
% DEFINITION DES OBJETS
%----------------------

%les voitures verticales sont définies de h en b et les voitures horizontales de g à d
%voiture (I,T,O,A) :- identifiant(I),taille(T),orientation(O), adresse(A).

niveau(Commande) :- Commande == 0,
    assert(voiture(1,2,horizontal,(1,1,1,2))),
    assert(voiture(j,2,horizontal,(3,2,3,3))),
    affichageGrille(1),
    tour().

niveau(Commande) :- Commande == 1,
    assert(voiture(1,2,horizontal,(1,1,1,2))),
    assert(voiture(2,3,vertical,(2,1,3,1,4,1))),
    assert(voiture(3,2,vertical,(5,1,6,1))),
    assert(voiture(4,3,vertical,(1,6,2,6,3,6))),
    assert(voiture(5,3,vertical,(2,4,3,4,4,4))),
    assert(voiture(6,2,horizontal,(5,5,5,6))),
    assert(voiture(7,3,horizontal,(6,3,6,4,6,5))),
    assert(voiture(j,2,horizontal,(3,2,3,3))),
    tour().

niveau(Commande) :- Commande == 2,
    assert(voiture(1,2,vertical,(1,1,2,1))),
    assert(voiture(2,3,horizontal,(4,1,4,2,4,3))),
    assert(voiture(3,2,horizontal,(6,1,6,2))),
    assert(voiture(4,2,vertical,(5,3,6,3))),
    assert(voiture(5,2,horizontal,(6,4,6,5))),
    assert(voiture(6,2,horizontal,(5,5,5,6))),
    assert(voiture(7,3,vertical,(2,6,3,6,4,6))),
    assert(voiture(8,2,vertical,(3,5,4,5))),
    assert(voiture(9,2,vertical,(2,4,3,4))),
    assert(voiture(a,3,horizontal,(1,4,1,5,1,6))),
    assert(voiture(j,2,horizontal,(3,1,3,2))),
    tour().

niveau(Commande) :- Commande == 3,
    assert(voiture(1,2,horizontal,(4,2,4,3))),
    assert(voiture(2,2,vertical,(5,2,6,2))),
    assert(voiture(3,2,horizontal,(6,3,6,4))),
    assert(voiture(4,3,vertical,(3,4,4,4,5,4))),
    assert(voiture(5,3,vertical,(4,6,5,6,6,6))),
    assert(voiture(j,2,horizontal,(3,2,3,3))),
    tour().

niveau(Commande) :- Commande == 4,
    assert(voiture(1,3,vertical,(1,1,2,1,3,1))),
    assert(voiture(2,3,vertical,(1,4,2,4,3,4))),
    assert(voiture(3,2,vertical,(4,3,5,3))),
    assert(voiture(4,3,horizontal,(4,4,4,5,4,6))),
    assert(voiture(5,2,vertical,(5,6,6,6))),
    assert(voiture(6,3,horizontal,(6,3,6,4,6,5))),
    assert(voiture(j,2,horizontal,(3,2,3,3))),
    tour().

%--------------------------
%CASE PRISE PAR UNE VOITURE
%--------------------------

caseprise(X,Y,I) :- 
    voiture(I,_,_,(X,Y,_,_)) ;
    voiture(I,_,_,(_,_,X,Y)).
caseprise(X,Y,I) :- 
    voiture(I,_,_,(X,Y,_,_,_,_)) ; 
    voiture(I,_,_,(_,_,X,Y,_,_)) ;
    voiture(I,_,_,(_,_,_,_,X,Y)).

%-------------------
% BLOCAGE PAR UN MUR
%-------------------

bloquemurG(V) :- 
    voiture(V,2,horizontal,(_,1,_,_)) ;
    voiture(V,3,horizontal,(_,1,_,_,_,_)).
bloquemurD(V) :- 
    voiture(V,2,horizontal,(_,_,_,6)) ;  
    voiture(V,3,horizontal,(_,_,_,_,_,6)).
bloquemurH(V) :-
    voiture(V,2,vertical,(1,_,_,_)) ; 
    voiture(V,3,vertical,(1,_,_,_,_,_)).
bloquemurB(V) :-
    voiture(V,2,vertical,(_,_,6,_)) ; 
    voiture(V,3,vertical,(_,_,_,_,6,_)).

%------------------------
% BLOCAGE PAR UNE VOITURE
%------------------------

%voiture horizontale bloquee par une voiture
bloquevoitureG(V) :- 
    voiture(V,2,horizontal,(X1,Y1,_,_)) ,
    Y is Y1-1 , caseprise(X1,Y,_).
bloquevoitureG(V) :- 
    voiture(V,3,horizontal,(X1,Y1,_,_,_,_)) , 
    Y is Y1-1 , caseprise(X1,Y,_).
bloquevoitureD(V) :- 
    voiture(V,2,horizontal,(_,_,X2,Y2)) ,
     Y is Y2+1 , 
     caseprise(X2,Y,_).
bloquevoitureD(V) :- 
    voiture(V,3,horizontal,(_,_,_,_,X3,Y3)) ,
     Y is Y3+1 ,
    caseprise(X3,Y,_).  

%voiture verticale bloquee par une voiture
bloquevoitureH(V) :- 
    voiture(V,2,vertical,(X1,Y1,_,_)) ,
     X is X1-1 ,
    caseprise(X,Y1,_).
bloquevoitureH(V) :- 
    voiture(V,3,vertical,(X1,Y1,_,_,_,_)) ,
    X is X1-1 ,
    caseprise(X,Y1,_).
bloquevoitureB(V) :- 
    voiture(V,2,vertical,(_,_,X2,Y2)),
    X is X2+1 ,
     caseprise(X,Y2,_).
bloquevoitureB(V) :- 
    voiture(V,3,vertical,(_,_,_,_,X3,Y3)) ,
    X is X3+1 ,
    caseprise(X,Y3,_).

%------------------------   
% MOUVEMENT D'UNE VOITURE
%------------------------

bouger(V,Dir) :- 
    Dir==droite,
    voiture(V,2,horizontal,(X1,Y1,X2,Y2)) ,
    \+bloquevoitureD(V) , 
    \+bloquemurD(V) , 
    Yn is Y1+1 , 
    Yn2 is Y2+1 , 
    retract(voiture(V,2,horizontal,(X1,Y1,X2,Y2))), 
    assert(voiture(V,2,horizontal,(X1,Yn,X2,Yn2))).

bouger(V,Dir) :- 
    Dir==droite, 
    voiture(V,3,horizontal,(X1,Y1,X2,Y2,X3,Y3)),
    \+bloquevoitureD(V) , 
    \+bloquemurD(V) , 
    Yn is Y1+1 , 
    Yn2 is Y2+1, 
    Yn3 is Y3+1 , 
    retract(voiture(V,3,horizontal,(X1,Y1,X2,Y2,X3,Y3))), 
    assert(voiture(V,3,horizontal,(X1,Yn,X2,Yn2,X3,Yn3))).

bouger(V,Dir) :- 
    Dir==gauche, 
    voiture(V,2,horizontal,(X1,Y1,X2,Y2)) , 
    \+bloquevoitureG(V) , 
    \+bloquemurG(V) , 
    Yn is Y1-1 , 
    Yn2 is Y2-1 , 
    retract(voiture(V,2,horizontal,(X1,Y1,X2,Y2))), 
    assert(voiture(V,2,horizontal,(X1,Yn,X2,Yn2))).

bouger(V,Dir) :- 
    Dir==gauche, 
    voiture(V,3,horizontal,(X1,Y1,X2,Y2,X3,Y3)) , 
    \+bloquevoitureG(V) , 
    \+bloquemurG(V) , 
    Yn is Y1-1 , 
    Yn2 is Y2-1, 
    Yn3 is Y3-1 , 
    retract(voiture(V,3,horizontal,(X1,Y1,X2,Y2,X3,Y3))), 
    assert(voiture(V,3,horizontal,(X1,Yn,X2,Yn2,X3,Yn3))).

bouger(V,Dir) :- 
    Dir==haut, 
    voiture(V,2,vertical,(X1,Y1,X2,Y2)) , 
    \+bloquevoitureH(V) , 
    \+bloquemurH(V) , 
    Xn is X1-1 , 
    Xn2 is X2-1 , 
    retract(voiture(V,2,vertical,(X1,Y1,X2,Y2))), 
    assert(voiture(V,2,vertical,(Xn,Y1,Xn2,Y2))).

bouger(V,Dir) :- 
    Dir==haut, 
    voiture(V,3,vertical,(X1,Y1,X2,Y2,X3,Y3)) , 
    \+bloquevoitureH(V) , 
    \+bloquemurH(V), 
    Xn is X1-1 , 
    Xn2 is X2-1, 
    Xn3 is X3-1 , 
    retract(voiture(V,3,vertical,(X1,Y1,X2,Y2,X3,Y3))), 
    assert(voiture(V,3,vertical,(Xn,Y1,Xn2,Y2,Xn3,Y3))).

bouger(V,Dir) :- 
    Dir==bas, 
    voiture(V,2,vertical,(X1,Y1,X2,Y2)) , 
    \+bloquevoitureB(V) , 
    \+bloquemurB(V) , 
    Xn is X1+1 , 
    Xn2 is X2+1 , 
    retract(voiture(V,2,vertical,(X1,Y1,X2,Y2))), 
    assert(voiture(V,2,vertical,(Xn,Y1,Xn2,Y2))).

bouger(V,Dir) :- 
    Dir==bas, 
    voiture(V,3,vertical,(X1,Y1,X2,Y2,X3,Y3)) , 
    \+bloquevoitureB(V) , 
    \+bloquemurB(V) , 
    Xn is X1+1 , 
    Xn2 is X2+1, 
    Xn3 is X3+1 , 
    retract(voiture(V,3,vertical,(X1,Y1,X2,Y2,X3,Y3))), 
    assert(voiture(V,3,vertical,(Xn,Y1,Xn2,Y2,Xn3,Y3))).

%---------
%AFFICHAGE
%---------

affichageGrille(Ligneactu) :- 
    Ligneactu == 7, 
    nl.

affichageGrille(Ligneactu) :- 
    Ligneactu <7,
    affichageLigne(Ligneactu,1),
    Lignesuiv is Ligneactu + 1,
    affichageGrille(Lignesuiv).

affichageLigne(_, Colonneactu) :- 
    Colonneactu == 7, 
    nl.

affichageLigne(Ligneactu,Colonneactu) :- 
    Colonneactu<7,
    affichageCellule(Ligneactu,Colonneactu),
    Colonnesuiv is Colonneactu + 1,
    affichageLigne(Ligneactu,Colonnesuiv).

affichageCellule(3,6) :- 
    caseprise(3,6,I),
    ansi_format([fg(red)],'~w',[I]),
    put(32).

affichageCellule(Ligneactu,Colonneactu) :- 
    caseprise(Ligneactu,Colonneactu,I),
    write(I), 
    put(32).

affichageCellule(3,6) :-
    \+caseprise(3,6,_),
    ansi_format([fg(red)],'~w',["-"]),
    put(32).

affichageCellule(Ligneactu,Colonneactu) :-
    \+caseprise(Ligneactu,Colonneactu,_),
    write("-"),
    put(32).

