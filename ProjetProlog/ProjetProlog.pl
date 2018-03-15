%-------------------------------------
%             RUSH HOUR
%------------------------------------

% RESTE A FAIRE
% Si commande fausse entrée, avoir un message pour l'utilisateur qui lui donne les entrées valides
% Les instructions en début de partie 

% OU 
% Faire des boucles read et write pour le jeu afin d'avoir les commandes pré-saisies.


% NE PAS OUBLIER DE PARLER DE L'ARGUMENT TAILLE DE LA VOITURE QUE L'ON A PAS PU SUPPRIMER

%---------------------------------------
% REGLES DU jEU ET OPERATEURS PERMANENTS
%---------------------------------------

findujeu :- voiture(j,_,_,(_,_,3,6)),                   %Lorsque le joueur a sa dernière composante sur la case 3,6 il a gagné
    write("Felicitations ! Vous avez gagne !!!"),
    menu().

:- dynamic(voiture/4).                         %définition dynamiques des voitures car on va en enlever et en créer
:- dynamic(compteurmvmt/1).


%---------------
%GESTION DU JEU
%--------------

menu():-
    retractall(voiture(_,_,_,_)),
    write("REGLES A AJOUTER ICI !!!!! Bienvenue sur Rush Hour ! \n Vous devez sortir votre voiture j des embouteillage en deplacant les autres voitures ! \n La case sur laquelle vous devez arriver est indiquée en rouge \n Choisissez le niveau en entrant niveau(X) ou X le numero du niveau").

tour() :-   
    \+findujeu,
    retract(compteurmvmt(X)),
    Y is X+1,
    assert(compteurmvmt(Y)),
    write("Score "+ Y),
    nl,
    affichageGrille(1).

%----------------------
% DEFINITION DES OBjETS
%----------------------

%les voitures verticales sont définies de haut en bas et les voitures horizontales de gauche à droite
%voiture (I,T,O,A) :- identifiant(I),taille(T),orientation(O), adresse(A).

compteurmvmt(0).

niveau(Commande) :- Commande == 0,
    assert(voiture(1,2,horizontal,(1,1,1,2))),
    assert(voiture(j,2,horizontal,(3,2,3,3))),
    affichageGrille(1),
    write "Vous voici dans de sacrés embouteillages !"


niveau(Commande) :- Commande == 1,
    assert(voiture(1,2,horizontal,(1,1,1,2))),
    assert(voiture(2,3,vertical,(2,1,3,1,4,1))),
    assert(voiture(3,2,vertical,(5,1,6,1))),
    assert(voiture(4,3,vertical,(1,6,2,6,3,6))),
    assert(voiture(5,3,vertical,(2,4,3,4,4,4))),
    assert(voiture(6,2,horizontal,(5,5,5,6))),
    assert(voiture(7,3,horizontal,(6,3,6,4,6,5))),
    assert(voiture(j,2,horizontal,(3,2,3,3))),
    affichageGrille(1).

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
    affichageGrille(1).

niveau(Commande) :- Commande == 3,
    assert(voiture(1,2,horizontal,(4,2,4,3))),
    assert(voiture(2,2,vertical,(5,2,6,2))),
    assert(voiture(3,2,horizontal,(6,3,6,4))),
    assert(voiture(4,3,vertical,(3,4,4,4,5,4))),
    assert(voiture(5,3,vertical,(4,6,5,6,6,6))),
    assert(voiture(j,2,horizontal,(3,2,3,3))),
    affichageGrille(1).

niveau(Commande) :- Commande == 4,
    assert(voiture(1,3,vertical,(1,1,2,1,3,1))),
    assert(voiture(2,3,vertical,(1,4,2,4,3,4))),
    assert(voiture(3,2,vertical,(4,3,5,3))),
    assert(voiture(4,3,horizontal,(4,4,4,5,4,6))),
    assert(voiture(5,2,vertical,(5,6,6,6))),
    assert(voiture(6,3,horizontal,(6,3,6,4,6,5))),
    assert(voiture(j,2,horizontal,(3,2,3,3))),
    affichageGrille(1).

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
    Dir == droite , voiture(V,2,horizontal,(X1,Y1,X2,Y2)) ,
    \+bloquevoitureD(V) , 
    \+bloquemurD(V) , 
    Yn is Y1+1 , 
    Yn2 is Y2+1 , 
    retract(voiture(V,2,horizontal,(X1,Y1,X2,Y2))), 
    assert(voiture(V,2,horizontal,(X1,Yn,X2,Yn2))),
    tour().

bouger(V,Dir) :- 
    Dir == droite , 
    voiture(V,3,horizontal,(X1,Y1,X2,Y2,X3,Y3)) ,
    \+bloquevoitureD(V) , 
    \+bloquemurD(V) , 
    Yn is Y1+1 , 
    Yn2 is Y2+1, 
    Yn3 is Y3+1 , 
    retract(voiture(V,3,horizontal,(X1,Y1,X2,Y2,X3,Y3))), 
    assert(voiture(V,3,horizontal,(X1,Yn,X2,Yn2,X3,Yn3))),
    tour().

bouger(V,Dir) :- 
    Dir == gauche , 
    voiture(V,2,horizontal,(X1,Y1,X2,Y2)) , 
    \+bloquevoitureG(V) , 
    \+bloquemurG(V) , 
    Yn is Y1-1 , 
    Yn2 is Y2-1 , 
    retract(voiture(V,2,horizontal,(X1,Y1,X2,Y2))), 
    assert(voiture(V,2,horizontal,(X1,Yn,X2,Yn2))),
    tour().

bouger(V,Dir) :- 
    Dir == gauche , 
    voiture(V,3,horizontal,(X1,Y1,X2,Y2,X3,Y3)) , 
    \+bloquevoitureG(V) , 
    \+bloquemurG(V) , 
    Yn is Y1-1 , 
    Yn2 is Y2-1, 
    Yn3 is Y3-1 , 
    retract(voiture(V,3,horizontal,(X1,Y1,X2,Y2,X3,Y3))), 
    assert(voiture(V,3,horizontal,(X1,Yn,X2,Yn2,X3,Yn3))),
    tour().

bouger(V,Dir) :- 
    Dir == haut , 
    voiture(V,2,vertical,(X1,Y1,X2,Y2)) , 
    \+bloquevoitureH(V) , 
    \+bloquemurH(V) , 
    Xn is X1-1 , 
    Xn2 is X2-1 , 
    retract(voiture(V,2,vertical,(X1,Y1,X2,Y2))), 
    assert(voiture(V,2,vertical,(Xn,Y1,Xn2,Y2))),
    tour().

bouger(V,Dir) :- 
    Dir == haut , 
    voiture(V,3,vertical,(X1,Y1,X2,Y2,X3,Y3)) , 
    \+bloquevoitureH(V) , 
    \+bloquemurH(V), 
    Xn is X1-1 , 
    Xn2 is X2-1, 
    Xn3 is X3-1 , 
    retract(voiture(V,3,vertical,(X1,Y1,X2,Y2,X3,Y3))), 
    assert(voiture(V,3,vertical,(Xn,Y1,Xn2,Y2,Xn3,Y3))),
    tour().

bouger(V,Dir) :- 
    Dir == bas , 
    voiture(V,2,vertical,(X1,Y1,X2,Y2)) , 
    \+bloquevoitureB(V) , 
    \+bloquemurB(V) , 
    Xn is X1+1 , 
    Xn2 is X2+1 , 
    retract(voiture(V,2,vertical,(X1,Y1,X2,Y2))), 
    assert(voiture(V,2,vertical,(Xn,Y1,Xn2,Y2))),
    tour().

bouger(V,Dir) :- 
    Dir == bas , 
    voiture(V,3,vertical,(X1,Y1,X2,Y2,X3,Y3)) , 
    \+bloquevoitureB(V) , 
    \+bloquemurB(V) , 
    Xn is X1+1 , 
    Xn2 is X2+1, 
    Xn3 is X3+1 , 
    retract(voiture(V,3,vertical,(X1,Y1,X2,Y2,X3,Y3))), 
    assert(voiture(V,3,vertical,(Xn,Y1,Xn2,Y2,Xn3,Y3))),
    tour().


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

