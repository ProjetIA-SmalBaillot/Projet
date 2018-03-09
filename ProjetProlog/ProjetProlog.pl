 
% couleur(rouge).
% couleur(bleu).
% couleur(vert).
% couleur(jaune).
% couleur(orange).
% couleur(violet).
% couleur(gris).
% couleur(blanc).
% couleur(noir).
% couleur(prune).
% couleur(corail).
% couleur(azur).
% couleur(indigo).
% couleur(marron).
% couleur(bordeaux).
% couleur(joueur).




% coordonnees(1,1).
% coordonnees(1,2).
% coordonnees(1,3).
% coordonnees(1,4).
% coordonnees(1,5).
% coordonnees(1,6).
% coordonnees(2,1).
% coordonnees(2,2).
% coordonnees(2,3).
% coordonnees(2,4).
% coordonnees(2,5).
% coordonnees(2,6).
% coordonnees(3,1).
% coordonnees(3,2).
% coordonnees(3,3).
% coordonnees(3,4).
% coordonnees(3,5).
% coordonnees(3,6).
% coordonnees(4,1).
% coordonnees(4,2).
% coordonnees(4,3).
% coordonnees(4,4).
% coordonnees(4,5).
% coordonnees(4,6).
% coordonnees(5,1).
% coordonnees(5,2).
% coordonnees(5,3).
% coordonnees(5,4).
% coordonnees(5,5).
% coordonnees(5,6).
% coordonnees(6,1).
% coordonnees(6,2).
% coordonnees(6,3).
% coordonnees(6,4).
% coordonnees(6,5).
% coordonnees(6,6).

findujeu(3,6).

adresse(X,Y,Z,U) :- coordonnees(X,Y) , coordonnees(Z,U).
adresse(X,Y,Z,U,V,W) :- coordonnees(X,Y) , coordonnees(Z,U), coordonnees(V,W).

%def d'une voiture
%voiture (I,T,O,A) :- identifiant(I),taille(T),orientation(O), adresse(A).

%les voitures verticales sont définies de haut en bas et les voitures horizontales de gauche à droite

%niveau 1 
voiture(1,2,horizontal,(1,1,1,2)).
voiture(2,3,vertical,(2,1,3,1,4,1)).
voiture(3,2,vertical,(5,1,6,1)).
voiture(4,3,vertical,(1,6,2,6,3,6)).
voiture(5,3,vertical,(2,4,3,4,4,4)).
voiture(6,2,horizontal,(5,5,5,6)).
voiture(7,3,horizontal,(6,3,6,4,6,5)).
voiture(joueur,2,horizontal,(3,2,3,3)).

%case prise par une voiture de taille 2 ou 3
caseprise(X,Y) :- voiture(_,_,_,(X,Y,_,_)) ; voiture(_,_,_,(_,_,X,Y)).
caseprise(X,Y) :- voiture(_,_,_,(X,Y,_,_,_,_)) ; voiture(_,_,_,(_,_,X,Y,_,_)); voiture(_,_,_,(_,_,_,_,X,Y)).

% BLOCAGE PAR UN MUR
bloquemurG(V) :- voiture(V,2,horizontal,(_,1,_,_)) ; voiture(V,3,horizontal,(_,1,_,_,_,_)).
bloquemurD(V) :- voiture(V,2,horizontal,(_,_,_,6)) ;  voiture(V,3,horizontal,(_,_,_,_,_,6)).
bloquemurH(V) :- voiture(V,2,vertical,(1,_,_,_)) ; voiture(V,3,vertical,(1,_,_,_,_,_)).
bloquemurB(V) :- voiture(V,2,vertical,(_,_,6,_)) ; voiture(V,3,vertical,(_,_,_,_,6,_)).

% BLOCAGE PAR UNE VOITURE
%voiture horizontale bloquee par une voiture
bloquevoitureG(V) :- voiture(V,2,horizontal,(X1,Y1,X2,Y2)) , Y is Y1-1 , caseprise(X1,Y).
bloquevoitureG(V) :- voiture(V,3,horizontal,(X1,Y1,X2,Y2,X3,Y3)), Y is Y1-1 , caseprise(X1,Y).
bloquevoitureD(V) :- voiture(V,2,horizontal,(X1,Y1,X2,Y2)) , Y is Y2+1 , caseprise(X2,Y).
bloquevoitureD(V) :- voiture(V,3,horizontal,(X1,Y1,X2,Y2,X3,Y3)) , Y is Y3+1 , caseprise(X3,Y).                
%voiture verticale bloquee par une voiture
bloquevoitureH(V) :- voiture(V,2,vertical,(X1,Y1,X2,Y2)) , X is X1-1 , caseprise(X,Y1).
bloquevoitureH(V) :- voiture(V,3,vertical,(X1,Y1,X2,Y2,X3,Y3)) , X is X1-1 , caseprise(X,Y1).
bloquevoitureB(V) :- voiture(V,2,vertical,(X1,Y1,X2,Y2)) , X is X2+1 , caseprise(X,Y2).
bloquevoitureB(V) :- voiture(V,3,vertical,(X1,Y1,X2,Y2,X3,Y3)) , X is X3+1 , caseprise(X,Y3).

% VOITURE IMMOBILE
% Si elle est bloquée entre deux voitures ou si elle est bloquée entre une voiture et un mur : tous les cas possibles :
voitureimmobile(V) :- voiture(V,2,horizontal,(_,_,_,_)) , bloquevoitureG(V), bloquemurD(V). 
voitureimmobile(V) :- voiture(V,2,horizontal,(_,_,_,_)) , bloquevoitureD(V) , bloquemurG(V).
voitureimmobile(V) :- voiture(V,2,horizontal,(_,_,_,_)) , bloquevoitureD(V) , bloquevoitureG(V).                
voitureimmobile(V) :- voiture(V,3,horizontal,(_,_,_,_,_,_)) , bloquevoitureG(V) , bloquemurD(V).
voitureimmobile(V) :- voiture(V,3,horizontal,(_,_,_,_,_,_)) , bloquevoitureD(V) , bloquemurG(V).
voitureimmobile(V) :- voiture(V,3,horizontal,(_,_,_,_,_,_)) , bloquevoitureD(V) , bloquevoitureG(V).
voitureimmobile(V) :- voiture(V,2,vertical,(_,_,_,_)) , bloquevoitureH(V), bloquemurB(V).
voitureimmobile(V) :- voiture(V,2,vertical,(_,_,_,_)) , bloquevoitureB(V) , bloquemurH(V).
voitureimmobile(V) :- voiture(V,2,vertical,(_,_,_,_)) , bloquevoitureB(V) , bloquevoitureH(V).                
voitureimmobile(V) :- voiture(V,3,vertical,(_,_,_,_,_,_)) , bloquevoitureH(V), bloquemurB(V).
voitureimmobile(V) :- voiture(V,3,vertical,(_,_,_,_,_,_)) , bloquevoitureB(V) , bloquemurH(V).
voitureimmobile(V) :- voiture(V,3,vertical,(_,_,_,_,_,_)) , bloquevoitureB(V) , bloquevoitureH(V).
                
% MOUVEMENT D'UNE VOITURE

bougerD(V) :- voiture(V,2,horizontal,(_,Y1,_,Y2)) , \+voitureimmobile(V) , Yn is Y1+1 , Yn2 is Y2+1. %trouver un moyen d'affeter Yn et Yn2 à la voiture
bougerD(V) :- voiture(V,3,horizontal,(_,Y1,_,Y2,_,Y3)) , \+voitureimmobile(V) , Yn is Y1+1 , Yn2 is Y2+1 Yn3 is Y3+1.

bougerD(V) :- voiture(V,2,horizontal,(_,Y1,_,Y2)) , \+voitureimmobile(V) , Yn is Y1-1 , Yn2 is Y2-1. %trouver un moyen d'affeter Yn et Yn2 à la voiture
bougerG(V) :- voiture(V,3,horizontal,(_,Y1,_,Y2,_,Y3)) , \+voitureimmobile(V) , Yn is Y1-1 , Yn2 is Y2-1 Yn3 is Y3-1.


bougerH(V) :- voiture(V,2,vertical,(_,Y1,_,_,_,Y3)) , \+voitureimmobile(V) , Yn is Y1+1 , Yn3 is Y3+1.        
