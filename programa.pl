% Aquí va el código.
% jugadores civilizaciones tecnologias
% ana beto carola dimitri elsa
% jugador expertoenmetales herreria forja fundicion o romanos.
%civilizacion popular varios jugadores >1
%alcanceglobal tecnologia todos los jugadores
%civiliazacion lider toidas las tecnologias q tienen lso demas

esJugador(ana, romanos).
esJugador(beto, incas).
esJugador(carola, romanos).
esJugador(dimitri, romanos).
% no esJugador(elsa).
tecnologia(ana,herreria ).
tecnologia(ana, forja).
tecnologia(ana, emplumado).
tecnologia(ana, laminas ).
tecnologia(beto,herreria ).
tecnologia(beto, fundicion).
tecnologia(beto, forja).
tecnologia(carola,herreria).
tecnologia(dimitri,herreria).
tecnologia(dimitri,fundicion).

%  PUNTO 2
esExpertoEnMetales(Persona):-
    esJugador(Persona, _),
    tecnologia(Persona, herreria),
    tecnologia(Persona, forja),
    (tecnologia(Persona, fundicion) ; esJugador(Persona, romanos)).

%  PUNTO 3
esPopular(Civilizacion) :-
        esJugador(Persona1, Civilizacion),
        esJugador(Persona2, Civilizacion),
        Persona1 \= Persona2.