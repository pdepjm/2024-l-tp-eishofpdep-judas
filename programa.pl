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
tecnologia(dimitri,herreria)
tecnologia(dimitri,fundicion)


esExpertoEnMetales(Persona):-
    esJugador(Persona, _),

esPopular(Civilizacion):-

tieneAlcanceGlobal(Tecnologia):-

esLider(Civilizacion):-

