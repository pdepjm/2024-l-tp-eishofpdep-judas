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
tecnologia(ana, herreria).
tecnologia(ana, forja).
tecnologia(ana, emplumado).
tecnologia(ana, laminas).
tecnologia(beto, herreria).
tecnologia(beto, fundicion).
tecnologia(beto, forja).
tecnologia(carola, herreria).
tecnologia(dimitri, herreria).
tecnologia(dimitri, fundicion).

%  PUNTO 2

esExpertoEnMetales(Persona):-
    esJugador(Persona,_),
    tecnologia(Persona,herreria),
    tecnologia(Persona,forja),
    oFundicionOEsRomano(Persona).

oFundicionOEsRomano(Persona):-
    tecnologia(Persona,fundicion).

oFundicionOEsRomano(Persona):-
    esJugador(Persona,romanos).
%  PUNTO 3
esPopular(Civilizacion):-
        esJugador(Persona1, Civilizacion),
        esJugador(Persona2, Civilizacion),
        Persona1 \= Persona2.


% PUNTO 4

tieneAlcanceGlobal(Tecnologia):-
    tecnologia(_,Tecnologia),
    forall(esJugador(Alguien, _) , tecnologia(Alguien, Tecnologia)).

% PUNTO 5

% Verifica si todas las personas de la civilizacion saben la tecnologia
civilizacionSabeTecnologia(Civilizacion, Tecnologia) :-
    esJugador(Persona, Civilizacion),
    tecnologia(Persona, Tecnologia).

% Toma una civilizacion y verifica si es lider
civilizacionLider(Civilizacion):-
    esJugador(_ , Civilizacion),
    forall(civilizacionSabeTecnologia(_, Tecnologia), civilizacionSabeTecnologia(Civilizacion, Tecnologia)).

% SEGUNDA PARTE

% PUNTO 6

tiene(ana, jinete(caballo)).
tiene(ana, piquero(conEscudo, 1)).
tiene(ana, piquero(sinEscudo, 2)).
tiene(beto, campeon(100)).
tiene(beto, campeon(80)).
tiene(beto, piquero(conEscudo, 1)).
tiene(beto, jinete(camello)).
tiene(carola, piquero(sinEscudo, 3)).
tiene(carola, piquero(conEscudo, 2)).

% Dimitri no tiene unidades

% Punto 7

% Vida jinetes
vidaUnidad(jinete(caballo), 90).
vidaUnidad(jinete(camello), 80).

% Vida Campeon
vidaUnidad(campeon(Vida), Vida).

% Vida Piqueros sin escudo
vidaUnidad(piquero(sinEscudo, 1), 50).
vidaUnidad(piquero(sinEscudo, 2), 65).
vidaUnidad(piquero(sinEscudo, 3), 70).

% Vida Piqueros con escudo
vidaUnidad(piquero(conEscudo, N), VidaConEscudo):-
    vidaUnidad(piquero(sinEscudo, N), VidaSinEscudo),
    VidaConEscudo is 1.10 * VidaSinEscudo.

% Resolver

% Usar Recursividad con listas posiblemente




