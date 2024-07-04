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

% Todas las tecnologias de todas las civilizaciones
todasLasTecnologias(Tecnologia):-
    tecnologia(_, Tecnologia).

% Verifica si todas las personas de la civilizacion saben la tecnologia
civilizacionSabeTecnologia(Civilizacion, Tecnologia) :-
    esJugador(Persona, Civilizacion),
    tecnologia(Persona, Tecnologia).

% Toma una civilizacion y verifica si es lider
civilizacionLider(Civilizacion):-
    esJugador(_ , Civilizacion),
    forall(civilizacionSabeTecnologia(_, Tecnologia), civilizacionSabeTecnologia(Civilizacion, Tecnologia)).

% campeones 1-100, jinetes cab-cam, piqueros 1-3 si-no.
%Poner todas las unidades juntas ?
% jugador, unidad[], vida.
unidad (ana, jinete[caballo],90).
unidad (ana, piquero[escudo,1],55).
unidad (ana, piquero[sinEscudo,2],65).
unidades (ana,unidad(jinete,caballo)). %cambiarnombreaunidadesporotra cosa
unidades (ana, unidad(piquero,escudo,1)).
unidades (ana, unidad(piquero,sinEscudo,2)).
unit (ana,unidad(jinete,caballo,90),). %cambiarnombreaunidadesporotra cosa
unit (ana, unidad(piquero,escudo,1,55)).
unit (ana, unidad(piquero,sinEscudo,2,65)).

tiene(ana,jinete(caballo,90)).
tiene(ana,piquero(escudo, 1, 55)).
tiene(ana,piquero(sinEscudo, 2, 65)).

vidaDe(Jugador, TotalVida):-
    unidad(Jugador,_,TotalVida).
%vidaDe(ana,)
%existeUnaUnidadQueEsMayorATodos

tieneVida(unidad())
forall((Jugador,Unidad,Vida),(Unidad,Vida)>=(_,Vida)).

% ?(Unidad,Vida)>=(_,Vida).
compararVida(Jugador, X , Y):-
    (Jugador,Unidad,Vida)>=(Jugador,_,Vida).
