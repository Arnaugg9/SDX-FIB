-module(lock3).
-export([start/1]).

%% Crea un nuevo proceso para el lock con un identificador único
start(MyId) ->
    spawn(fun() -> init(MyId) end).

%% Inicializa el proceso y espera recibir la lista de nodos peer
init(MyId) ->
    receive
        {peers, Nodes} ->
            open(Nodes, MyId, 0); % Inicia estado "abierto" con reloj lógico en 0
        stop ->
            ok
    end.

%% Estado "abierto": el lock está disponible y puede ser solicitado
%% MyClock mantiene el valor del reloj lógico local
open(Nodes, MyId, MyClock) ->
    receive
        {take, Master, Ref} ->
            MyNewClock = MyClock + 1, % IMPORTANTE: Incrementa el reloj al solicitar el lock
            Refs = requests(Nodes, MyId, MyNewClock), % Envía solicitudes con timestamp
            wait(Nodes, Master, Refs, [], Ref, MyId, MyNewClock, MyNewClock);
        {request, From, Ref, _, ReqTime} ->
            %% IMPORTANTE: Actualiza el reloj local al máximo entre el actual y el recibido
            MyNewClock = max(MyClock, ReqTime),
            From ! {ok, Ref}, % Responde inmediatamente en estado abierto
            open(Nodes, MyId, MyNewClock);
        stop ->
            ok;
        {ok, _} ->
            %% Ignora mensajes de confirmación retrasados
            open(Nodes, MyId, MyClock);
        Error ->
            io:format("open: unsupported message: ~w~n", [Error]),
            open(Nodes, MyId, MyClock)
           
    end.

%% Envía solicitudes a todos los nodos peers incluyendo el MyId y el timestamp actual
requests(Nodes, MyId, MyClock) ->
    lists:map(
      fun(P) -> 
        R = make_ref(), 
        P ! {request, self(), R, MyId, MyClock}, % IMPORTANTE: Incluye timestamp para ordenamiento
        R 
      end, 
      Nodes).

%% Estado de espera: aguarda respuesta de todos los peers antes de tomar el lock
%% MyReqTime es el timestamp de la solicitud original, MyClock es el reloj actual
wait(Nodes, Master, [], Waiting, TakeRef, MyId, _, MyClock) ->
    %% Cuando recibe todas las confirmaciones, toma el lock
    Master ! {taken, TakeRef},
    held(Nodes, Waiting, MyId, MyClock);
wait(Nodes, Master, Refs, Waiting, TakeRef, MyId, MyReqTime, MyClock) ->
    receive
        {request, From, Ref, ReqId, ReqTime} ->
            MyNewClock = max(MyClock, ReqTime), % Actualiza el reloj local
            
            %% CLAVE DEL ALGORITMO DE LAMPORT: Ordenamiento por timestamps
            %% Si la solicitud tiene un timestamp menor O igual pero con ID menor (mayor prioridad)
            if ( ReqTime < MyReqTime ) or (( ReqTime == MyReqTime ) and ( ReqId < MyId )) ->
                From ! {ok, Ref}, % Cede el paso a la solicitud con prioridad
                wait(Nodes, Master, Refs, Waiting, TakeRef, MyId, MyReqTime, MyNewClock);

            true -> 
                %% Deja la solicitud en espera si tiene menor prioridad
                wait(Nodes, Master, Refs, [{From, Ref}|Waiting], TakeRef, MyId, MyReqTime, MyNewClock)
            end;
        
        {ok, Ref} ->
            %% Recibe confirmación de un nodo
            NewRefs = lists:delete(Ref, Refs),
            wait(Nodes, Master, NewRefs, Waiting, TakeRef, MyId, MyReqTime, MyClock);
        
        release ->
            %% Si se libera mientras espera, responder a todos los que esperan
            ok(Waiting),
            open(Nodes, MyId, MyClock);

        Error ->
            io:format("wait: unsupported message: ~w~n", [Error]),
            wait(Nodes, Master, Refs, Waiting, TakeRef, MyId, MyReqTime, MyClock)
    end.

%% Envía confirmaciones a todos los nodos en espera
ok(Waiting) ->
    lists:map(
      fun({F,R}) -> 
        F ! {ok, R} 
      end, 
      Waiting).

%% Estado "held": el lock está tomado por este proceso
held(Nodes, Waiting, MyId, MyClock) ->
    receive
        {request, From, Ref, _, ReqTime} ->
            MyNewClock = max(MyClock, ReqTime),
            held(Nodes, [{From, Ref}|Waiting], MyId, MyNewClock);
        release ->
            %% Al liberar, responde a todos los que esperaban
            ok(Waiting),
            open(Nodes, MyId, MyClock);
        {ok, _} ->
            %% Ignora mensajes de confirmación retrasados
            held(Nodes, Waiting, MyId, MyClock);
        Error ->
            io:format("held: unsupported message: ~w~n", [Error]),
            held(Nodes, Waiting, MyId, MyClock)
    end.