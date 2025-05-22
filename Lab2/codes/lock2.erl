-module(lock2).
-export([start/1]).

%% Crea un nuevo proceso para el lock
start(MyId) ->
    spawn(fun() -> init(MyId) end).

%% Inicializa el proceso y espera recibir la lista de nodos peer
init(MyId) ->
    receive
        {peers, Nodes} ->
            open(Nodes, MyId); % Inicia el estado "abierto" (sin lock)
        stop ->
            ok
    end.

%% Estado "abierto": el lock está disponible y puede ser solicitado
open(Nodes, MyId) ->
    receive
        {take, Master, Ref} ->
            %% Un worker solicita el lock
            Refs = requests(Nodes, MyId), % Envía solicitudes a todos los nodos
            wait(Nodes, Master, Refs, [], Ref, MyId);
        {request, From, Ref, _} ->
            %% Responde inmediatamente a solicitudes cuando está libre
            From ! {ok, Ref},
            open(Nodes, MyId);
        stop ->
            ok;
    {ok, _} ->
            %% Ignora mensajes de confirmación retrasados
            open(Nodes, MyId);
        Error ->
            io:format("open: unsupported message: ~w~n", [Error]),
            open(Nodes, MyId)
    end.

%% Envía solicitudes a todos los nodos peers incluyendo el MyId (prioridad)
requests(Nodes, MyId) ->
    lists:map(
      fun(P) -> 
        R = make_ref(), 
        P ! {request, self(), R, MyId}, % IMPORTANTE: Incluye MyId para resolver conflictos
        R 
      end, 
      Nodes).

%% Estado de espera: aguarda respuesta de todos los peers antes de tomar el lock
wait(Nodes, Master, [], Waiting, TakeRef, MyId) ->
    %% Cuando recibe todas las confirmaciones, toma el lock
    Master ! {taken, TakeRef},
    held(Nodes, Waiting, MyId);
wait(Nodes, Master, Refs, Waiting, TakeRef, MyId) ->
    receive
        {request, From, Ref, ReqId} ->
        
        %% CLAVE DEL ALGORITMO: Resolución de conflictos por prioridad de ID
        if ReqId < MyId ->
            %% Si el solicitante tiene mayor prioridad (ID menor), ceder paso
            From ! {ok, Ref},
            %% IMPORTANTE: Re-solicitar el lock al nodo priorizado
            R = make_ref(),
            From ! {request, self(), R, MyId},
            wait(Nodes, Master, [R|Refs], Waiting, TakeRef, MyId);
            
        true ->
            %% Si tiene menor prioridad, dejarlo en espera
            wait(Nodes, Master, Refs, [{From, Ref}|Waiting], TakeRef, MyId)
            end;
        {ok, Ref} ->
            %% Recibe confirmación de un nodo
            NewRefs = lists:delete(Ref, Refs),
            wait(Nodes, Master, NewRefs, Waiting, TakeRef, MyId);
        release ->
            %% Si se libera mientras espera, responder a todos los que esperan
            ok(Waiting),            
            open(Nodes, MyId);
    Error ->
            io:format("wait: unsupported message: ~w~n", [Error]),
            wait(Nodes, Master, Refs, Waiting, TakeRef, MyId)
    end.

%% Envía confirmaciones a todos los nodos en espera
ok(Waiting) ->
    lists:foreach(
      fun({F,R}) -> 
        F ! {ok, R} 
      end, 
      Waiting).

%% Estado "held": el lock está tomado por este proceso
held(Nodes, Waiting, MyId) ->
    receive
        {request, From, Ref, _} ->
            %% Guarda las solicitudes para responder cuando libere el lock
            held(Nodes, [{From, Ref}|Waiting], MyId);
        release ->
            %% Al liberar, responde a todos los que esperaban
            ok(Waiting),
            open(Nodes, MyId);
    {ok, _} ->
            %% Ignora mensajes de confirmación retrasados
            held(Nodes, Waiting, MyId);
    Error ->
            io:format("held: unsupported message: ~w~n", [Error]),
            held(Nodes, Waiting, MyId)
    end.