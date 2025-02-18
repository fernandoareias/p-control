-module(p_control).
-export([start/0, controller/3, adjust_threads/3]).


start() ->
    DesiredThroughput = 100, % Meta de desempenho (ex.: 100 requisições por segundo)
    InitialThreads = 5,      % Número inicial de processos no pool
    Kp = 0.5,                % Constante proporcional (ajuste conforme necessário)
    spawn(?MODULE, controller, [DesiredThroughput, InitialThreads, Kp]).

controller(DesiredThroughput, CurrentThreads, Kp) ->
    ActualThroughput = measure_throughput(), 

    Error = DesiredThroughput - ActualThroughput,

    % Ajusta o número de threads usando o P-Control
    NewThreads = adjust_threads(CurrentThreads, Error, Kp),

    % Atualiza o número de threads no sistema (simulação)
    update_thread_pool(NewThreads),

    % Intervalo de amostragem (1 segundo)
    timer:sleep(1000), 

    controller(DesiredThroughput, NewThreads, Kp).

%% Função para ajustar o número de threads com base no erro
% u(k) = Kp * Error
adjust_threads(CurrentThreads, Error, Kp) ->
    % Calcula a mudança no número de threads
    DeltaThreads = round(Kp * Error),

    MinThreads = 1,
    MaxThreads = 20,
    NewThreads = max(MinThreads, min(MaxThreads, CurrentThreads + DeltaThreads)),

    io:format("Error: ~p, DeltaThreads: ~p, NewThreads: ~p~n", [Error, DeltaThreads, NewThreads]),
    NewThreads.

%% Simulação da medição do throughput atual
measure_throughput() ->
    % Simulação aleatória para teste
    rand:uniform(150). 

update_thread_pool(NewThreads) ->
    io:format("Updating thread pool to ~p threads~n", [NewThreads]),
    ok.