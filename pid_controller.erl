-module(pid_controller).
-export([start/0, pid_loop/5, measure_throughput/2]).

start() ->
    Setpoint = 1000,  % Desired throughput (e.g., requests per second)
    Kp = 0.1,         % Proportional gain
    Ki = 0.01,        % Integral gain
    Kd = 0.05,        % Derivative gain
    InitialProcesses = 10,  % Initial number of processes
    
    Pid = spawn(?MODULE, pid_loop, [Setpoint, Kp, Ki, Kd, InitialProcesses]),
    register(pid_controller, Pid),
    
    spawn(?MODULE, measure_throughput, [InitialProcesses, Setpoint]).

pid_loop(Setpoint, Kp, Ki, Kd, CurrentProcesses) ->
    receive
        {current_throughput, Throughput} ->
            Error = Setpoint - Throughput,
            
            Integral = case get(integral) of
                           undefined -> 0;
                           ExistingIntegral -> ExistingIntegral
                       end + Error,
            put(integral, Integral),
            
            % Update derivative term
            LastError = case get(last_error) of
                            undefined -> 0;
                            ExistingLastError -> ExistingLastError
                        end,
            Derivative = Error - LastError,
            put(last_error, Error),
            
            P = Kp * Error,
            I = Ki * Integral,
            D = Kd * Derivative,
            Output = P + I + D,
            
            % Ensure at least 1 process
            NewProcesses = max(1, round(CurrentProcesses + Output)), 
            
            io:format("Throughput: ~p, Error: ~p, Processes: ~p~n", [Throughput, Error, NewProcesses]),
            
            % Spawn new processes or terminate existing ones
            adjust_processes(CurrentProcesses, NewProcesses),
            
            pid_loop(Setpoint, Kp, Ki, Kd, NewProcesses)
    end.

%% Simulate measuring throughput with random variation
measure_throughput(CurrentProcesses, Setpoint) ->
    BaseThroughput = CurrentProcesses * 100,  % Base throughput per process
    Variation = rand:uniform(200) - 100,      % Random variation between -100 and +100
    Throughput = BaseThroughput + Variation,  % Add variation to base throughput
    timer:sleep(1000),  
    
    pid_controller ! {current_throughput, Throughput},
    
    measure_throughput(CurrentProcesses, Setpoint).

%% Adjust the number of processes
adjust_processes(CurrentProcesses, NewProcesses) ->
    if
        NewProcesses > CurrentProcesses -> 
            lists:foreach(fun(_) -> spawn(fun worker/0) end, lists:seq(1, NewProcesses - CurrentProcesses));
        NewProcesses < CurrentProcesses ->
            io:format("Terminating ~p processes~n", [CurrentProcesses - NewProcesses]);
        true ->
            ok
    end.

worker() ->
    receive
        stop ->
            ok
    after
        1000 ->  % Simulate work by waiting for 1 second
            worker()
    end.