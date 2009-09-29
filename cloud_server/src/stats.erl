-module(stats).

% Things we want to know:
%  Per node:
%   - Current throughput (OS)
%   - System load (OS)
%   - IOPS (OS)
%   - IO throughput (OS)
%   - Number of erlang processes (VM)
%   - How many sessions of each type are open (app)
%
%  Cluster wide:
%   - Aggregate of above metrics
%

