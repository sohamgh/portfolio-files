function [tsol,Xsol,Vsol] = symplecticEuler(f,g,tspan,X0,V0)
%symplecticEuler  Solve differential systems, low-order conservative method.
%   [tsol,Xsol,Vsol] = ODE45(dXdt,dVdt,TSPAN,Z0) with TSPAN of times integrates 
%   the system of differential equations x' = f(t,x,v) and v' = g(t,x,v)
%   with initial conditions Z0. For a scalar T and a vectors X and V,
%   dXdt(T,X,V) must return a column vector corresponding 
%   to f(t,x,v), and similarly dVdt(T,X,V) must return a column vector
%   corresponding to g(t,x,v). Each row in the solution array ZOUT
%   corresponds to a time returned in the column vector TOUT.

nvars = numel(X0);

nsteps = numel(tspan)-1;

Xsol = zeros(1+nsteps,nvars);
Vsol = zeros(1+nsteps,nvars);

X = X0;
V = V0;
Xsol(1+0,:) = X(:);
Vsol(1+0,:) = V(:);

dts = diff(tspan);
% loop
for nn=1:nsteps
    
    t = tspan(1+nn);
    dt = dts(nn);
    
    X = X + dt*f(t,X,V);
    V = V + dt*g(t,X,V);
    
    Xsol(1+nn,:) = X(:);
    Vsol(1+nn,:) = V(:);
end

tsol = tspan;

