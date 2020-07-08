clear; clc;

g = 9.8;
l = 10.0;

dthetadt = @(theta, phi) phi;
dphidt = @(theta, phi) sin(theta).*(-g/l);

theta0 = 3.1;
phi0 = 0;

%Problem 1(scorelator): done

ts = 0:0.1:100;
Z0 = [theta0; phi0];
odefun = @(t,Z) [ dthetadt(Z(1),Z(2));
                  dphidt(Z(1),Z(2)) ];

[tsol, Zsol] = ode45(odefun,ts,Z0);

thsave_ODE45 = Zsol(:,1);
phisave_ODE45 = Zsol(:,2);

answer_theta1 = zeros(500,1);
answer_phi1 = zeros(500,1);

for tt = 1:500
    answer_theta1(tt,1) = thsave_ODE45(tt,1);
    answer_phi1(tt,1) = phisave_ODE45(tt,1);
end

save('A1.dat','answer_theta1','-ascii');
save('A2.dat','answer_phi1','-ascii');

%Problem 2(writeup): done (but the graph looks really weird for theta)
clf;

t_axis = 0:0.1:100;

plot(t_axis,thsave_ODE45,'-b') ;
hold on ;
plot(t_axis,phisave_ODE45,'-r') ;

xlabel('Time (seconds)') ;
title('Physically Unrealistic Numerical Solution') ;
legend('\theta','\phi') ;

print('non_conservative_pendulum','-dpng') ;

%Problem 3(writeup) :
clf;

theta_arrows = -6:0.5:6;
phi_arrows = -4:0.5:4;

[x,y] = meshgrid(theta_arrows, phi_arrows);
quiver(x,y,dthetadt(x,y),dphidt(x,y));
hold on;

%Case 1
ts = 0:0.1:1000;
theta0 = 1; phi0 = 0;
Z0 = [theta0; phi0];
odefun = @(t,Z) [ dthetadt(Z(1),Z(2));
                  dphidt(Z(1),Z(2)) ];

[tsol, Zsol] = ode45(odefun,ts,Z0);

theta1 = Zsol(:,1);
phi1 = Zsol(:,2);

%Case 2
theta0 = 2; phi0 = 0;
Z0 = [theta0; phi0];
odefun = @(t,Z) [ dthetadt(Z(1),Z(2));
                  dphidt(Z(1),Z(2)) ];

[tsol, Zsol] = ode45(odefun,ts,Z0);

theta2 = Zsol(:,1);
phi2 = Zsol(:,2);

%Case 3
theta0 = 3.1; phi0 = 0;
Z0 = [theta0; phi0];
odefun = @(t,Z) [ dthetadt(Z(1),Z(2));
                  dphidt(Z(1),Z(2)) ];

[tsol, Zsol] = ode45(odefun,ts,Z0);

theta3 = Zsol(:,1);
phi3 = Zsol(:,2);

%final plot

plot(theta1,phi1,'.r');
plot(theta2,phi2,'.g');
plot(theta3,phi3,'.k');

axis([-6 6 -4 4]);
xlabel('\theta'); ylabel('\phi');
title('Phase Portrait with ode45() trajectories');

print('pendulum_phase_ode45','-dpng');
%%
%Problem 4(scorelator): done

dthetadt = @(t,theta,phi) phi;
dphidt = @(t,theta,phi) (-g/l)*sin(theta);

ts = 0:0.1:100;
[tsol_symp,thetasol_symp,phisol_symp] = symplecticEuler(dthetadt,dphidt,ts,theta0,phi0);

answer_theta4 = zeros(500,1);
answer_phi4 = zeros(500,1);

for tt = 1:500
    answer_theta4(tt,1) = thetasol_symp(tt,1);
    answer_phi4(tt,1) = phisol_symp(tt,1);
end

save('B1.dat','answer_theta4','-ascii');
save('B2.dat','answer_phi4','-ascii');

%Problem 5(writeup):
clf;

t_axis = 0:0.1:100;

plot(t_axis,thetasol_symp,'-b');
hold on;
plot(t_axis,phisol_symp,'-r');

xlabel('Time (seconds)');
title('Energy-Conserving Numerical Solution');
legend('\theta','\phi');

print('conservative_pendulum','-dpng');

%Problem 6(writeup): done
clf;

dthetadt = @(t,theta,phi) phi;
dphidt = @(t,theta,phi) (-g/l)*sin(theta);

t_axis = 0:0.1:100;

theta_arrows = -6:0.5:6;
phi_arrows = -4:0.5:4;

[x,y] = meshgrid(theta_arrows, phi_arrows);
quiver(x,y,dthetadt(t_axis,x,y),dphidt(t_axis,x,y));
hold on;

ts = 0:0.1:1000;

%Case 1
theta0 = 1; phi0 = 0;

[tsol_symp,thetasol_case1,phisol_case1] = symplecticEuler(dthetadt,dphidt,ts,theta0,phi0);

%Case 2
theta0 = 2; phi0 = 0;
[tsol_symp,thetasol_case2,phisol_case2] = symplecticEuler(dthetadt,dphidt,ts,theta0,phi0);

%Case 3
theta0 = 3.1; phi0 = 0;
[tsol_symp,thetasol_case3,phisol_case3] = symplecticEuler(dthetadt,dphidt,ts,theta0,phi0);

%final plot

plot(thetasol_case1,phisol_case1,'.r');
plot(thetasol_case2,phisol_case2,'.g');
plot(thetasol_case3,phisol_case3,'.k');

axis([-6 6 -4 4]);
xlabel('\theta'); ylabel('\phi');
title('Phase Portrait with Symplectic Euler Trajectories');

print('pendulum_phase_sympEuler_dt_0.1','-dpng');
%%

%Problem 7(scorelator): done

load('tprism_spec.mat');
F = calcSpringForces(nodes,springs);

save('C1.dat','F','-ascii');

%Problem 8(scorelator): wrong

c = 5;

dXdt = @(t,X,V) V;
dVdt = @(t,X,V) calcSpringForces(X,springs) - c.*V;

tspan = 0:0.01:50;
V0 = zeros(6,3);
X0 = nodes;

[tsol,Xsol,Vsol] = symplecticEuler(dXdt,dVdt,tspan,X0,V0);

save('D1.dat','Xsol','-ascii');
save('D2.dat','Vsol','-ascii');

%Problem 9(scorelator): wrong

Xrest = Xsol(end,:);

save('E1.dat','Xrest','-ascii');

%Problem 10(scorelator): F2 wrong

F_rest = calcSpringForces(Xrest,springs);
[F_rest10,new_length] = calcSpringForces(Xrest,springs);
new_length10 = new_length(:,end);

save('F1.dat','F_rest','-ascii');
save('F2.dat','new_length10','-ascii');

%Problem 11(writeup): done
clf;

drawSpringMassSystem(Xrest,springs);

view([-10 30]);
print('t_prism_side','-dpng');

view([0 90]);
print('t_prism_above','-dpng');
