g = 9.8; l = 10;

%Prob 1

A = [0 1 ;
    -g/l 0] ;
dt = 0.01 ;
x0 = [1 ; 0] ;
xf(:,1) = x0 ;
tf(1) = 0 ;
T = 50 ;

for ii = 1 : T/dt
    tf(ii+1) = ii*dt ;
    xf(:,ii+1) = (eye(2) + dt*A)*xf(:,ii) ;
end
thsave_FE = xf(1,:)' ;
phisave_FE = xf(2,:)' ;
save("A1.dat", "thsave_FE", "-ASCII") ;
save("A2.dat", "phisave_FE", "-ASCII") ;

%Prob 2

A = [0 1 ;
    -g/l 0] ;
dt = 0.01 ;
x0 = [1; 0] ;
xb(:,1) = x0 ;
tb(1) = 0 ;
T = 50 ;

for ii = 1 : T/dt
    tb(ii+1) = ii*dt ;
    xb(:,ii+1) = inv(eye(2) - dt*A)*xb(:,ii) ;
end
theta_save_BE = xb(1,:)' ;
phi_save_BE = xb(2,:)' ;


save("B2.dat", "phi_save_BE", "-ASCII") ;


save("B1.dat", "theta_save_BE", "-ASCII") ;
%Prob3

xC(:,1) = x0; %creates vector to store theta and phi

LF_th_save = zeros(5001,1); %creates save vectors
LF_phi_save = zeros(5001,1) ;

xC(:,2) = (eye(2) + dt*A)*xC(:,1);

LF_th_save = xC(1,:);
LF_th_save = LF_th_save.';
LF_phi_save = xC(2,:);
LF_phi_save = LF_phi_save.';

for ii = 2:T/dt
    xC(:,ii+1) = xC(:,ii-1) + 2*dt*A*xC(:,ii) ;
    LF_th_save(ii+1) = xC(1,ii+1) ;
    LF_phi_save(ii+1) = xC(2,ii+1) ;
end

save('C1.dat','LF_th_save','-ascii');
save('C2.dat','LF_phi_save','-ascii');

%Prob4

dtheta = @(theta, phi) phi;
dphi= @(theta, phi) theta.*(-g/l);
ts = 0:0.01:50;
Zinit = [1; 0];
func4 = @(t,Z) [ dtheta(Z(1),Z(2));
                  dphi(Z(1),Z(2)) ];
[tfin, Zfin] = ode45(func4,ts,Zinit);
thsave_ODE45 = Zfin(:,1);
phisave_ODE45 = Zfin(:,2);
save("D1.dat", "thsave_ODE45", "-ASCII");
save("D2.dat", "phisave_ODE45", "-ASCII");

 %Problem 5
 clf;
 hold on;
 time = 0:5000;
 plot(time, thsave_FE, 'b');
 plot(time, theta_save_BE, 'r');
 plot(time, LF_th_save, 'g');
 plot(time, ones(5001,1).','k-');
 legend("Forward Euler", "Backward Euler", "Leap Frog");
 xlabel("time (seconds * 100)");
 ylabel("\theta")
 title("Linear Pendulum Solutions");
 print('linear_pendulum_solutions','-dpng');

 %Prob6
clf;
xvec = linspace(-2, 2, 21);
yvec = linspace(-2, 2, 21);
[phi, theeta] = meshgrid(xvec, yvec);
dtheeta = @(phi, theeta) phi;
dphi= @(phi, theeta) (-g/l)*theeta;
clf
quiver(theeta,phi, dtheeta(phi,theeta), dphi(phi,theeta));
axis([-2.0 2.0 -2.0 2.0]);
hold on;
plot(thsave_FE,phisave_FE,'b');
plot(theta_save_BE,phi_save_BE,'r');
plot(LF_th_save,LF_phi_save,'g');
legend('Phase Portrait','Forward Euler method','Backward Euler method','Leapfrog method','Location','Northwest');
ylabel("\theta");
xlabel("Time");
print('linear_phase_portrait','-dpng');

%Problem7
dtheta = @(theta, phi) phi;
dphi= @(theta, phi) sin(theta).*(-g/l);
ts = 0:0.01:50;
Zinit = [1; 0];
func4 = @(t,Z) [ dtheta(Z(1),Z(2));
                  dphi(Z(1),Z(2)) ];
[tfin, Zfin] = ode45(func4,ts,Zinit);
thsave_ODE45 = Zfin(:,1);
phisave_ODE45 = Zfin(:,2);
save('E1.dat','thsave_ODE45','-ascii');
save('E2.dat','phisave_ODE45','-ascii');

%Problem 8
clf;

th = linspace(-2*pi,2*pi,25);
ph = -3:0.5:4;
hold on;
[phi2,theeta2] = meshgrid(th, ph);
quiver(theeta2,phi2, dtheeta(phi2,theeta2), dphi(phi2,theeta2));

ts = 0:0.01:50;

Zsol = prob8(0.1,0);
Zsol = prob8(1.0,0);
Zsol = prob8(3.0,0);
Zsol = prob8((-2*pi),2.1);
Zsol = prob8(-2*pi,3);


 % Functions
function Zfin = prob8(a, b)
g = 9.8;
l = 10;
dtheta_new = @(theta, phi) phi;
dphi2= @(theta, phi) sin(theta)*(-g/l);
ts = 0:0.01:50;
Zinit = [a; b];
problem8_1 = @(t,Z) [ dtheta_new(Z(1),Z(2));
                  dphi2(Z(1),Z(2)) ];

[tfin, Zfin] = ode45(problem8_1,ts,Zinit);
ylabel("phi");
xlabel("Theta");

plot(Zfin(:,1),Zfin(:,2),'-r');
xticks([-6:1:6]) ;
yticks([-3 : 1: 4]) ;
end
