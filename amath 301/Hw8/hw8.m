clear; close; clc;
load("Plutonium.mat");

P = @(t) Pdata(t-1969);

% Problem 1
dt = 1;
diff_1980 = ( P(1980 + dt) - P(1980 - dt))/(2*dt);
save("A1.dat", "diff_1980", "-ASCII");

%Problem 2
diff_1970 = (P(1971) - P(1970))/dt;
save("A2.dat", "diff_1970", "-ASCII");

%Problem 3
diff_2010 = (P(2010) - P(2009))/dt;
save("A3.dat", "diff_1970", "-ASCII");

%Problem 4
n = length(tdata);
diff_all = zeros(n, 1);

diff_all(1) = (P(1971) - P(1970))/dt;
for i = 2:(n - 1)
    diff_all(i) = (P(1969 + i + 1) - P(1969 + i - 1))/(tdata(i+1) - tdata(i-1));
end
diff_all(n) = (P(1969 + n) - P(1969 + n - 1))/(tdata(n) - tdata(n-1));

save("A4.dat" , "diff_all" , "-ASCII") ;

%%prob 5
plutonium_rate = zeros(n, 1);
for i = 1 : n
  plutonium_rate(i) = (-1/P(1969 + i))*diff_all(i);
end

avg_rate = mean(plutonium_rate) ;
save("A5.dat","avg_rate","-ASCII")
%%prob 6
lambda = avg_rate ;
t_half = log(2)/lambda ;
save("A6.dat","t_half","-ASCII");

%%prob 7
theory_P = @(t) 1000*exp((-lambda*(t - 1970)));
subplot(2,1,1);
title('Radioactive Decay of Plutonium');
hold on;
plot(tdata,P(tdata),'k+');
plot(tdata,theory_P(tdata));
legend('Data','Exponential Decay');
ylabel('Mass of Plutonium');
subplot(2,1,2);
plot(tdata, (theory_P(tdata) - P(tdata)));
xlabel('Time');
ylabel('Error');
print('plutonium_decay','-dpng');


% prob 8
dx = 1 ;
xc = 2:dx:4 ;
P = @(x) exp(-((x-1).^2)/(2*4));
Py = P(xc);
n = length(xc) ;
area = 0 ;
for i = 1:(n-1)
    area = area + Py(i) * dx ;
end
save("B1.dat","area","-ASCII");


final_area = zeros(17, 1) ;
for ind = 0:16
    dx = 2^(-ind);
    xc = 2:dx:4;
    Py = P(xc);
    area = 0 ;
    n = length(xc) ;
    for i = 1:(n-1)
        area = area + Py(i) * dx ;
    end
    final_area(ind + 1) = area ;
end
save("B2.dat","final_area","-ASCII");

final_area1 = zeros(17, 1) ;
for ind = 0:16
    dx = 2^(-ind) ;
    xc = 2:dx:4;
    Py = P(xc);
    area1 = 0 ;
    n = length(xc) ;
    for i = 1:(n-1)
        area1 = area1 + Py(i + 1) * dx ;
    end
    final_area1(ind + 1) = area1 ;
end
save("B3.dat","final_area1","-ASCII");

final_area2 = zeros(17, 1) ;
for ind = 0:16
    dx = 2^(-ind) ;
    xc = 2:dx:4;
    Py = P(xc);
    area2 = 0 ;
    n = length(xc);
    for i = 1:(n-1)
        area2 = area2 + (Py(i) + Py(i + 1)) * dx/2 ;
    end
    final_area2(ind + 1) = area2 ;
end
save("B4.dat","final_area2","-ASCII") ;

total_area3 = zeros(17, 1)  ;
for ind = 0:16
    dx = 2^(-ind)  ;
    total_area3(ind + 1) = simpson(P,2,4,dx) ;
end
save("B5.dat","final_area3","-ASCII") ;

true_vaue = integral(P, 2, 4) ;
save("B6.dat", "true_value", "-ASCII") ;

clf;
dx = zeros(17,1);
for z = 1:17
  j = -(z-1);
  dx(z,1) = 2^j;
end

xaxis = logspace(-4,0,17);
%true_integral is the result from Problem 11
lram_error = abs(area2 - true_integral); %answer8 is the 17x1 vector from left rectangular rule
rram_error = abs(true_integral - final_area1); %answer8_2 is the 17x1 vector from right rectangular rule
tram_error = abs(true_integral - final_area2); %answer9 is the 17x1 vector from trapezoid rule
simpson_error = abs(total_area3 - true_integral); %answer10 is the result from simpson's rule

loglog(dx,lram_error,'.k','Markersize',[20]);
hold on;
loglog(dx,rram_error,'.r');
loglog(dx,tram_error,'.g');
loglog(dx,.0000000005.*(dx).^3,'.b');
loglog(xaxis,simpson_error,'.b');

loglog(dx,0.5.*dx,'k-'); %O(N) %LRAM + RRAM
hold on
loglog(dx,0.005.*(dx).^2,'--k'); %O(N^2) %trapezoid
loglog(dx,.000000005.*(dx).^3,'.k'); %O(N^3) %simpson's rule

xr = linspace(10^(-6),1);
nline = length(xr);
yr = repmat(10^(-16),1,nline);
plot(xr,yr,'-g');

axis([10^(-5) 1 10^(-17) 1]);
xticks([10^(-4) 10^(-2) 10^(0)]);
xlabel('Grid spacing Deltax');
ylabel('Error');
title('Convergence of quadrature schemes');
legend('Left Rectangle','Right Rectangle','Trapezoid Rule',"Simpson's Rule",'O(N)','O(N^2)','O(N^3)','Machine Error','Location','southeastoutside');

print('quadrature_errors','-dpng');

%%
function int = simpson(f,a,b,dx)
xc = a:dx:b;
n = length(xc);
x = zeros(1,n+1);
x(1) = a;
x(n+1) = b;
p1 = 0; p2 = 0; p3 = 0;

for j = 2:n
    x(j) = a+dx*(j-1);
end
for j = 1:n/2
    p1 = p1 + f(x(2*j-1));
    p2 = p2 + f(x(2*j));
    p3 = p3 + f(x(2*j+1));
end
int = (dx/3)*(p1+4*p2+p3);
end
%%
