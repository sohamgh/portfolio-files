clc; clear;

f_impact = @(x) ((100*sin(2*x))/9.8);
minusF = @(x) -f_impact(x);
optimal = fminbnd(minusF,0,pi);
max = f_impact(optimal);
save('A1.dat','optimal','-ASCII');
save('A2.dat','max','-ASCII');

f_x_horizontal = @(x) ((10*cos(x)*(10*sin(x) + sqrt(100*(sin(x)^2) + 196)))/9.8);
minusF2 = @(x) -f_x_horizontal(x);
optimal_horizontal_angle = fminbnd(minusF2,0,pi/2);
max_horizontal = f_x_horizontal(optimal_horizontal_angle);
save('A3.dat','optimal_horizontal_angle','-ASCII');
save('A4.dat','max_horizontal','-ASCII');

e4 = ones(29,1);
e5 = -2*ones(30,1);
A = diag(e5) + diag(e4,-1) + diag(e4,1);
D = diag(diag(A));
L = tril(A) - D;
U = triu(A) - D;
build_M = @(w) norm(abs(eig(-(D + w*L)\(w*U + (w-1)*D))), Inf);
minimum_o = fminbnd(build_M,1,1.9);
large_eig = build_M(minimum_o);
save('A5.dat','minimum_o','-ASCII');
save('A6.dat','large_eig','-ASCII');
