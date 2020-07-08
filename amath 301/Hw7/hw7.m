clear; close all; clc ;

x = linspace(-5,5,31) ;
y = linspace(-5,5,31) ;
[X,Y] = meshgrid(x,y) ;
fsurf = (X.^2 + Y - 11).^2 + (X + Y.^2 - 7).^2 ;
surf(X,Y,fsurf) ;
title('Beale Function') ;
ylabel('y') ;
xlabel('x') ;
zlim([0 500]) ;
caxis([0 500]) ;
colorbar ;
view(-70,30) ;
daspect([1 1 100]) ;
print('beale_surf','-dpng') ;

x = linspace(-7,7,100) ;
y = linspace(-6,6,100) ;
[X1,Y1] = meshgrid(x,y) ;
Z = (X1.^2 + Y1 - 11).^2 + (X1 + Y1.^2 - 7).^2 ;
level = logspace(-1,3, 21) ;
contourf(X1,Y1,Z,level) ;
title('Contours of the Beale Function') ;
ylabel('y') ;
xlabel('x') ;
colormap default ;
caxis([0 500]) ;
colorbar ;
print('beale_contour','-dpng') ;

f = @(x) (x(1)^2 + x(2) - 11)^2 + (x(1) + x(2)^2 - 7)^2 ;
a1 = f([1 1]) ;
save('A1.dat','a1','-ASCII') ;

grad_f = @(p) [4*p(1)*(p(1)^2+p(2)-11) + 2*(p(1) + p(2)^2 - 7) 2*(p(1)^2+p(2)-11) + 4*p(2)*(p(1) + p(2)^2 - 7)] ;
a_2 = grad_f([1 1]) ;
save('A2.dat','a_2','-ASCII') ;
a_3 = norm(a_2,Inf) ;
save('A3.dat','a_3','-ASCII') ;

pi_f = @(t) [1 1].' - t*a_2 ;
a_4 = pi_f(0.1) ;
a_5 = f(a_4.') ;
save('A4.dat','a_4','-ASCII') ;
save('A5.dat','a_5','-ASCII') ;

composed_function = @(t) f(pi_f(t).') ;
a_6 = fminbnd(composed_function,0,0.1) ;
a_7 = pi_f(a_6) ;
save('A6.dat','a_6','-ASCII') ;
save('A7.dat','a_7','-ASCII') ;

tau = a_6 ;
p = a_7.' ;
for j = 1:1000
    pi_f2 = @(t) p.' - t*grad_f(p) ;
    composed_function2 = @(t) f(pi_f2(t).') ;
    new_gradient = grad_f(p) ;
    if norm(new_gradient,Inf) < 10^(-4)
        break ;
    end
    tau = fminbnd(composed_function2,0,tau) ;
    p = pi_f2(tau).' ;
end
p_2 = p.' ;
last_iterate = j-1 ;
save('A8.dat','p_2','-ASCII') ;
save('A9.dat','last_iterate','-ASCII') ;

coordinates1 = fminsearch(f,[4 2]) ;
coordinates2 = fminsearch(f,[-3 4]) ;
coordinates3 = fminsearch(f,[-4 -3]) ;
coordinates4 = fminsearch(f,[4 -2]) ;
local_minima_coordinates = [coordinates1.' coordinates2.' coordinates3.'  coordinates4.' ] ;
save('A10.dat','local_minima_coordinates','-ASCII') ;
global_minima = min([coordinates1 ;coordinates2 ;coordinates3 ;coordinates4 ;p]) ;

x = linspace(-7,7,100) ;
y = linspace(-6,6,100) ;
[X1,Y1] = meshgrid(x,y) ;
Z = (X1.^2 + Y1 - 11).^2 + (X1 + Y1.^2 - 7).^2 ;
level = logspace(-1,3, 21) ;
contourf(X1,Y1,Z,level) ;
hold on;
plot(coordinates1(1),coordinates1(2),'ro') ;
plot(coordinates2(1),coordinates2(2),'ro') ;
plot(coordinates3(1),coordinates3(2),'ro') ;
plot(coordinates4(1),coordinates4(2),'ro') ;
plot(p(1),p(2),'y*') ;
plot(global_minima(1),global_minima(2),'g+') ;
title('Minima of the Beale Function') ;
ylabel('y') ;
xlabel('x') ;
colormap default ;
caxis([0 500]) ;
colorbar ;
print('beale_minima','-dpng') ;
