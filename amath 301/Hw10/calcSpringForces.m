function varargout = calcSpringForces(X,springList)
%calcSpringForces  Calculate forces on nodes with positions X due to springs.
%   F = calcSpringForces(X,springList) with X an N-by-3 matrix of node
%   positions and springList an M-by-4 list of springs; with each row
%   encoding a to- and from-node, a spring rest length, and spring
%   stiffness; calculates the forces on each node due to all springs.

X = reshape(X,[],3);
F = zeros(size(X));

% loop over all springs in the list
for jj=1:size(springList,1)
    myspring = springList(jj,:);
    n1 = myspring(1);
    n2 = myspring(2);
    restleng = myspring(3);
    k = myspring(4);
    
    v = X(n1,:) - X(n2,:);
    currleng = norm(v);
    springList(jj,5) = currleng;
    direction = v / currleng;
    stretch = currleng - restleng;
    
    % spring force is -k*x in the direction of the opposite node, where x
    % is stretch past rest length
    springF = -k*stretch*direction;
    
    F(n1,:) = F(n1,:) + springF;
    F(n2,:) = F(n2,:) - springF;
end

varargout = {F};

if nargout==2
    varargout{2} = springList;
end

end