function drawSpringMassSystem(X,springs)
hold on
axis equal
X = reshape(X,[],3);
for jj=1:size(springs,1)
    myspring = springs(jj,:);
    inds = myspring(1:2);
    
    
    lw = myspring(4);
    lw = log(1+lw);
    
    tmp = X(inds,:);
    currleng = norm(diff(tmp,[],1));
    stretch = currleng - myspring(3);
    
    myc = 'k';
    if stretch > 0
        myc = 'g';
    else
        myc = 'r';
    end
    
    plot3( X(inds,1),X(inds,2),X(inds,3), 'linewidth',lw,'color',myc);
end
plot3(X(:,1),X(:,2),X(:,3),'k.','markersize',40);
end