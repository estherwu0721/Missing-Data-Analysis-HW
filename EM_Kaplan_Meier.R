
p1=1/7
p2=1/7
p3=1/7
p4=1/7
p5=1/7
p6=1/7
p7=1/7

p.old=c(p1,p2,p3,p4,p5,p6,p7)

diff=1
count=0
while(diff>0.001){
w1=1
w2=1
w3=1+p3/(p3+p4+p5+p6+p7)
w4=1+p4/(p3+p4+p5+p6+p7)
w5=1+p5/(p3+p4+p5+p6+p7)+p5/(p5+p6+p7)
w6=1+p6/(p3+p4+p5+p6+p7)+p6/(p5+p6+p7)
w7=p7/(p3+p4+p5+p6+p7)+p7/(p5+p6+p7)+3

p1=w1/11
p2=w2/11
p3=w3/11
p4=w4/11
p5=w5/11
p6=w6/11
p7=w7/11

p.new=c(p1,p2,p3,p4,p5,p6,p7)
diff=max(abs(p.new-p.old))
p.old=p.new
count=count+1
}
