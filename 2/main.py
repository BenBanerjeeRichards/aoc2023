import re
t=v=i=0
for s in open("i").readlines():
	i+=1
	p=lambda a:max([int(x)for x in re.findall("(\d+) "+a,s)])
	x,y,z=p("r"),p("g"),p("b")
	t+=i*all([y<14,x<13,z<15])
	v+=x*y*z
print(t,v)