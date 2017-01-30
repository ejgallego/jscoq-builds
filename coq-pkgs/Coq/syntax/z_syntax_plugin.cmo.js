(function(aB){"use strict";var
af="Z",ae="N",ad="positive",i=aB.jsoo_runtime,c=i.caml_new_string,ac=i.caml_register_global,C=i.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):i.caml_call_gen(a,[b])}function
d(a,b,c){return a.length==2?a(b,c):i.caml_call_gen(a,[b,c])}function
D(a,b,c,d,e){return a.length==4?a(b,c,d,e):i.caml_call_gen(a,[b,c,d,e])}var
f=i.caml_get_global_data(),E=c("z_syntax_plugin"),e=[0,c("Coq"),[0,c("Numbers"),[0,c("BinNums"),0]]],g=f.Globnames,b=f.Bigint,K=f.Pp,L=f.CErrors,l=f.Names,h=f.Loc,B=f.Notation,ah=f.Libnames,ag=f.Util;a(f.Mltop[12],E);var
j=[248,c("Z_syntax.Non_closed_number"),i.caml_fresh_oo_id(0)],at=c('No negative numbers in type "N".'),au=c("interp_N"),am=c('Only strictly positive numbers in type "positive".'),an=c("interp_positive"),ai=c(ad),aj=c(ad),ao=c("positive_scope"),ap=c(ae),as=c(ae),av=c("N_scope"),aw=c(af),ax=c(af),aA=c("Z_scope");function
m(b){var
c=d(ag[17][14],l[1][5],b);return a(l[5][4],c)}function
p(c,b){var
e=a(l[1][5],b),f=m(c);return d(ah[17],f,e)}var
F=p(e,ai);function
q(b,a){return d(g[25],b,a)}var
ak=a(l[1][5],aj),n=q(m(e),ak),G=[0,[0,n,0],1],H=[0,[0,n,0],2],I=[0,[0,n,0],3],r=[3,G],s=[3,H],t=[3,I],al=[2,[0,n,0]];function
u(c,f){var
h=[0,[0,c,r,0]],i=[0,[0,c,t,0]],j=[0,[0,c,s,0]];function
e(k){var
g=a(b[8],k),f=g[1];return 0===g[2]?[4,c,j,[0,e(f),0]]:d(b[17],f,b[5])?i:[4,c,h,[0,e(f),0]]}return e(f)}function
J(b){var
c=[0,b,an,a(K[1],am)];return a(L[8],c)}function
M(d,c){return a(b[18],c)?u(d,c):J(d)}function
k(c){switch(c[0]){case
0:if(d(g[5],c[1][2],t))return b[6];break;case
4:var
f=c[2];if(0===f[0]){var
e=c[3];if(e)if(!e[2]){var
h=e[1],i=f[1][2];if(d(g[5],i,s)){var
l=k(h);return a(b[11],l)}if(d(g[5],i,r)){var
m=k(h),n=a(b[11],m);return a(b[9],n)}}}break}throw j}function
N(a){try{var
b=[0,k(a)];return b}catch(a){a=C(a);if(a===j)return 0;throw a}}D(B[13],ao,[0,F,e],M,[0,[0,[0,[0,h[4],r,0]],[0,[0,[0,h[4],s,0]],[0,[0,[0,h[4],t,0]],0]]],N,1]);var
aq=a(l[1][5],ap),v=q(m(e),aq),O=[0,[0,v,0],1],P=[0,[0,v,0],2],w=[3,O],x=[3,P],Q=p(e,as),ar=[2,[0,v,0]];function
R(a,e,c){return d(b[17],c,b[5])?[0,[0,a,w,0]]:[4,a,[0,[0,a,x,0]],[0,u(a,c),0]]}function
S(b){var
c=[0,b,au,a(K[1],at)];return a(L[8],c)}function
T(d,c){return a(b[20],c)?R(d,1,c):S(d)}function
U(a){switch(a[0]){case
0:if(d(g[5],a[1][2],w))return b[5];break;case
4:var
e=a[2];if(0===e[0]){var
c=a[3];if(c)if(!c[2]){var
f=c[1];if(d(g[5],e[1][2],x))return k(f)}}break}throw j}function
V(a){try{var
b=[0,U(a)];return b}catch(a){a=C(a);if(a===j)return 0;throw a}}D(B[13],av,[0,Q,e],T,[0,[0,[0,[0,h[4],w,0]],[0,[0,[0,h[4],x,0]],0]],V,1]);var
W=p(e,aw),ay=a(l[1][5],ax),o=q(m(e),ay),X=[0,[0,o,0],1],Y=[0,[0,o,0],2],Z=[0,[0,o,0],3],y=[3,X],z=[3,Y],A=[3,Z],az=[2,[0,o,0]];function
_(e,c){if(d(b[17],c,b[5]))return[0,[0,e,y,0]];if(a(b[20],c))var
g=z,f=c;else
var
g=A,f=a(b[22],c);return[4,e,[0,[0,e,g,0]],[0,u(e,f),0]]}function
$(c){switch(c[0]){case
0:if(d(g[5],c[1][2],y))return b[5];break;case
4:var
f=c[2];if(0===f[0]){var
e=c[3];if(e)if(!e[2]){var
h=e[1],i=f[1][2];if(d(g[5],i,z))return k(h);if(d(g[5],i,A)){var
l=k(h);return a(b[22],l)}}}break}throw j}function
aa(a){try{var
b=[0,$(a)];return b}catch(a){a=C(a);if(a===j)return 0;throw a}}D(B[13],aA,[0,W,e],_,[0,[0,[0,[0,h[4],y,0]],[0,[0,[0,h[4],z,0]],[0,[0,[0,h[4],A,0]],0]]],aa,1]);var
ab=[0,E,j,e,m,p,F,q,n,al,G,H,I,r,s,t,u,J,M,k,N,v,ar,O,P,w,x,Q,R,S,T,U,V,W,o,az,X,Y,Z,y,z,A,_,$,aa];ac(26,ab,"Z_syntax_plugin.Z_syntax");ac(27,[0,ab],"Z_syntax_plugin");return});
