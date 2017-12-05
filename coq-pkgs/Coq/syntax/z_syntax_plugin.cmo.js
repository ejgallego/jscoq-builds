(function(aM){"use strict";var
ag="Z",af="N",ae="positive",i=aM.jsoo_runtime,e=i.caml_new_string,ad=i.caml_register_global,C=i.caml_wrap_exception;function
c(a,b){return a.length==1?a(b):i.caml_call_gen(a,[b])}function
a(a,b,c){return a.length==2?a(b,c):i.caml_call_gen(a,[b,c])}function
ac(a,b,c,d){return a.length==3?a(b,c,d):i.caml_call_gen(a,[b,c,d])}function
D(a,b,c,d,e){return a.length==4?a(b,c,d,e):i.caml_call_gen(a,[b,c,d,e])}var
g=i.caml_get_global_data(),E=e("z_syntax_plugin"),f=[0,e("Coq"),[0,e("Numbers"),[0,e("BinNums"),0]]],h=g.Globnames,d=g.Bigint,b=g.CAst,K=g.Pp,L=g.CErrors,l=g.Names,B=g.Notation,ai=g.Libnames,ah=g.Util;c(g.Mltop[12],E);var
j=[248,e("Z_syntax.Non_closed_number"),i.caml_fresh_oo_id(0)],ax=e('No negative numbers in type "N".'),ay=[0,e("interp_N")],an=e('Only strictly positive numbers in type "positive".'),ao=[0,e("interp_positive")],aj=e(ae),ak=e(ae),as=e("positive_scope"),at=e(af),aw=e(af),aC=e("N_scope"),aD=e(ag),aE=e(ag),aL=e("Z_scope");function
m(b){var
d=a(ah[17][17],l[1][6],b);return c(l[5][4],d)}function
p(d,b){var
e=c(l[1][6],b),f=m(d);return a(ai[17],f,e)}var
F=p(f,aj);function
q(c,b){return a(h[25],c,b)}var
al=c(l[1][6],ak),n=q(m(f),al),G=[0,[0,n,0],1],H=[0,[0,n,0],2],I=[0,[0,n,0],3],r=[3,G],s=[3,H],t=[3,I],am=[2,[0,n,0]];function
u(e,g){var
i=a(b[1],e,[0,r,0]),j=a(b[1],e,[0,t,0]),k=a(b[1],e,[0,s,0]);function
f(l){var
h=c(d[8],l),g=h[1];if(0===h[2]){var
m=[4,k,[0,f(g),0]];return a(b[1],e,m)}if(a(d[17],g,d[5]))return j;var
n=[4,i,[0,f(g),0]];return a(b[1],e,n)}return f(g)}function
J(a){var
b=c(K[3],an);return ac(L[6],a,ao,b)}function
M(b,a){return c(d[18],a)?u(b,a):J(b)}function
k(e){function
f(b){switch(b[0]){case
0:if(a(h[5],b[1],t))return d[6];break;case
4:var
f=b[1][1];if(0===f[0]){var
e=b[2];if(e)if(!e[2]){var
g=e[1],i=f[1];if(a(h[5],i,s)){var
l=k(g);return c(d[11],l)}if(a(h[5],i,r)){var
m=k(g),n=c(d[11],m);return c(d[9],n)}}}break}throw j}return a(b[5],f,e)}function
N(a){try{var
b=[0,k(a)];return b}catch(a){a=C(a);if(a===j)return 0;throw a}}var
ap=[0,a(b[1],0,[0,t,0]),0],aq=[0,a(b[1],0,[0,s,0]),ap],ar=[0,[0,a(b[1],0,[0,r,0]),aq],N,1];D(B[14],as,[0,F,f],M,ar);var
au=c(l[1][6],at),v=q(m(f),au),O=[0,[0,v,0],1],P=[0,[0,v,0],2],w=[3,O],x=[3,P],Q=p(f,aw),av=[2,[0,v,0]];function
R(e,h,c){if(a(d[17],c,d[5]))var
f=[0,w,0];else
var
g=[0,u(e,c),0],f=[4,a(b[1],0,[0,x,0]),g];return a(b[1],e,f)}function
S(a){var
b=c(K[3],ax);return ac(L[6],a,ay,b)}function
T(b,a){return c(d[20],a)?R(b,1,a):S(b)}function
az(b){switch(b[0]){case
0:if(a(h[5],b[1],w))return d[5];break;case
4:var
e=b[1][1];if(0===e[0]){var
c=b[2];if(c)if(!c[2]){var
f=c[1];if(a(h[5],e[1],x))return k(f)}}break}throw j}var
U=c(b[5],az);function
V(a){try{var
b=[0,c(U,a)];return b}catch(a){a=C(a);if(a===j)return 0;throw a}}var
aA=[0,a(b[1],0,[0,x,0]),0],aB=[0,[0,a(b[1],0,[0,w,0]),aA],V,1];D(B[14],aC,[0,Q,f],T,aB);var
W=p(f,aD),aF=c(l[1][6],aE),o=q(m(f),aF),X=[0,[0,o,0],1],Y=[0,[0,o,0],2],Z=[0,[0,o,0],3],y=[3,X],z=[3,Y],A=[3,Z],aG=[2,[0,o,0]];function
_(f,e){if(a(d[17],e,d[5]))return a(b[1],f,[0,y,0]);if(c(d[20],e))var
h=z,g=e;else
var
h=A,g=c(d[22],e);var
i=[0,u(f,g),0],j=[4,a(b[1],f,[0,h,0]),i];return a(b[1],f,j)}function
aH(b){switch(b[0]){case
0:if(a(h[5],b[1],y))return d[5];break;case
4:var
f=b[1][1];if(0===f[0]){var
e=b[2];if(e)if(!e[2]){var
g=e[1],i=f[1];if(a(h[5],i,z))return k(g);if(a(h[5],i,A)){var
l=k(g);return c(d[22],l)}}}break}throw j}var
$=c(b[5],aH);function
aa(a){try{var
b=[0,c($,a)];return b}catch(a){a=C(a);if(a===j)return 0;throw a}}var
aI=[0,a(b[1],0,[0,A,0]),0],aJ=[0,a(b[1],0,[0,z,0]),aI],aK=[0,[0,a(b[1],0,[0,y,0]),aJ],aa,1];D(B[14],aL,[0,W,f],_,aK);var
ab=[0,E,j,f,m,p,F,q,n,am,G,H,I,r,s,t,u,J,M,k,N,v,av,O,P,w,x,Q,R,S,T,U,V,W,o,aG,X,Y,Z,y,z,A,_,$,aa];ad(26,ab,"Z_syntax_plugin.Z_syntax");ad(27,[0,ab],"Z_syntax_plugin");return});
