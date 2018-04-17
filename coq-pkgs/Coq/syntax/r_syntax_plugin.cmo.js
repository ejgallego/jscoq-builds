function(ao){"use strict";var
V="Z",U="Rdefinitions",T="positive",S="Reals",A="Coq",g=ao.jsoo_runtime,b=g.caml_new_string,R=g.caml_register_global,am=g.caml_wrap_exception;function
c(a,b){return a.length==1?a(b):g.caml_call_gen(a,[b])}function
a(a,b,c){return a.length==2?a(b,c):g.caml_call_gen(a,[b,c])}function
an(a,b,c,d,e){return a.length==4?a(b,c,d,e):g.caml_call_gen(a,[b,c,d,e])}var
f=g.caml_get_global_data(),B=b("r_syntax_plugin"),k=[0,b(A),[0,b("Numbers"),[0,b("BinNums"),0]]],z=[0,b(A),[0,b(S),[0,b(U),0]]],d=f.DAst,m=f.Globnames,e=f.Bigint,h=f.Names,X=f.Libnames,W=f.Util,al=f.Notation;c(f.Mltop[10],B);var
j=[248,b("R_syntax.Non_closed_number"),g.caml_fresh_oo_id(0)],Y=b(T),_=b(T),ab=b(V),ad=b(V),ag=b("R"),ah=b("IZR"),aj=[0,b(A),[0,b(S),[0,b(U),0]]],ak=b("R_scope");function
l(b){var
d=a(W[17][17],h[1][6],b);return c(h[5][4],d)}function
r(d,b){var
e=c(h[1][6],b),f=l(d);return a(X[17],f,e)}function
i(f,e){var
b=c(d[1],f);return 0===b[0]?a(m[5],b[1],e):0}var
Z=r(k,Y);function
s(c,b){return a(m[25],c,b)}var
$=c(h[1][6],_),n=s(l(k),$),C=[0,[0,n,0],1],D=[0,[0,n,0],2],E=[0,[0,n,0],3],t=[3,C],u=[3,D],v=[3,E],aa=[2,[0,n,0]];function
F(g,f){var
h=a(d[3],0,[0,t,0]),i=a(d[3],0,[0,v,0]),j=a(d[3],0,[0,u,0]);function
b(k){var
g=c(e[8],k),f=g[1];if(0===g[2]){var
l=[4,j,[0,b(f),0]];return a(d[3],0,l)}if(a(e[17],f,e[5]))return i;var
m=[4,h,[0,b(f),0]];return a(d[3],0,m)}return b(f)}function
o(k){var
b=c(d[1],k);switch(b[0]){case
0:if(a(m[5],b[1],v))return e[6];break;case
4:var
f=b[2];if(f)if(!f[2]){var
g=f[1],h=b[1];if(i(h,u)){var
l=o(g);return c(e[11],l)}if(i(h,t)){var
n=o(g),p=c(e[11],n);return c(e[9],p)}}break}throw j}var
ac=r(k,ab),ae=c(h[1][6],ad),p=s(l(k),ae),G=[0,[0,p,0],1],H=[0,[0,p,0],2],I=[0,[0,p,0],3],w=[3,G],x=[3,H],y=[3,I],af=[2,[0,p,0]];function
J(h,b){if(a(e[17],b,e[5]))return a(d[3],0,[0,w,0]);if(c(e[20],b))var
g=x,f=b;else
var
g=y,f=c(e[22],b);var
i=[0,F(h,f),0],j=[4,a(d[3],0,[0,g,0]),i];return a(d[3],0,j)}function
K(k){var
b=c(d[1],k);switch(b[0]){case
0:if(a(m[5],b[1],w))return e[5];break;case
4:var
f=b[2];if(f)if(!f[2]){var
g=f[1],h=b[1];if(i(h,x))return o(g);if(i(h,y)){var
l=o(g);return c(e[22],l)}}break}throw j}var
L=r(z,ag);function
M(d,b){var
e=c(h[1][6],b);return a(m[27],d,e)}var
q=[1,M(l(z),ah)];function
N(c,b){var
e=[0,J(c,b),0],f=[4,a(d[3],0,[0,q,0]),e];return a(d[3],0,f)}function
O(e){var
a=c(d[1],e);if(4===a[0]){var
b=a[2];if(b)if(!b[2]){var
f=b[1];if(i(a[1],q))return K(f)}}throw j}function
P(a){var
b=a[1];try{var
c=[0,O(b)];return c}catch(a){a=am(a);if(a===j)return 0;throw a}}var
ai=[0,[0,a(d[3],0,[0,q,0]),0],P,0];an(al[14],ak,[0,L,aj],N,ai);var
Q=[0,B,j,k,l,i,Z,s,n,aa,C,D,E,t,u,v,F,o,ac,p,af,G,H,I,w,x,y,J,K,z,L,M,q,N,O,P];R(20,Q,"R_syntax_plugin.R_syntax");R(21,[0,Q],"R_syntax_plugin");return}
