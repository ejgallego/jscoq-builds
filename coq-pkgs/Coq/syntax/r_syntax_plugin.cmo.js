(function(an){"use strict";var
U="Z",T="Rdefinitions",S="positive",R="Reals",z="Coq",h=an.jsoo_runtime,b=h.caml_new_string,Q=h.caml_register_global,al=h.caml_wrap_exception;function
d(a,b){return a.length==1?a(b):h.caml_call_gen(a,[b])}function
a(a,b,c){return a.length==2?a(b,c):h.caml_call_gen(a,[b,c])}function
am(a,b,c,d,e){return a.length==4?a(b,c,d,e):h.caml_call_gen(a,[b,c,d,e])}var
g=h.caml_get_global_data(),A=b("r_syntax_plugin"),k=[0,b(z),[0,b("Numbers"),[0,b("BinNums"),0]]],y=[0,b(z),[0,b(R),[0,b(T),0]]],f=g.Globnames,c=g.Bigint,e=g.CAst,i=g.Names,W=g.Libnames,V=g.Util,ak=g.Notation;d(g.Mltop[12],A);var
j=[248,b("R_syntax.Non_closed_number"),h.caml_fresh_oo_id(0)],X=b(S),Z=b(S),aa=b(U),ac=b(U),af=b("R"),ag=b("IZR"),ai=[0,b(z),[0,b(R),[0,b(T),0]]],aj=b("R_scope");function
l(b){var
c=a(V[17][17],i[1][6],b);return d(i[5][4],c)}function
q(c,b){var
e=d(i[1][6],b),f=l(c);return a(W[17],f,e)}var
Y=q(k,X);function
r(c,b){return a(f[25],c,b)}var
_=d(i[1][6],Z),m=r(l(k),_),B=[0,[0,m,0],1],C=[0,[0,m,0],2],D=[0,[0,m,0],3],s=[3,B],t=[3,C],u=[3,D],$=[2,[0,m,0]];function
E(g,f){var
h=a(e[1],0,[0,s,0]),i=a(e[1],0,[0,u,0]),j=a(e[1],0,[0,t,0]);function
b(k){var
g=d(c[8],k),f=g[1];if(0===g[2]){var
l=[4,j,[0,b(f),0]];return a(e[1],0,l)}if(a(c[17],f,c[5]))return i;var
m=[4,h,[0,b(f),0]];return a(e[1],0,m)}return b(f)}function
n(k){var
b=k[1];switch(b[0]){case
0:if(a(f[5],b[1],u))return c[6];break;case
4:var
g=b[1][1];if(0===g[0]){var
e=b[2];if(e)if(!e[2]){var
h=e[1],i=g[1];if(a(f[5],i,t)){var
l=n(h);return d(c[11],l)}if(a(f[5],i,s)){var
m=n(h),o=d(c[11],m);return d(c[9],o)}}}break}throw j}var
ab=q(k,aa),ad=d(i[1][6],ac),o=r(l(k),ad),F=[0,[0,o,0],1],G=[0,[0,o,0],2],H=[0,[0,o,0],3],v=[3,F],w=[3,G],x=[3,H],ae=[2,[0,o,0]];function
I(h,b){if(a(c[17],b,c[5]))return a(e[1],0,[0,v,0]);if(d(c[20],b))var
g=w,f=b;else
var
g=x,f=d(c[22],b);var
i=[0,E(h,f),0],j=[4,a(e[1],0,[0,g,0]),i];return a(e[1],0,j)}function
J(k){var
b=k[1];switch(b[0]){case
0:if(a(f[5],b[1],v))return c[5];break;case
4:var
g=b[1][1];if(0===g[0]){var
e=b[2];if(e)if(!e[2]){var
h=e[1],i=g[1];if(a(f[5],i,w))return n(h);if(a(f[5],i,x)){var
l=n(h);return d(c[22],l)}}}break}throw j}var
K=q(y,af);function
L(c,b){var
e=d(i[1][6],b);return a(f[27],c,e)}var
p=[1,L(l(y),ag)];function
M(c,b){var
d=[0,I(c,b),0],f=[4,a(e[1],0,[0,p,0]),d];return a(e[1],0,f)}function
N(e){var
b=e[1];if(4===b[0]){var
d=b[1][1];if(0===d[0]){var
c=b[2];if(c)if(!c[2]){var
g=c[1];if(a(f[5],d[1],p))return J(g)}}}throw j}function
O(a){try{var
b=[0,N(a)];return b}catch(a){a=al(a);if(a===j)return 0;throw a}}var
ah=[0,[0,a(e[1],0,[0,p,0]),0],O,0];am(ak[14],aj,[0,K,ai],M,ah);var
P=[0,A,j,k,l,Y,r,m,$,B,C,D,s,t,u,E,n,ab,o,ae,F,G,H,v,w,x,I,J,y,K,L,p,M,N,O];Q(20,P,"R_syntax_plugin.R_syntax");Q(21,[0,P],"R_syntax_plugin");return});
