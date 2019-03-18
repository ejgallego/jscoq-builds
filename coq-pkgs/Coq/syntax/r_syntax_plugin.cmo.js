function(ap){"use strict";var
Z="Z",Y="Rdefinitions",X="positive",W="Reals",B="Coq",h=ap.jsoo_runtime,b=h.caml_new_string,V=h.caml_register_global,an=h.caml_wrap_exception;function
c(a,b){return a.length==1?a(b):h.caml_call_gen(a,[b])}function
a(a,b,c){return a.length==2?a(b,c):h.caml_call_gen(a,[b,c])}function
ao(a,b,c,d){return a.length==3?a(b,c,d):h.caml_call_gen(a,[b,c,d])}var
g=h.caml_get_global_data(),r=b("r_syntax_plugin"),k=[0,b(B),[0,b("Numbers"),[0,b("BinNums"),0]]],A=[0,b(B),[0,b(W),[0,b(Y),0]]],q=b("R_scope"),d=g.DAst,f=g.Names,e=g.Bigint,C=g.Mltop,D=g.Globnames,T=g.Notation,$=g.Libnames,_=g.Util;c(C[10],r);var
j=[248,b("R_syntax.Non_closed_number"),h.caml_fresh_oo_id(0)],aa=b(X),ac=b(X),af=b(Z),ah=b(Z),ak=b("R"),al=b("IZR"),am=[0,b(B),[0,b(W),[0,b(Y),0]]];function
l(b){var
d=a(_[17][14],f[1][6],b);return c(f[5][4],d)}function
s(d,b){var
e=c(f[1][6],b),g=l(d);return a($[15],g,e)}function
i(g,e){var
b=c(d[1],g);return 0===b[0]?a(f[68][1],b[1],e):0}var
ab=s(k,aa);function
t(c,b){return a(D[24],c,b)}var
ad=c(f[1][6],ac),m=t(l(k),ad),E=[0,[0,m,0],1],F=[0,[0,m,0],2],G=[0,[0,m,0],3],u=[3,E],v=[3,F],w=[3,G],ae=[2,[0,m,0]];function
H(g,f){var
h=a(d[3],0,[0,u,0]),i=a(d[3],0,[0,w,0]),j=a(d[3],0,[0,v,0]);function
b(k){var
g=c(e[8],k),f=g[1];if(0===g[2]){var
l=[4,j,[0,b(f),0]];return a(d[3],0,l)}if(a(e[17],f,e[5]))return i;var
m=[4,h,[0,b(f),0]];return a(d[3],0,m)}return b(f)}function
n(l){var
b=c(d[1],l);switch(b[0]){case
0:if(a(f[68][1],b[1],w))return e[6];break;case
4:var
g=b[2];if(g)if(!g[2]){var
h=g[1],k=b[1];if(i(k,v)){var
m=n(h);return c(e[11],m)}if(i(k,u)){var
o=n(h),p=c(e[11],o);return c(e[9],p)}}break}throw j}var
ag=s(k,af),ai=c(f[1][6],ah),o=t(l(k),ai),I=[0,[0,o,0],1],J=[0,[0,o,0],2],K=[0,[0,o,0],3],x=[3,I],y=[3,J],z=[3,K],aj=[2,[0,o,0]];function
L(h,b){if(a(e[17],b,e[5]))return a(d[3],0,[0,x,0]);if(c(e[20],b))var
g=y,f=b;else
var
g=z,f=c(e[22],b);var
i=[0,H(h,f),0],j=[4,a(d[3],0,[0,g,0]),i];return a(d[3],0,j)}function
M(l){var
b=c(d[1],l);switch(b[0]){case
0:if(a(f[68][1],b[1],x))return e[5];break;case
4:var
g=b[2];if(g)if(!g[2]){var
h=g[1],k=b[1];if(i(k,y))return n(h);if(i(k,z)){var
m=n(h);return c(e[22],m)}}break}throw j}var
N=s(A,ak);function
O(d,b){var
e=c(f[1][6],b);return a(D[26],d,e)}var
p=[1,O(l(A),al)];function
P(c,b){var
e=[0,L(c,b),0],f=[4,a(d[3],0,[0,p,0]),e];return a(d[3],0,f)}function
Q(e){var
a=c(d[1],e);if(4===a[0]){var
b=a[2];if(b)if(!b[2]){var
f=b[1];if(i(a[1],p))return M(f)}}throw j}function
R(a){var
b=a[1];try{var
c=[0,Q(b)];return c}catch(a){a=an(a);if(a===j)return 0;throw a}}function
S(d,b){function
e(a){return c(d,b)}return a(C[14],e,r)}ao(T[19],0,q,[0,P,R]);S(T[22],[0,0,q,[0,q],[0,N,am],[0,p,0],0]);var
U=[0,r,j,k,l,i,ab,t,m,ae,E,F,G,u,v,w,H,n,ag,o,aj,I,J,K,x,y,z,L,M,A,N,O,p,P,Q,R,S,q];V(20,U,"R_syntax_plugin.R_syntax");V(21,[0,U],"R_syntax_plugin");return}
