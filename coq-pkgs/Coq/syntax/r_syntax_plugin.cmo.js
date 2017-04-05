(function(am){"use strict";var
T="Z",S="Rdefinitions",R="positive",Q="Reals",y="Coq",g=am.jsoo_runtime,a=g.caml_new_string,P=g.caml_register_global,ak=g.caml_wrap_exception;function
c(a,b){return a.length==1?a(b):g.caml_call_gen(a,[b])}function
d(a,b,c){return a.length==2?a(b,c):g.caml_call_gen(a,[b,c])}function
al(a,b,c,d,e){return a.length==4?a(b,c,d,e):g.caml_call_gen(a,[b,c,d,e])}var
f=g.caml_get_global_data(),z=a("r_syntax_plugin"),j=[0,a(y),[0,a("Numbers"),[0,a("BinNums"),0]]],x=[0,a(y),[0,a(Q),[0,a(S),0]]],e=f.Globnames,b=f.Bigint,h=f.Names,V=f.Libnames,U=f.Util,ag=f.Loc,aj=f.Notation;c(f.Mltop[12],z);var
i=[248,a("R_syntax.Non_closed_number"),g.caml_fresh_oo_id(0)],W=a(R),Y=a(R),$=a(T),ab=a(T),ae=a("R"),af=a("IZR"),ah=[0,a(y),[0,a(Q),[0,a(S),0]]],ai=a("R_scope");function
k(a){var
b=d(U[17][14],h[1][6],a);return c(h[5][4],b)}function
p(b,a){var
e=c(h[1][6],a),f=k(b);return d(V[17],f,e)}var
X=p(j,W);function
q(b,a){return d(e[25],b,a)}var
Z=c(h[1][6],Y),l=q(k(j),Z),A=[0,[0,l,0],1],B=[0,[0,l,0],2],C=[0,[0,l,0],3],r=[3,A],s=[3,B],t=[3,C],_=[2,[0,l,0]];function
D(a,f){var
h=[0,[0,a,r,0]],i=[0,[0,a,t,0]],j=[0,[0,a,s,0]];function
e(k){var
g=c(b[8],k),f=g[1];return 0===g[2]?[4,a,j,[0,e(f),0]]:d(b[17],f,b[5])?i:[4,a,h,[0,e(f),0]]}return e(f)}function
m(a){switch(a[0]){case
0:if(d(e[5],a[1][2],t))return b[6];break;case
4:var
g=a[2];if(0===g[0]){var
f=a[3];if(f)if(!f[2]){var
h=f[1],j=g[1][2];if(d(e[5],j,s)){var
k=m(h);return c(b[11],k)}if(d(e[5],j,r)){var
l=m(h),n=c(b[11],l);return c(b[9],n)}}}break}throw i}var
aa=p(j,$),ac=c(h[1][6],ab),n=q(k(j),ac),E=[0,[0,n,0],1],F=[0,[0,n,0],2],G=[0,[0,n,0],3],u=[3,E],v=[3,F],w=[3,G],ad=[2,[0,n,0]];function
H(e,a){if(d(b[17],a,b[5]))return[0,[0,e,u,0]];if(c(b[20],a))var
g=v,f=a;else
var
g=w,f=c(b[22],a);return[4,e,[0,[0,e,g,0]],[0,D(e,f),0]]}function
I(a){switch(a[0]){case
0:if(d(e[5],a[1][2],u))return b[5];break;case
4:var
g=a[2];if(0===g[0]){var
f=a[3];if(f)if(!f[2]){var
h=f[1],j=g[1][2];if(d(e[5],j,v))return m(h);if(d(e[5],j,w)){var
k=m(h);return c(b[22],k)}}}break}throw i}var
J=p(x,ae);function
K(b,a){var
f=c(h[1][6],a);return d(e[27],b,f)}var
o=[1,K(k(x),af)];function
L(a,b){return[4,a,[0,[0,a,o,0]],[0,H(a,b),0]]}function
M(a){if(4===a[0]){var
c=a[2];if(0===c[0]){var
b=a[3];if(b)if(!b[2]){var
f=b[1];if(d(e[5],c[1][2],o))return I(f)}}}throw i}function
N(a){try{var
b=[0,M(a)];return b}catch(a){a=ak(a);if(a===i)return 0;throw a}}al(aj[13],ai,[0,J,ah],L,[0,[0,[0,[0,ag[4],o,0]],0],N,0]);var
O=[0,z,i,j,k,X,q,l,_,A,B,C,r,s,t,D,m,aa,n,ad,E,F,G,u,v,w,H,I,x,J,K,o,L,M,N];P(20,O,"R_syntax_plugin.R_syntax");P(21,[0,O],"R_syntax_plugin");return});
