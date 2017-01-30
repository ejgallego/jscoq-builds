(function(ac){"use strict";var
L="Rdefinitions",J="R",K="Reals",I="Coq",j=ac.jsoo_runtime,d=j.caml_new_string,H=j.caml_register_global,aa=j.caml_wrap_exception;function
e(a,b){return a.length==1?a(b):j.caml_call_gen(a,[b])}function
a(a,b,c){return a.length==2?a(b,c):j.caml_call_gen(a,[b,c])}function
ab(a,b,c,d,e){return a.length==4?a(b,c,d,e):j.caml_call_gen(a,[b,c,d,e])}var
i=j.caml_get_global_data(),v=d("r_syntax_plugin"),b=i.Bigint,c=i.Globnames,p=i.Names,x=i.Util,X=i.Loc,O=i.Libnames,$=i.Notation;e(i.Mltop[12],v);var
m=[248,d("R_syntax.Non_closed_number"),j.caml_fresh_oo_id(0)],M=[0,d(I),[0,d(K),[0,d(L),0]]],P=d(J),Q=d(J),S=d("R1"),T=d("R0"),U=d("Ropp"),V=d("Rplus"),W=d("Rmult"),Z=[0,d(I),[0,d(K),[0,d(L),0]]],_=d("R_scope");function
w(b){var
c=a(x[17][14],p[1][5],b);return e(p[5][4],c)}var
g=w(M),N=e(p[1][5],P),y=a(O[17],g,N);function
k(d,b){var
f=e(p[1][5],b);return a(c[27],d,f)}var
z=k(g,Q),f=[1,k(g,S)],q=[1,k(g,T)],r=[1,k(g,U)],h=[1,k(g,V)],n=[1,k(g,W)],l=e(b[11],b[6]),A=e(b[9],l),B=e(b[11],l),R=[1,z];function
s(c,d){return a(b[17],b[6],d)?[0,[0,c,f,0]]:[4,c,[0,[0,c,h,0]],[0,[0,[0,c,f,0]],[0,s(c,e(b[10],d)),0]]]}function
t(c,d){var
j=[0,[0,c,f,0]],k=s(c,l);function
g(d){if(a(b[16],d,B))return s(c,d);var
f=e(b[8],d),l=f[2],i=[4,c,[0,[0,c,n,0]],[0,k,[0,g(f[1]),0]]];return l?[4,c,[0,[0,c,h,0]],[0,j,[0,i,0]]]:i}return a(b[17],d,b[5])?[0,[0,c,q,0]]:g(d)}function
C(a,c){return e(b[19],c)?[4,a,[0,[0,a,r,0]],[0,t(a,e(b[22],c)),0]]:t(a,c)}function
o(q){if(4===q[0]){var
x=q[2];if(0===x[0]){var
g=q[3];if(g){var
i=g[1],j=x[1][2];if(0===i[0]){var
k=g[2];if(k){var
p=k[1],y=i[1][2];switch(p[0]){case
0:if(k[2])var
d=1;else{var
E=p[1][2];if(a(c[5],j,h))if(a(c[5],y,f))if(a(c[5],E,f))return l;var
d=0}break;case
4:var
B=p[2];if(0===B[0]){var
v=p[3];if(v){var
C=v[1];if(0===C[0]){var
w=v[2];if(w){var
D=w[1];if(0===D[0])if(w[2])var
d=0;else
if(k[2])var
d=1;else{var
P=D[1][2],Q=C[1][2],R=B[1][2];if(a(c[5],j,h))if(a(c[5],R,h))if(a(c[5],y,f))if(a(c[5],Q,f))if(a(c[5],P,f))return A;var
d=0}else
var
d=0}else
var
d=0}else
var
d=0}else
var
d=0}else
var
d=0;break;default:var
d=0}}else
var
d=1}else
var
d=0;if(!d){var
r=g[2];if(r)if(!r[2]){var
F=r[1];if(a(c[5],j,n)){var
G=o(i);if(1-a(b[17],G,l))throw m;var
H=o(F);return e(b[11],H)}if(0===i[0]){var
s=g[2][1];if(4===s[0]){var
z=s[2];if(0===z[0]){var
t=s[3];if(t){var
u=t[2];if(u)if(!u[2]){var
I=u[1],J=t[1],K=z[1][2],L=i[1][2];if(a(c[5],j,h))if(a(c[5],K,n))if(a(c[5],L,f)){var
M=o(J);if(1-a(b[17],M,l))throw m;var
N=o(I),O=e(b[11],N);return e(b[9],O)}}}}}}}}}}}throw m}function
u(d){if(0===d[0]){var
e=d[1][2];if(a(c[5],e,q))return b[5];if(a(c[5],e,f))return b[6]}return o(d)}function
D(d){if(4===d[0]){var
g=d[2];if(0===g[0]){var
f=d[3];if(f)if(!f[2]){var
i=f[1];if(a(c[5],g[1][2],r)){var
h=u(i);if(a(b[17],h,b[5]))throw m;return e(b[22],h)}}}}return u(d)}function
E(a){try{var
b=[0,D(a)];return b}catch(a){a=aa(a);if(a===m)return 0;throw a}}function
F(a){return[0,[0,X[4],a,0]]}var
Y=[0,a(x[17][12],F,[0,r,[0,q,[0,h,[0,n,[0,f,0]]]]]),E,0];ab($[13],_,[0,y,Z],C,Y);var
G=[0,v,m,w,g,y,k,z,R,f,q,r,h,n,l,A,B,s,t,C,u,D,E,F];H(20,G,"R_syntax_plugin.R_syntax");H(21,[0,G],"R_syntax_plugin");return});
