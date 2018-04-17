function(aL){"use strict";var
ah="Z",ag="N",af="positive",h=aL.jsoo_runtime,d=h.caml_new_string,ae=h.caml_register_global,D=h.caml_wrap_exception;function
e(a,b){return a.length==1?a(b):h.caml_call_gen(a,[b])}function
a(a,b,c){return a.length==2?a(b,c):h.caml_call_gen(a,[b,c])}function
ad(a,b,c,d){return a.length==3?a(b,c,d):h.caml_call_gen(a,[b,c,d])}function
E(a,b,c,d,e){return a.length==4?a(b,c,d,e):h.caml_call_gen(a,[b,c,d,e])}var
g=h.caml_get_global_data(),F=d("z_syntax_plugin"),f=[0,d("Coq"),[0,d("Numbers"),[0,d("BinNums"),0]]],n=g.Globnames,c=g.Bigint,b=g.DAst,L=g.Pp,M=g.CErrors,k=g.Names,C=g.Notation,aj=g.Libnames,ai=g.Util;e(g.Mltop[10],F);var
i=[248,d("Z_syntax.Non_closed_number"),h.caml_fresh_oo_id(0)],ay=d('No negative numbers in type "N".'),az=[0,d("interp_N")],ao=d('Only strictly positive numbers in type "positive".'),ap=[0,d("interp_positive")],ak=d(af),al=d(af),at=d("positive_scope"),au=d(ag),ax=d(ag),aC=d("N_scope"),aD=d(ah),aE=d(ah),aK=d("Z_scope");function
m(b){var
c=a(ai[17][17],k[1][6],b);return e(k[5][4],c)}function
q(c,b){var
d=e(k[1][6],b),f=m(c);return a(aj[17],f,d)}var
G=q(f,ak);function
r(c,b){return a(n[25],c,b)}var
am=e(k[1][6],al),o=r(m(f),am),H=[0,[0,o,0],1],I=[0,[0,o,0],2],J=[0,[0,o,0],3],s=[3,H],t=[3,I],u=[3,J],an=[2,[0,o,0]];function
v(d,g){var
i=a(b[3],d,[0,s,0]),j=a(b[3],d,[0,u,0]),k=a(b[3],d,[0,t,0]);function
f(l){var
h=e(c[8],l),g=h[1];if(0===h[2]){var
m=[4,k,[0,f(g),0]];return a(b[3],d,m)}if(a(c[17],g,c[5]))return j;var
n=[4,i,[0,f(g),0]];return a(b[3],d,n)}return f(g)}function
K(a){var
b=e(L[3],ao);return ad(M[6],a,ap,b)}function
N(b,a){return e(c[18],a)?v(b,a):K(b)}function
l(f,d){var
c=e(b[1],f);return 0===c[0]?a(n[5],c[1],d):0}function
j(d){function
f(b){switch(b[0]){case
0:if(a(n[5],b[1],u))return c[6];break;case
4:var
d=b[2];if(d)if(!d[2]){var
f=d[1],g=b[1];if(l(g,t)){var
h=j(f);return e(c[11],h)}if(l(g,s)){var
k=j(f),m=e(c[11],k);return e(c[9],m)}}break}throw i}return a(b[8],f,d)}function
O(a){var
b=a[1];try{var
c=[0,j(b)];return c}catch(a){a=D(a);if(a===i)return 0;throw a}}var
aq=[0,a(b[3],0,[0,u,0]),0],ar=[0,a(b[3],0,[0,t,0]),aq],as=[0,[0,a(b[3],0,[0,s,0]),ar],O,1];E(C[14],at,[0,G,f],N,as);var
av=e(k[1][6],au),w=r(m(f),av),P=[0,[0,w,0],1],Q=[0,[0,w,0],2],x=[3,P],y=[3,Q],R=q(f,ax),aw=[2,[0,w,0]];function
S(e,h,d){if(a(c[17],d,c[5]))var
f=[0,x,0];else
var
g=[0,v(e,d),0],f=[4,a(b[3],0,[0,y,0]),g];return a(b[3],e,f)}function
T(a){var
b=e(L[3],ay);return ad(M[6],a,az,b)}function
U(b,a){return e(c[20],a)?S(b,1,a):T(b)}function
V(d){function
e(b){switch(b[0]){case
0:if(a(n[5],b[1],x))return c[5];break;case
4:var
d=b[2];if(d)if(!d[2]){var
e=d[1];if(l(b[1],y))return j(e)}break}throw i}return a(b[8],e,d)}function
W(a){var
b=a[1];try{var
c=[0,V(b)];return c}catch(a){a=D(a);if(a===i)return 0;throw a}}var
aA=[0,a(b[3],0,[0,y,0]),0],aB=[0,[0,a(b[3],0,[0,x,0]),aA],W,1];E(C[14],aC,[0,R,f],U,aB);var
X=q(f,aD),aF=e(k[1][6],aE),p=r(m(f),aF),Y=[0,[0,p,0],1],Z=[0,[0,p,0],2],_=[0,[0,p,0],3],z=[3,Y],A=[3,Z],B=[3,_],aG=[2,[0,p,0]];function
$(f,d){if(a(c[17],d,c[5]))return a(b[3],f,[0,z,0]);if(e(c[20],d))var
h=A,g=d;else
var
h=B,g=e(c[22],d);var
i=[0,v(f,g),0],j=[4,a(b[3],f,[0,h,0]),i];return a(b[3],f,j)}function
aa(d){function
f(b){switch(b[0]){case
0:if(a(n[5],b[1],z))return c[5];break;case
4:var
d=b[2];if(d)if(!d[2]){var
f=d[1],g=b[1];if(l(g,A))return j(f);if(l(g,B)){var
h=j(f);return e(c[22],h)}}break}throw i}return a(b[8],f,d)}function
ab(a){var
b=a[1];try{var
c=[0,aa(b)];return c}catch(a){a=D(a);if(a===i)return 0;throw a}}var
aH=[0,a(b[3],0,[0,B,0]),0],aI=[0,a(b[3],0,[0,A,0]),aH],aJ=[0,[0,a(b[3],0,[0,z,0]),aI],ab,1];E(C[14],aK,[0,X,f],$,aJ);var
ac=[0,F,i,f,m,q,G,r,o,an,H,I,J,s,t,u,v,K,N,l,j,O,w,aw,P,Q,x,y,R,S,T,U,V,W,X,p,aG,Y,Z,_,z,A,B,$,aa,ab];ae(26,ac,"Z_syntax_plugin.Z_syntax");ae(27,[0,ac],"Z_syntax_plugin");return}
