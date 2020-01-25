function(aD){"use strict";var
R="Z",Q="RbaseSymbolsImpl",A="Rdefinitions",z="",y="Reals",n="Coq",h=aD.jsoo_runtime,q=h.caml_ml_string_length,d=h.caml_new_string,P=h.caml_register_global,aC=h.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):h.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):h.caml_call_gen(a,[b,c])}function
r(a,b,c,d){return a.length==3?a(b,c,d):h.caml_call_gen(a,[b,c,d])}var
i=h.caml_get_global_data(),B=d("r_syntax_plugin"),x=d("R_scope"),e=i.DAst,f=i.Names,c=i.Bigint,C=i.Mltop,N=i.Stdlib,k=i.Util,O=i.Notation,aw=i.NumTok,V=i.Libnames;P(21,[0,0],"R_syntax_plugin");a(C[9],B);var
j=[248,d("R_syntax_plugin.R_syntax.Non_closed_number"),h.caml_fresh_oo_id(0)],at=d("-"),av=d(z),au=d(z),ar=d(z),S=[0,d(n),[0,d("Numbers"),[0,d("BinNums"),0]]],X=d("positive"),Z=d(R),$=[0,d(n),[0,d(y),[0,d(A),0]]],aa=d(Q),ac=d("R"),ad=[0,d(n),[0,d(y),[0,d(A),[0,d(Q),0]]]],ae=d("IZR"),ag=d("Rmult"),ai=d("Rdiv"),ak=[0,d(n),[0,d("ZArith"),[0,d("BinIntDef"),0]]],al=d(R),ao=d("pow_pos"),az=[0,d(n),[0,d(y),[0,d(A),0]]];function
o(c){var
d=b(k[22][14],f[1][6],c);return a(f[5][4],d)}function
g(g,d){var
c=a(e[1],g);return 0===c[0]?b(f[69][1],c[1],d):0}var
D=[0,o(S)],Y=a(f[8][4],X),s=b(f[25][3],D,Y),E=[3,[0,[0,s,0],1]],F=[3,[0,[0,s,0],2]],G=[3,[0,[0,s,0],3]];function
H(g,f){var
h=b(e[3],0,[0,E,0]),i=b(e[3],0,[0,G,0]),j=b(e[3],0,[0,F,0]);function
d(k){var
g=a(c[8],k),f=g[1];if(0===g[2]){var
l=[4,j,[0,d(f),0]];return b(e[3],0,l)}if(b(c[17],f,c[5]))return i;var
m=[4,h,[0,d(f),0]];return b(e[3],0,m)}return d(f)}function
l(m){var
d=a(e[1],m);switch(d[0]){case
0:if(b(f[69][1],d[1],G))return c[6];break;case
4:var
h=d[2];if(h)if(!h[2]){var
i=h[1],k=d[1];if(g(k,F)){var
n=l(i);return a(c[11],n)}if(g(k,E)){var
o=l(i),p=a(c[11],o);return a(c[9],p)}}break}throw j}var
_=a(f[8][4],Z),t=b(f[25][3],D,_),I=[3,[0,[0,t,0],1]],J=[3,[0,[0,t,0],2]],K=[3,[0,[0,t,0],3]];function
L(h,d){if(b(c[17],d,c[5]))return b(e[3],0,[0,I,0]);if(a(c[20],d))var
g=J,f=d;else
var
g=K,f=a(c[22],d);var
i=[0,H(h,f),0],j=[4,b(e[3],0,[0,g,0]),i];return b(e[3],0,j)}function
u(m){var
d=a(e[1],m);switch(d[0]){case
0:if(b(f[69][1],d[1],I))return c[5];break;case
4:var
h=d[2];if(h)if(!h[2]){var
i=h[1],k=d[1];if(g(k,J))return l(i);if(g(k,K)){var
n=l(i);return a(c[22],n)}}break}throw j}var
v=[0,o($)],ab=[2,v,a(f[8][4],aa)],T=a(f[1][6],ac),U=o(ad),W=b(V[15],U,T),af=a(f[8][4],ae),m=[1,b(f[19][3],v,af)],ah=a(f[8][4],ag),w=[1,b(f[19][3],ab,ah)],aj=a(f[8][4],ai),p=[1,b(f[19][3],v,aj)],am=a(f[8][4],al),an=[2,[0,o(ak)],am],ap=a(f[8][4],ao),M=[1,b(f[19][3],an,ap)];function
aq(s,o){var
g=o[2],d=g[3],t=g[2],B=o[1],C=g[1];function
i(a){var
c=[4,b(e[3],0,[0,m,0]),[0,a,0]];return b(e[3],0,c)}function
u(d){var
f=L(s,a(c[3],10)),g=[0,f,[0,H(0,d),0]],h=[4,b(e[3],0,[0,M,0]),g];return b(e[3],0,h)}var
I=b(N[17],C,t),v=a(c[1],I),J=0===B?v:a(c[22],v),j=i(L(s,J));if(h.caml_string_equal(d,ar))var
x=c[5];else{var
y=h.caml_string_get(d,1)-43|0;if(2<y>>>0)var
l=0;else{switch(y){case
0:var
Q=b(k[5],q(d),2),R=r(k[20][4],d,2,Q),A=a(c[1],R),n=1;break;case
1:var
l=0,n=0;break;default:var
S=b(k[5],q(d),2),T=r(k[20][4],d,2,S),U=a(c[1],T),A=a(c[22],U),n=1}if(n)var
z=A,l=1}if(!l)var
O=b(k[5],q(d),1),P=r(k[20][4],d,1,O),z=a(c[1],P);var
x=z}var
K=a(c[3],q(t)),f=b(c[13],x,K);if(a(c[18],f)){var
D=[0,j,[0,i(u(f)),0]],E=[4,b(e[3],0,[0,w,0]),D];return b(e[3],0,E)}if(a(c[19],f)){var
F=[0,j,[0,i(u(a(c[22],f))),0]],G=[4,b(e[3],0,[0,p,0]),F];return b(e[3],0,G)}return j}function
as(D){var
f=a(e[1],D);if(4===f[0]){var
h=f[2];if(h){var
i=h[2],y=h[1],d=f[1];if(i){if(!i[2]){var
E=i[1],T=g(d,w)?0:g(d,p)?0:1;if(!T){var
k=a(e[1],y),n=a(e[1],E);if(4===k[0]){var
o=k[2];if(o)if(!o[2])if(4===n[0]){var
q=n[2];if(q)if(!q[2]){var
F=q[1],G=n[1],H=o[1];if(g(k[1],m))if(g(G,m)){var
r=a(e[1],F);if(4===r[0]){var
s=r[2];if(s){var
t=s[2];if(t)if(!t[2]){var
I=t[1],J=s[1];if(g(r[1],M)){var
K=u(J),L=a(c[3],10);if(b(c[17],K,L)){var
v=u(H),O=l(I);if(a(c[20],v))var
A=0,z=v;else
var
A=1,z=a(c[22],v);var
P=a(c[2],z),Q=g(d,p)?at:av,R=a(c[2],O);return[0,A,[0,P,au,b(N[17],Q,R)]]}throw j}}}}throw j}}}}throw j}}}else
if(g(d,m)){var
x=u(y);if(a(c[19],x))var
C=1,B=a(c[22],x);else
var
C=0,B=x;var
S=a(c[2],B);return[0,C,a(aw[2],S)]}}}throw j}var
ay=[0,aq,function(a){var
b=a[1];try{var
c=[0,as(b)];return c}catch(a){a=aC(a);if(a===j)return 0;throw a}}];r(O[19],0,x,ay);var
aA=[0,0,x,[0,x],[0,W,az],[0,m,[0,w,[0,p,0]]],0],aB=O[23];function
ax(b){return a(aB,aA)}b(C[13],ax,B);P(31,[0],"R_syntax_plugin__R_syntax");return}
