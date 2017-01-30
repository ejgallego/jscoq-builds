(function(a$){"use strict";var
$="zn2z",H="BigN",aJ="Cyclic",n="Numbers",aI="t_",aH="int31",aG="digits",G="BigZ",F="BigQ",_="t",aF="Int31",m="Coq",h=a$.jsoo_runtime,a=h.caml_new_string,aE=h.caml_register_global,D=h.caml_wrap_exception;function
d(a,b){return a.length==1?a(b):h.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):h.caml_call_gen(a,[b,c])}function
E(a,b,c,d,e){return a.length==4?a(b,c,d,e):h.caml_call_gen(a,[b,c,d,e])}var
e=h.caml_get_global_data(),aa=a("numbers_syntax_plugin"),q=[0,a(m),[0,a(n),[0,a(aJ),[0,a(aF),[0,a(aF),0]]]]],ad=a("int31_scope"),M=[0,a(m),[0,a(n),[0,a(aJ),[0,a("DoubleCyclic"),[0,a("DoubleType"),0]]]]],u=[0,a(m),[0,a(n),[0,a("Natural"),[0,a(H),[0,a(H),0]]]]],ag=a("bigN_scope"),v=[0,a(m),[0,a(n),[0,a("Integer"),[0,a(G),[0,a(G),0]]]]],aj=a("bigZ_scope"),y=[0,a(m),[0,a(n),[0,a("Rational"),[0,a(F),[0,a(F),0]]]]],am=a("bigQ_scope"),g=e.Globnames,c=e.Bigint,l=e.Loc,k=e.Pervasives,ao=e.Pp,ap=e.CErrors,ab=e.List,i=e.Names,A=e.Notation,a8=e.Nat_syntax_plugin,aK=e.Libnames;d(e.Mltop[12],aa);var
a9=a("bigN are only non-negative numbers."),a_=a("interp_bigN"),a6=a("int31 are only non-negative numbers."),a7=a("interp_int31"),aL=a(aH),aM=a(aH),aN=a(aG),aO=a(aG),aP=a($),aR=a($),aS=a($),aT=a(_),aU=[0,a(H),0],aV=a("t'"),aW=a(H),aX=a(_),aY=[0,a(G),0],aZ=a(aI),a0=a(G),a1=a(_),a2=[0,a(F),0],a3=a(aI),a4=a(F),a5=a("Numbers_syntax.Non_closed");function
o(a){var
c=b(ab[15],i[1][5],a);return d(i[5][4],c)}function
j(c,a){var
e=d(i[1][5],a),f=o(c);return b(aK[17],f,e)}function
I(c,a){var
e=d(i[6][4],a);return b(i[23][3],c,e)}function
J(b,a){return I([0,o(b)],a)}function
p(c,b,a){var
e=d(i[6][4],b);return I([2,[0,o(c)],e],a)}var
ac=j(q,aL);function
r(a){return J(q,a)}var
s=[3,[0,[0,r(aM),0],1]],K=[3,[0,[0,r(aN),0],1]],L=[3,[0,[0,r(aO),0],2]],aQ=j(M,aP);function
N(a){return J(M,a)}var
O=[3,[0,[0,N(aR),0],1]],t=[3,[0,[0,N(aS),0],2]],ae=j(b(k[22],u,aU),aT),af=p(u,aW,aV),ah=7;function
P(a){return[3,[0,[0,af,0],b(k[4],a,ah)+1|0]]}var
ai=j(b(k[22],v,aY),aX),Q=p(v,a0,aZ),w=[3,[0,[0,Q,0],1]],x=[3,[0,[0,Q,0],2]],ak=j(b(k[22],y,a2),a1),al=p(y,a4,a3),z=[3,[0,[0,al,0],1]],f=[248,a5,h.caml_fresh_oo_id(0)];function
R(a,e){var
f=[0,[0,a,s,0]],g=[0,[0,a,K,0]],h=[0,[0,a,L,0]];function
b(a,f){if(0<a){var
e=d(c[8],f),i=e[2],j=b(a-1|0,e[1]),k=i?h:g;return[0,k,j]}return 0}var
i=b(31,e);return[4,a,f,d(ab[6],i)]}function
an(a){var
b=[0,a,a7,d(ao[1],a6)];return d(ap[8],b)}function
aq(b,a){return d(c[20],a)?R(b,a):an(b)}function
S(h){if(4===h[0]){var
l=h[2];if(0===l[0]){var
n=h[3];if(b(g[5],l[1][2],s)){var
e=n,a=c[5];for(;;){if(e){var
i=e[1];if(0===i[0]){var
j=e[2],k=i[1][2];if(b(g[5],k,K)){var
e=j,a=d(c[11],a);continue}if(b(g[5],k,L)){var
m=d(c[11],a),e=j,a=d(c[9],m);continue}}throw f}return a}}}}throw f}function
ar(a){try{var
b=[0,S(a)];return b}catch(a){a=D(a);if(a===f)return 0;throw a}}E(A[13],ad,[0,ac,q],aq,[0,[0,[0,[0,l[4],s,0]],0],ar,1]);var
T=b(c[23],c[7],31);function
U(e){var
d=e,a=T;for(;;){if(0<d){var
d=d-1|0,a=b(c[14],a,a);continue}return a}}function
as(d,a){var
e=U(d-1|0);return b(c[15],a,e)}function
at(e){var
d=0,a=T;for(;;){if(b(c[16],e,a))return d;var
d=d+1|0,a=b(c[14],a,a);continue}}function
au(a,f,d){var
h=[0,[0,a,O,0]],i=[0,[0,a,t,0]];function
e(d,f){if(0<d){if(b(c[17],f,c[5]))return[4,a,h,[0,[13,[0,a,0,0,0]],0]];var
g=as(d,f),j=g[1],k=[0,e(d-1|0,g[2]),0];return[4,a,i,[0,[13,[0,a,0,0,0]],[0,e(d-1|0,j),k]]]}return R(a,f)}return e(f,d)}function
B(a,f){var
e=at(f),i=[0,[0,a,P(e),0]],g=au(a,e,f);if(e<7)var
h=[0,g,0];else
var
j=d(c[3],e-7|0),h=[0,b(a8[1][4],a,j),[0,g,0]];return[4,a,i,h]}function
av(a){var
b=[0,a,a_,d(ao[1],a9)];return d(ap[8],b)}function
aw(b,a){return d(c[20],a)?B(b,a):av(b)}function
V(a){if(4===a[0]){var
e=a[2];if(0===e[0]){var
f=a[3];if(f){var
c=f[2];if(c){var
d=c[2];if(d)if(!d[2]){var
h=d[1],i=c[1];if(b(g[5],e[1][2],t)){var
j=V(h),l=V(i);return 1+b(k[5],l,j)|0}}}}}}return 0}function
W(k,a){if(4===a[0]){var
h=a[2];if(0===h[0]){var
i=a[3],j=h[1][2];if(b(g[5],j,O))return c[5];if(i){var
d=i[2];if(d){var
e=d[2];if(e)if(!e[2]){var
l=e[1],m=d[1];if(b(g[5],j,t)){var
f=k-1|0,n=W(f,l),o=W(f,m),p=U(f),q=b(c[14],p,o);return b(c[12],q,n)}}}}}}return S(a)}function
X(a){return W(V(a),a)}function
C(c){if(4===c[0]){var
a=c[3];if(a){var
b=a[2],d=a[1];if(!b)return X(d);if(!b[2])return X(b[1])}}throw f}function
ax(a){try{var
b=[0,C(a)];return b}catch(a){a=D(a);if(a===f)return 0;throw a}}function
ay(a){if(a<8){var
b=ay(a+1|0),c=P(a);return[0,[0,[0,l[4],c,0]],b]}return 0}var
az=ay(0);E(A[13],ag,[0,ae,u],aw,[0,az,ax,1]);function
Y(a,b){var
e=[0,[0,a,w,0]],f=[0,[0,a,x,0]];return d(c[20],b)?[4,a,e,[0,B(a,b),0]]:[4,a,f,[0,B(a,d(c[22],b)),0]]}function
Z(a){if(4===a[0]){var
h=a[2];if(0===h[0]){var
e=a[3];if(e)if(!e[2]){var
i=e[1],j=h[1][2];if(b(g[5],j,w))return C(i);if(b(g[5],j,x)){var
k=C(i);if(b(c[17],k,c[5]))throw f;return d(c[22],k)}}}}throw f}function
aA(a){try{var
b=[0,Z(a)];return b}catch(a){a=D(a);if(a===f)return 0;throw a}}E(A[13],aj,[0,ai,v],Y,[0,[0,[0,[0,l[4],w,0]],[0,[0,[0,l[4],x,0]],0]],aA,1]);function
aB(a,b){return[4,a,[0,[0,a,z,0]],[0,Y(a,b),0]]}function
aC(c){try{if(4===c[0]){var
h=c[2];if(0===h[0]){var
d=c[3];if(d)if(d[2])var
a=0;else{var
i=d[1];if(b(g[5],h[1][2],z))var
e=[0,Z(i)],a=1;else
var
a=0}else
var
a=0}else
var
a=0}else
var
a=0;if(!a)var
e=0;return e}catch(a){a=D(a);if(a===f)return 0;throw a}}E(A[13],am,[0,ak,y],aB,[0,[0,[0,[0,l[4],z,0]],0],aC,1]);var
aD=[0,aa,o,j,I,J,p,q,ac,r,ad,s,K,L,M,aQ,N,O,t,u,ae,af,ag,ah,P,v,ai,Q,aj,w,x,y,ak,al,am,z,f,R,an,aq,S,ar,T,U,as,at,au,B,av,aw,X,C,ax,az,Y,Z,aA,aB,aC];aE(46,aD,"Numbers_syntax_plugin.Numbers_syntax");aE(47,[0,aD],"Numbers_syntax_plugin");return});
