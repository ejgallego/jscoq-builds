(function(a$){"use strict";var
$="zn2z",H="BigN",aJ="Cyclic",n="Numbers",aI="t_",aH="int31",aG="digits",G="BigZ",F="BigQ",_="t",aF="Int31",m="Coq",h=a$.jsoo_runtime,a=h.caml_new_string,aE=h.caml_register_global,D=h.caml_wrap_exception;function
d(a,b){return a.length==1?a(b):h.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):h.caml_call_gen(a,[b,c])}function
E(a,b,c,d,e){return a.length==4?a(b,c,d,e):h.caml_call_gen(a,[b,c,d,e])}var
e=h.caml_get_global_data(),ad=a("numbers_syntax_plugin"),r=[0,a(m),[0,a(n),[0,a(aJ),[0,a(aF),[0,a(aF),0]]]]],af=a("int31_scope"),M=[0,a(m),[0,a(n),[0,a(aJ),[0,a("DoubleCyclic"),[0,a("DoubleType"),0]]]]],v=[0,a(m),[0,a(n),[0,a("Natural"),[0,a(H),[0,a(H),0]]]]],ai=a("bigN_scope"),w=[0,a(m),[0,a(n),[0,a("Integer"),[0,a(G),[0,a(G),0]]]]],al=a("bigZ_scope"),z=[0,a(m),[0,a(n),[0,a("Rational"),[0,a(F),[0,a(F),0]]]]],ao=a("bigQ_scope"),g=e.Globnames,c=e.Bigint,l=e.Loc,k=e.Pervasives,aa=e.Pp,ac=e.CErrors,ab=e.List,i=e.Names,o=e.Notation,aK=e.Nat_syntax_plugin,aL=e.Libnames;d(e.Mltop[12],ad);var
a9=a("bigN are only non-negative numbers."),a_=a("interp_bigN"),a7=a("int31 are only non-negative numbers."),a8=a("interp_int31"),aM=a(aH),aN=a(aH),aO=a(aG),aP=a(aG),aQ=a($),aS=a($),aT=a($),aU=a(_),aV=[0,a(H),0],aW=a("t'"),aX=a(H),aY=a(_),aZ=[0,a(G),0],a0=a(aI),a1=a(G),a2=a(_),a3=[0,a(F),0],a4=a(aI),a5=a(F),a6=a("Numbers_syntax.Non_closed");function
p(a){var
c=b(ab[15],i[1][5],a);return d(i[5][4],c)}function
j(c,a){var
e=d(i[1][5],a),f=p(c);return b(aL[17],f,e)}function
I(c,a){var
e=d(i[6][4],a);return b(i[23][3],c,e)}function
J(b,a){return I([0,p(b)],a)}function
q(c,b,a){var
e=d(i[6][4],b);return I([2,[0,p(c)],e],a)}var
ae=j(r,aM);function
s(a){return J(r,a)}var
t=[3,[0,[0,s(aN),0],1]],K=[3,[0,[0,s(aO),0],1]],L=[3,[0,[0,s(aP),0],2]],aR=j(M,aQ);function
N(a){return J(M,a)}var
O=[3,[0,[0,N(aS),0],1]],u=[3,[0,[0,N(aT),0],2]],ag=j(b(k[22],v,aV),aU),ah=q(v,aX,aW),aj=7;function
P(a){return[3,[0,[0,ah,0],b(k[4],a,aj)+1|0]]}var
ak=j(b(k[22],w,aZ),aY),Q=q(w,a1,a0),x=[3,[0,[0,Q,0],1]],y=[3,[0,[0,Q,0],2]],am=j(b(k[22],z,a3),a2),an=q(z,a5,a4),A=[3,[0,[0,an,0],1]],f=[248,a6,h.caml_fresh_oo_id(0)];function
R(a,e){var
f=[0,[0,a,t,0]],g=[0,[0,a,K,0]],h=[0,[0,a,L,0]];function
b(a,f){if(0<a){var
e=d(c[8],f),i=e[2],j=b(a-1|0,e[1]),k=i?h:g;return[0,k,j]}return 0}var
i=b(31,e);return[4,a,f,d(ab[6],i)]}function
ap(a){var
b=[0,a,a8,d(aa[1],a7)];return d(ac[8],b)}function
aq(b,a){return d(c[20],a)?R(b,a):ap(b)}function
S(h){if(4===h[0]){var
l=h[2];if(0===l[0]){var
n=h[3];if(b(g[5],l[1][2],t)){var
e=n,a=c[5];for(;;){if(e){var
i=e[1];if(0===i[0]){var
j=e[2],k=i[1][2];if(b(g[5],k,K)){var
e=j,a=d(c[11],a);continue}if(b(g[5],k,L)){var
m=d(c[11],a),e=j,a=d(c[9],m);continue}}throw f}return a}}}}throw f}function
ar(a){try{var
b=[0,S(a)];return b}catch(a){a=D(a);if(a===f)return 0;throw a}}E(o[13],af,[0,ae,r],aq,[0,[0,[0,[0,l[4],t,0]],0],ar,1]);var
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
h=[0,[0,a,O,0]],i=[0,[0,a,u,0]];function
e(d,f){if(0<d){if(b(c[17],f,c[5]))return[4,a,h,[0,[13,[0,a,0,0,0]],0]];var
g=as(d,f),j=g[1],k=[0,e(d-1|0,g[2]),0];return[4,a,i,[0,[13,[0,a,0,0,0]],[0,e(d-1|0,j),k]]]}return R(a,f)}return e(f,d)}function
B(a,f){var
e=at(f),i=[0,[0,a,P(e),0]],g=au(a,e,f);if(e<7)var
h=[0,g,0];else
var
j=d(c[3],e-7|0),h=[0,b(aK[1][4],a,j),[0,g,0]];return[4,a,i,h]}function
av(a){var
b=[0,a,a_,d(aa[1],a9)];return d(ac[8],b)}function
aw(b,a){return d(c[20],a)?B(b,a):av(b)}function
V(a){if(4===a[0]){var
e=a[2];if(0===e[0]){var
f=a[3];if(f){var
c=f[2];if(c){var
d=c[2];if(d)if(!d[2]){var
h=d[1],i=c[1];if(b(g[5],e[1][2],u)){var
j=V(h),l=V(i);return 1+b(k[5],l,j)|0}}}}}}return 0}function
W(k,a){if(4===a[0]){var
h=a[2];if(0===h[0]){var
i=a[3],j=h[1][2];if(b(g[5],j,O))return c[5];if(i){var
d=i[2];if(d){var
e=d[2];if(e)if(!e[2]){var
l=e[1],m=d[1];if(b(g[5],j,u)){var
f=k-1|0,n=W(f,l),o=W(f,m),p=U(f),q=b(c[14],p,o);return b(c[12],q,n)}}}}}}return S(a)}function
X(a){return W(V(a),a)}function
C(c){if(4===c[0]){var
a=c[3];if(a){var
b=a[2],d=a[1];if(!b)return X(d);if(!b[2])return X(b[1])}}throw f}function
ax(a){try{var
b=[0,C(a)];return b}catch(a){a=D(a);if(a===f)return 0;throw a}}function
ay(a){if(a<8){var
b=ay(a+1|0),c=P(a);return[0,[0,[0,l[4],c,0]],b]}return 0}var
az=ay(0);E(o[13],ai,[0,ag,v],aw,[0,az,ax,1]);function
Y(a,b){var
e=[0,[0,a,x,0]],f=[0,[0,a,y,0]];return d(c[20],b)?[4,a,e,[0,B(a,b),0]]:[4,a,f,[0,B(a,d(c[22],b)),0]]}function
Z(a){if(4===a[0]){var
h=a[2];if(0===h[0]){var
e=a[3];if(e)if(!e[2]){var
i=e[1],j=h[1][2];if(b(g[5],j,x))return C(i);if(b(g[5],j,y)){var
k=C(i);if(b(c[17],k,c[5]))throw f;return d(c[22],k)}}}}throw f}function
aA(a){try{var
b=[0,Z(a)];return b}catch(a){a=D(a);if(a===f)return 0;throw a}}E(o[13],al,[0,ak,w],Y,[0,[0,[0,[0,l[4],x,0]],[0,[0,[0,l[4],y,0]],0]],aA,1]);function
aB(a,b){return[4,a,[0,[0,a,A,0]],[0,Y(a,b),0]]}function
aC(c){try{if(4===c[0]){var
h=c[2];if(0===h[0]){var
d=c[3];if(d)if(d[2])var
a=0;else{var
i=d[1];if(b(g[5],h[1][2],A))var
e=[0,Z(i)],a=1;else
var
a=0}else
var
a=0}else
var
a=0}else
var
a=0;if(!a)var
e=0;return e}catch(a){a=D(a);if(a===f)return 0;throw a}}E(o[13],ao,[0,am,z],aB,[0,[0,[0,[0,l[4],A,0]],0],aC,1]);var
aD=[0,ad,p,j,I,J,q,r,ae,s,af,t,K,L,M,aR,N,O,u,v,ag,ah,ai,aj,P,w,ak,Q,al,x,y,z,am,an,ao,A,f,R,ap,aq,S,ar,T,U,as,at,au,B,av,aw,X,C,ax,az,Y,Z,aA,aB,aC];aE(46,aD,"Numbers_syntax_plugin.Numbers_syntax");aE(47,[0,aD],"Numbers_syntax_plugin");return});
