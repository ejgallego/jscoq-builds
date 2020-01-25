function(aW){"use strict";var
G="(",D="after",l=")",C="Instead of Decimal.int, the types Decimal.uint or Z or Int63.int or Decimal.decimal could be used (you may need to require BinNums or Decimal or Int63 first).",h=aW.jsoo_runtime,c=h.caml_new_string,r=h.caml_register_global,aV=h.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):h.caml_call_gen(a,[b])}function
d(a,b,c){return a.length==2?a(b,c):h.caml_call_gen(a,[b,c])}function
L(a,b,c,d){return a.length==3?a(b,c,d):h.caml_call_gen(a,[b,c,d])}function
s(a,b,c,d,e){return a.length==4?a(b,c,d,e):h.caml_call_gen(a,[b,c,d,e])}var
e=h.caml_get_global_data(),al=c("num.int63.type"),ae=c("num.int.type"),af=c("num.uint.type"),ag=c("num.decimal.type"),ab=c("num.Z.type"),ac=c("num.pos.type"),v=c("numeral_notation_plugin"),k=e.Constrexpr_ops,E=e.Global,H=e.Smartlocate,x=e.Nametab,b=e.Pp,q=e.Libnames,am=e.CErrors,n=e.Coqlib,t=e.Names,F=e.Util,o=e.Vernacextend,B=e.Attributes,g=e.CLexer,y=e.Pcoq,i=e.Genarg,j=e.Stdarg,bD=e.CAst,bz=e.Evd,bF=e.Notation,U=e.Constrintern,T=e.Pretype_errors,S=e.Not_found,M=e.Termops,R=e.CWarnings,aG=e.Locality,V=e.Mltop;r(43,[0,0,0],"Numeral_notation_plugin");var
bC=[0,0],bI=[0,0,1],bJ=[0,0,0],bG=[0,0,1],bH=[0,0,0],bj=c(C),bm=c(" to Decimal.int or (option Decimal.int)."),bp=c(" should go from "),a2=c(C),a5=c(")."),a8=c(" or (option "),a$=c(" should go from Decimal.int to "),aT=c("core.option.type"),J=c("option type."),K=c(") targets an "),N=c("the parsing function ("),O=c("The 'abstract after' directive has no effect when "),P=c("numbers"),Q=c("abstract-large-number-no-op"),W=c(l),X=c("(warning after "),Y=c(l),Z=c("(abstract after "),ad=c(l),aj=c(D),an=c("warning"),ap=c(G),as=c(l),av=c(D),ax=c("abstract"),az=c(G),aC=c("numnotoption"),aJ=c(":"),aN=c("Notation"),aP=c("Numeral"),aU=c("NumeralNotation");function
I(c){var
e=a(b[22],J),f=a(b[22],K),g=a(E[2],0),h=a(M[78],g),i=d(x[44],h,c),j=a(b[22],N),k=a(b[22],O),l=d(b[12],k,j),m=d(b[12],l,i),n=d(b[12],m,f);return d(b[12],n,e)}var
aO=s(R[1],Q,P,0,I);function
m(b){var
c=a(n[2],b);return L(x[45],0,t[1][10][1],c)}function
p(c){var
b=a(x[12],c);if(2===b[0])return b[1];throw S}function
f(e,d,c,b){var
f=[0,a(k[9],c),[0,b]],g=a(k[10],f);try{s(U[10],e,d,0,g);var
h=1;return h}catch(a){a=aV(a);if(a[1]===T[1])return 0;throw a}}function
u(by,o,j,i,bx,G){var
c=a(E[2],0),e=a(bz[17],c);if(a(n[3],ae))if(a(n[3],af))if(a(n[3],ag))var
ah=m(ae),ai=m(af),aj=m(ag),aX=p(ah),ak=[0,p(ai),aX],aY=[0,ak,p(aj)],aZ=a(k[9],aj),a0=a(k[9],ai),r=[0,[0,ak,a(k[9],ah),a0,aY,aZ]],B=1;else
var
B=0;else
var
B=0;else
var
B=0;if(!B)var
r=0;if(a(n[3],ab))if(a(n[3],ac))var
ad=m(ab),aU=m(ac),aV=a(k[9],ad),aW=p(aU),s=[0,[0,[0,p(ad),aW],aV]],M=1;else
var
M=0;else
var
M=0;if(!M)var
s=0;if(a(n[3],al))var
a1=m(al),t=[0,a(k[9],a1)];else
var
t=0;var
I=a(H[4],o),bA=d(H[3],0,j),bB=d(H[3],0,i),g=a(k[9],o);function
h(c,b){var
e=[0,[0,d(bD[1],0,0),0],bC,c,b];return a(k[13],e)}function
l(d){var
b=m(aT),c=[0,a(k[9],b),[0,d,0]];return a(k[14],c)}var
aP=a(E[40],I)[2][4];function
aQ(a,b){return[3,[0,I,d(F[4],a,1)]]}var
aR=d(F[24][16],aQ,aP),aS=a(F[24][11],aR);if(r){var
u=r[1],an=u[5],ao=u[4],ap=u[3],aq=u[2],y=u[1];if(f(c,e,j,h(aq,g)))var
ar=[0,[0,y],1],O=1;else{if(f(c,e,j,h(aq,l(g))))var
aG=[0,[0,y],0],P=1;else{if(f(c,e,j,h(ap,g)))var
aH=[0,[1,y[1]],1],Q=1;else{if(f(c,e,j,h(ap,l(g))))var
K=[0,[1,y[1]],0],C=1;else
if(f(c,e,j,h(an,g)))var
K=[0,[3,ao],1],C=1;else
if(f(c,e,j,h(an,l(g))))var
K=[0,[3,ao],0],C=1;else
var
N=0,O=0,P=0,Q=0,C=0;if(C)var
aH=K,Q=1}if(Q)var
aG=aH,P=1}if(P)var
ar=aG,O=1}if(O)var
v=ar,N=1}else
var
N=0;if(!N){if(s){var
aI=s[1],aJ=aI[2],aK=aI[1];if(f(c,e,j,h(aJ,g)))var
aL=[0,[2,aK],1],S=1;else
if(f(c,e,j,h(aJ,l(g))))var
aL=[0,[2,aK],0],S=1;else
var
R=0,S=0;if(S)var
v=aL,R=1}else
var
R=0;if(!R){if(t){var
aM=t[1];if(f(c,e,j,h(aM,g)))var
aN=bI,U=1;else
if(f(c,e,j,h(aM,l(g))))var
aN=bJ,U=1;else
var
T=0,U=0;if(U)var
v=aN,T=1}else
var
T=0;if(!T)var
a3=a(b[3],a2),a4=a(b[5],0),a6=a(b[3],a5),a7=a(q[26],o),a9=a(b[3],a8),a_=a(q[26],o),ba=a(b[3],a$),bb=a(q[26],j),bc=d(b[12],bb,ba),bd=d(b[12],bc,a_),be=d(b[12],bd,a9),bf=d(b[12],be,a7),bg=d(b[12],bf,a6),bh=d(b[12],bg,a4),bi=d(b[12],bh,a3),v=L(am[5],0,0,bi)}}if(r){var
w=r[1],as=w[5],at=w[4],au=w[3],av=w[2],z=w[1];if(f(c,e,i,h(g,av)))var
aw=[0,[0,z],1],W=1;else{if(f(c,e,i,h(g,l(av))))var
ay=[0,[0,z],0],X=1;else{if(f(c,e,i,h(g,au)))var
az=[0,[1,z[1]],1],Y=1;else{if(f(c,e,i,h(g,l(au))))var
J=[0,[1,z[1]],0],D=1;else
if(f(c,e,i,h(g,as)))var
J=[0,[3,at],1],D=1;else
if(f(c,e,i,h(g,l(as))))var
J=[0,[3,at],0],D=1;else
var
V=0,W=0,X=0,Y=0,D=0;if(D)var
az=J,Y=1}if(Y)var
ay=az,X=1}if(X)var
aw=ay,W=1}if(W)var
A=aw,V=1}else
var
V=0;if(!V){if(s){var
aA=s[1],aB=aA[2],aC=aA[1];if(f(c,e,i,h(g,aB)))var
aD=[0,[2,aC],1],_=1;else
if(f(c,e,i,h(g,l(aB))))var
aD=[0,[2,aC],0],_=1;else
var
Z=0,_=0;if(_)var
A=aD,Z=1}else
var
Z=0;if(!Z){if(t){var
aE=t[1];if(f(c,e,i,h(g,aE)))var
aF=bG,aa=1;else
if(f(c,e,i,h(g,l(aE))))var
aF=bH,aa=1;else
var
$=0,aa=0;if(aa)var
A=aF,$=1}else
var
$=0;if(!$)var
bk=a(b[3],bj),bl=a(b[5],0),bn=a(b[3],bm),bo=a(q[26],o),bq=a(b[3],bp),br=a(q[26],i),bs=d(b[12],br,bq),bt=d(b[12],bs,bo),bu=d(b[12],bt,bn),bv=d(b[12],bu,bl),bw=d(b[12],bv,bk),A=L(am[5],0,0,bw)}}var
ax=[0,v,bA,A,bB,o,G],bK=typeof
G==="number"?0:1===G[0]?0===v[2]?(d(aO,0,ax[2]),1):0:0,bE=[0,by,bx,[1,ax],[0,a(x[38],[2,I]),0],aS,1];return a(bF[23],bE)}r(62,[0,u],"Numeral_notation_plugin__Numeral");a(V[9],v);function
w(c){if(typeof
c==="number")return a(b[7],0);else{if(0===c[0]){var
e=c[1],f=a(b[3],W),g=a(b[3],e),h=a(b[3],X),i=d(b[12],h,g);return d(b[12],i,f)}var
j=c[1],k=a(b[3],Y),l=a(b[3],j),m=a(b[3],Z),n=d(b[12],m,l);return d(b[12],n,k)}}var
_=0,$=[0,[0,0,function(a){return 0}],_];function
aa(f,a,e,d,c,b){return[0,a]}var
ah=[0,a(g[10],ad)],ai=[6,y[15][11]],ak=[0,a(g[10],aj)],ao=[0,a(g[10],an)],aq=[0,[0,[0,[0,[0,[0,[0,0,[0,a(g[10],ap)]],ao],ak],ai],ah],aa],$];function
ar(f,a,e,d,c,b){return[1,a]}var
at=[0,a(g[10],as)],au=[6,y[15][11]],aw=[0,a(g[10],av)],ay=[0,a(g[10],ax)],aA=[1,[0,[0,[0,[0,[0,[0,[0,0,[0,a(g[10],az)]],ay],aw],au],at],ar],aq]],aB=[0,function(b,a){return w},aA],z=d(o[3],aC,aB),A=z[1],aD=z[2],aE=0,aF=0;function
aH(h,g,f,e,c,b){var
i=d(B[1],B[7],b);return[0,function(d){var
b=a(t[1][8],e);return u(a(aG[7],i),h,g,f,b,c)}]}var
aI=[1,[5,a(i[16],A)],0],aK=[0,aJ,[1,[5,a(i[16],j[7])],aI]],aL=[1,[5,a(i[16],j[17])],aK],aM=[1,[5,a(i[16],j[17])],aL],aQ=[0,[0,0,[0,aP,[0,aN,[1,[5,a(i[16],j[17])],aM]]],aH,aF],aE],aR=0,aS=[0,function(a){return o[6]}];s(o[2],aU,aS,aR,aQ);r(71,[0,v,w,A,aD],"Numeral_notation_plugin__G_numeral");return}
