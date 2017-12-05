(function(eM){"use strict";var
bD=104,ah=123,aS="Firstorder",aR="$l",bs="already done",aQ="firstorder_using",bC=",",bt=250,z=105,au="$t",aM="gintuition",bu=110,aP=246,O="Extension: cannot occur",bB=120,ag=144,bA="with",br="Depth",af="firstorder",bz="ground_plugin",aO="Firstorder_Print_Solver",ae=100,by="Solver",P="plugins/firstorder/g_ground.ml4",bx=248,aL="Firstorder_Set_Solver",aN="using",bw="-----",bq=114,S=147,bv="reversible in 1st order mode",u=eM.jsoo_runtime,I=u.caml_check_bound,bn=u.caml_fresh_oo_id,c=u.caml_new_string,bo=u.caml_obj_tag,N=u.caml_register_global,v=u.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):u.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):u.caml_call_gen(a,[b,c])}function
g(a,b,c,d){return a.length==3?a(b,c,d):u.caml_call_gen(a,[b,c,d])}function
r(a,b,c,d,e){return a.length==4?a(b,c,d,e):u.caml_call_gen(a,[b,c,d,e])}function
J(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):u.caml_call_gen(a,[b,c,d,e,f])}function
R(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):u.caml_call_gen(a,[b,c,d,e,f,g])}function
bp(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):u.caml_call_gen(a,[b,c,d,e,f,g,h])}var
f=u.caml_get_global_data(),M=c(bz),a_=[0,c(bz),c("auto_with")],T=f.Term,j=f.Util,Y=f.Context,d=f.EConstr,B=f.Termops,al=f.Hipattern,ax=f.Global,ai=f.Inductiveops,X=f.Reductionops,av=f.CClosure,w=f.Names,Z=f.Int,F=f.Not_found,A=f.Queue,G=f.Assert_failure,aE=f.Evd,n=f.Pp,a0=f.Ppconstr,aq=f.Printer,aF=f.Hints,x=f.Pervasives,aa=f.CErrors,ap=f.Typing,an=f.Globnames,_=f.Option,aC=f.Heap,a2=f.CamlinternalLazy,h=f.Tactics,p=f.Tacmach,e=f.Tacticals,i=f.Proofview,k=f.Ltac_plugin,a8=f.Feedback,a7=f.List,a6=f.Classops,C=f.Loc,l=f.Genarg,E=f.Stdarg,bf=f.Egramml,at=f.Vernac_classifier,be=f.Vernacinterp,bd=f.Locality,aH=f.Mltop,a9=f.Goptions,V=f.CList,D=f.Pcoq,bk=f.Genintern,aJ=f.Geninterp,ad=f.CLexer,bl=f.Array,aT=[0,1],aw=[0,av[14]],bG=c("Formula.Is_atom"),bI=[0,0,[0,0,0]],bJ=c("_"),bL=c("Unify.UFAIL"),b6=c(" : "),b7=c("| "),b8=c(bw),b9=c(bw),b2=c(" : No such Hint database"),b3=c("Firstorder: "),cw=c("iff"),cx=c("not"),cs=[0,c("Init"),[0,c("Logic"),0]],ct=c("User"),cp=c(bv),cf=c("No link"),cd=c("No axiom link"),cb=[0,c("plugins/firstorder/rules.ml"),59,7],b$=c("Not the expected number of hyps."),cJ=c("not implemented ... yet"),cH=c("Untypable instance, maybe higher-order non-prenex quantification"),cG=c(bs),cI=c(bs),cC=c("can't happen."),cD=c("x"),cK=[0,0],cL=c(bv),eJ=[0,c(P),1,0],eI=c(au),eK=[0,c(aM)],eL=c(aM),eD=c(O),ez=[0,c(P),1,0],ex=[0,c(P),1,0],eu=[0,c(P),1,0],er=[0,c(P),1,0],eo=[0,c(P),1,0],em=[0,c(P),1,0],ej=[0,c(P),1,0],ei=c("$l'"),ek=[0,c(bA)],el=c(aR),en=c(au),ep=[0,c(af)],eq=c(aR),es=[0,c(bA)],et=c(au),ev=[0,c(af)],ew=c(aR),ey=c(au),eA=[0,c(af)],eB=c(af),ed=c(O),eb=c(O),d$=c(O),dy=c('Deprecated syntax; use "," as separator'),ds=c(aO),dp=c(aO),dl=c(O),dj=c(aO),dg=c("Firstorder solver tactic is "),df=c(O),dd=c(aL),c7=c(aL),c4=c(O),c2=c(aL),cZ=c(O),cN=[0,c(aS),[0,c(br),0]],cO=c("Firstorder Depth"),cR=[0,c("Congruence"),[0,c(br),0]],cS=c("Congruence Depth"),cX=c("Firstorder default solver"),c_=[0,c(by)],c$=[0,c(aS)],da=[0,c("Set")],dq=[0,[0,[0,c("Print")],[0,[0,c(aS)],[0,[0,c(by)],0]]],0],dt=c("GTauto failed"),dz=c("deprecated"),dA=c("firstorder-deprecated-syntax"),dC=c(aQ),dK=c(aQ),dP=c(aN),dS=c(bC),dV=c(bC),dY=c(aN),d4=c(aN),d9=c(aQ),eg=c(af),eG=c(aM),b5=f.Constrextern,cu=f.Coqlib,cv=f.Universes,b_=f.Control,cF=f.Evarutil,dw=f.Libnames,cU=f.Auto,dB=f.CWarnings;function
bE(h,g,f,e,d,c){var
a=b(h,f,e);return 0===a?b(g,d,c):a}function
bF(j,i,h,g,f,e,d,c){var
a=r(j,h,g,f,e);return 0===a?b(i,d,c):a}var
W=[bx,bG,bn(0)];function
aU(g,f){var
b=g,c=f;for(;;){var
d=a(T[135],c);if(6===d[0]){var
e=d[3];if(0<b){var
b=b-1|0,c=e;continue}return 1+aU(0,e)|0}return 0}}function
bH(d,c){var
e=a(ax[26],c[1])[1][6],f=b(ai[4],d,c);function
g(a){return aU(e,a)}return b(j[19][15],g,f)}function
aj(i,c,h,f,e){var
k=b(ai[4],i,f);function
l(f){var
i=a(d[8],f),j=g(B[60],c,i,e),k=g(d[90],c,h,j)[2];return b(d[89],c,k)[1]}return b(j[19][15],l,k)}function
ay(c,b,a){return r(X[17],aw[1],c,b,a)}function
ak(m,c,C){var
e=r(X[18],aw[1],m,c,C),n=b(al[23],c,e);if(n){var
o=n[1],D=o[1];return[0,D,b(d[z][1],-1,o[2])]}var
p=b(al[21],c,e);if(p){var
q=p[1];return[5,q[2],q[3]]}var
s=b(al[27],c,e);if(s){var
g=s[1],h=g[2],E=g[3],t=b(d[75],c,g[1]),f=t[1],i=b(d[2][2],c,t[2]),u=a(ax[26],f),k=u[2],v=u[1],w=k[4].length-1;if(0===w)return[1,[0,f,i],h];var
F=0<E?1:0,G=function(e){var
f=v[6],g=a(d[8],e);return b(B[67],c,g)===f?1:0},l=b(j[19][29],G,k[9]);if(!a(ai[19],[0,f,v,k])){var
J=F?l?1:0:1;if(J)return 1===w?[2,[0,f,i],h,l]:[3,[0,f,i],h,l]}return[6,e]}var
x=b(al[29],c,e);if(x){var
y=x[1],H=y[2],A=b(d[75],c,y[1]),I=A[1];return[4,[0,I,b(d[2][2],c,A[2])],H]}return[6,ay(m,c,e)]}var
bK=[0,a(w[1][6],bJ)];function
aV(m,h,p,f,c){var
n=[0,0],k=[0,0],l=[0,0];function
i(B,e,A){var
f=B,o=A;for(;;){var
c=ak(m,h,o);switch(c[0]){case
0:var
C=c[2];i(f,1-e,c[1]);var
o=C;continue;case
1:var
s=1-e,D=s?(n[1]=1,0):s;return D;case
4:var
v=c[2],K=c[1],L=a(p,1),M=a(d[11],L),N=I(aj(m,h,1,K,v),0)[1],O=function(g,j,c){var
h=a(Y[1][1][3],c);return i([0,M,f],e,b(d[z][1],g,h))},P=2-a(j[17][1],v)|0;return r(j[17][90],O,P,0,N);case
5:var
Q=c[2],R=a(p,1),f=[0,a(d[11],R),f],o=Q;continue;case
6:var
q=g(d[z][3],f,0,c[1]),w=1-b(d[48],h,q);if(w){if(e){k[1]=[0,q,k[1]];return 0}l[1]=[0,q,l[1]];var
x=0}else
var
x=w;return x;default:var
E=c[2],F=c[1];if(c[3]){var
y=ay(m,h,g(d[z][3],f,0,o));if(e)k[1]=[0,y,k[1]];else
l[1]=[0,y,l[1]]}var
t=aj(m,h,0,F,E),G=function(g,j,c){var
h=a(Y[1][1][3],c);return i(f,e,b(d[z][1],g,h))},H=function(b){var
c=1-a(j[17][1],b)|0;return r(j[17][90],G,c,0,b)};if(e)var
J=function(a){return a?0:1},u=b(j[19][29],J,t);else
var
u=e;if(u)n[1]=1;return b(j[19][13],H,t)}}}switch(f){case
0:i(0,0,c);break;case
1:i(0,1,c);break;default:var
e=b(d[88],h,c),o=e[2],q=e[1],s=function(c){var
b=a(p,1);return a(d[11],b)};i(b(j[17][17],s,q),0,o);n[1]=0}return[0,n[1],[0,k[1],l[1]]]}var
o=[0,aT,aw,bE,bF,bH,aj,bK,aV,function(g,f,o,t,e,n){function
k(a){return ay(g,f,a)}try{var
p=a(n,0)+1|0,q=aT[1]?aV(g,f,n,o,e):bI,r=q[1],u=q[2];if(1===o){var
l=ak(g,f,e);switch(l[0]){case
0:var
h=0;break;case
1:var
h=3;break;case
2:var
h=1;break;case
3:var
h=2;break;case
4:var
x=I(aj(g,f,0,l[1],l[2]),0)[1],y=a(j[17][112],x),h=[0,p,a(Y[1][1][3],y),r];break;case
5:var
h=4;break;default:throw[0,W,l[1]]}var
s=[1,h]}else{var
c=ak(g,f,e);switch(c[0]){case
0:var
m=c[1],z=c[2],A=k(m),b=ak(g,f,m);switch(b[0]){case
0:var
d=[5,b[1],b[2],z];break;case
1:var
d=[0,b[1],b[2]];break;case
2:var
d=[1,b[1],b[2]];break;case
3:var
d=[2,b[1],b[2]];break;case
4:var
d=[4,b[1],b[2]];break;case
5:var
d=[3,m];break;default:var
d=0}var
i=[4,A,d];break;case
1:var
i=0;break;case
2:var
B=c[1];if(c[3])throw[0,W,k(e)];var
i=[0,B];break;case
3:var
C=c[1];if(c[3])throw[0,W,k(e)];var
i=[1,C];break;case
4:var
i=[3,c[1]];break;case
5:var
i=[2,p,c[1],r];break;default:throw[0,W,c[1]]}var
s=[0,i]}var
w=[0,[0,t,k(e),s,u]];return w}catch(a){a=v(a);if(a[1]===W)return[1,a[2]];throw a}}];N(z,o,"Ground_plugin.Formula");var
K=[bx,bL,bn(0)];function
aW(a){return b(d[z][1],-1,a)}function
az(e,c){function
f(b){var
c=b[1];return[0,c,a(d[ah][1],b[2])]}var
g=b(j[17][15],f,e),h=a(d[ah][1],c),i=b(B[47],g,h);return a(d[8],i)}function
aA(f,V,U){var
i=a(A[2],0),p=[0,0];function
s(c,a){var
d=p[1];function
e(b){var
d=b[1];return[0,d,az([0,[0,c,a],0],b[2])]}p[1]=[0,[0,c,a],b(j[17][15],e,d)];return 0}function
t(c){var
a=b(d[3],f,c);if(2===a[0]){var
e=a[1];try{var
g=t(b(Z[4][2],e,p[1]));return g}catch(a){a=v(a);if(a===F)return c;throw a}}return c}b(A[3],[0,V,U],i);try{for(;;){var
z=a(A[5],i),W=z[2],k=t(b(X[28],f,z[1])),l=t(b(X[28],f,W)),h=b(d[3],f,k),c=b(d[3],f,l);switch(h[0]){case
2:var
q=h[1];if(2===c[0]){var
x=c[1];if(1-(q===x?1:0))if(q<x)s(x,k);else
s(q,l)}else{var
w=az(p[1],l),ab=b(B[36],f,w);if(a(Z[2][2],ab))var
ac=a(d[11],q),S=g(B[37],f,ac,w)?0:(s(q,w),1);else
var
S=0;if(!S)throw[0,K,k,l]}var
e=3;break;case
6:var
ad=h[3],ae=h[2];switch(c[0]){case
2:var
e=0;break;case
5:var
e=1;break;case
6:var
H=c[3],G=c[2],E=ad,D=ae,e=4;break;default:var
e=2}break;case
7:var
ah=h[3],ai=h[2];switch(c[0]){case
2:var
e=0;break;case
5:var
e=1;break;case
7:var
H=c[3],G=c[2],E=ah,D=ai,e=4;break;default:var
e=2}break;case
9:var
J=h[2],aj=h[1];switch(c[0]){case
2:var
e=0;break;case
5:var
e=1;break;case
9:var
L=c[2];b(A[3],[0,aj,c[1]],i);var
M=J.length-1;if(M!==L.length-1)throw[0,K,k,l];var
N=M-1|0,ak=0;if(!(N<0)){var
m=ak;for(;;){var
al=I(L,m)[m+1],am=[0,I(J,m)[m+1],al];b(A[3],am,i);var
an=m+1|0;if(N!==m){var
m=an;continue}break}}var
e=3;break;default:var
e=2}break;case
13:var
O=h[4],ao=h[3],ap=h[2];switch(c[0]){case
2:var
e=0;break;case
5:var
e=1;break;case
13:var
P=c[4],aq=c[3];b(A[3],[0,ap,c[2]],i);b(A[3],[0,ao,aq],i);var
Q=O.length-1;if(Q!==P.length-1)throw[0,K,k,l];var
R=Q-1|0,ar=0;if(!(R<0)){var
n=ar;for(;;){var
as=I(P,n)[n+1],at=[0,I(O,n)[n+1],as];b(A[3],at,i);var
au=n+1|0;if(R!==n){var
n=au;continue}break}}var
e=3;break;default:var
e=2}break;default:var
e=0}switch(e){case
0:if(2===c[0]){var
C=c[1],u=az(p[1],k),$=b(B[36],f,u);if(a(Z[2][2],$)){var
aa=a(d[11],C);if(g(B[37],f,aa,u))var
y=1;else{s(C,u);var
o=2,y=0}}else
var
y=1;if(y)throw[0,K,k,l]}else
if(5===h[0]){var
_=[0,b(B[61],f,k),l];b(A[3],_,i);var
o=2}else
var
o=0;break;case
1:var
o=0;break;case
2:var
o=1;break;case
3:var
o=2;break;default:b(A[3],[0,D,G],i);var
af=aW(H),ag=[0,aW(E),af];b(A[3],ag,i);var
o=3}switch(o){case
0:if(5===c[0]){var
Y=[0,k,b(B[61],f,l)];b(A[3],Y,i);var
r=1}else
var
r=0;break;case
1:var
r=0;break;case
2:var
r=1;break;default:var
r=2}switch(r){case
0:if(1-g(d[94],f,k,l))throw[0,K,k,l];var
T=0;break;case
1:var
T=0;break;default:var
T=1}continue}}catch(a){a=v(a);if(a===A[1])return p[1];throw a}}function
bM(a,g,c){function
e(c){if(b(d[48],a,c))if(b(d[65],a,c)===g)return 0;function
h(a,c){var
b=e(c);return 0<=a?0<=b?a+b|0:a:b}var
f=r(d[bD],a,h,-1,c);return 0<=f?f+1|0:-1}return e(c)}function
bN(g,f){var
c=[0,1],e=[0,0];function
h(f,i){var
j=b(d[3],g,i);if(2===j[0]){var
k=j[1];try{var
n=f+b(Z[4][2],k,e[1])|0,o=a(d[9],n);return o}catch(b){b=v(b);if(b===F){var
l=c[1];c[1]++;e[1]=[0,[0,k,l],e[1]];return a(d[9],l+f|0)}throw b}}function
m(a){return a+1|0}return J(d[ae],g,m,h,f,i)}var
i=h(0,f);return[0,c[1]-1|0,i]}function
bO(a,f,e,c,i){try{var
j=aA(a,c,i),g=b(Z[4][2],f,j);if(b(d[48],a,g))var
h=[0,[1,e]];else
var
k=bM(a,f,c),h=[0,[0,bN(a,g),k]];return h}catch(a){a=v(a);if(a[1]===K)return 0;if(a===F)return[0,[1,e]];throw a}}function
aX(f,e,c){function
g(b){return a(d[11],f+b|0)}var
h=b(j[17][54],e,g);return b(d[z][4],h,c)}var
am=[0,K,aA,bO,function(f,e,c){var
a=e[1],g=c[2],h=c[1],i=aX(0,a,e[2]),k=aX(a,h,g);try{var
l=aA(f,i,k),m=function(c){var
e=c[1]<a?1:0,g=c[2];return e?e:b(d[48],f,g)},n=b(j[17][25],m,l);return n}catch(a){a=v(a);if(a[1]===K)return 0;throw a}}];N(bu,am,"Ground_plugin.Unify");function
aY(a){if(0===a[0]){var
b=a[1];if(typeof
b==="number")return 999;else
switch(b[0]){case
0:return 90;case
1:return 40;case
2:return-30;case
3:return 60;default:var
c=b[2];if(typeof
c==="number")return 0;else
switch(c[0]){case
0:return ae;case
1:return 80;case
2:return 70;case
3:return-20;case
4:return 50;default:return-10}}}var
d=a[1];if(typeof
d==="number")switch(d){case
0:return ae;case
1:return 40;case
2:return-15;case
3:return-50;default:return ae}return-29}var
bP=[0,function(b,a){var
c=aY(a[3]);return aY(b[3])-c|0}],aB=[0,T[136]],bQ=[0,function(c,a){var
e=a[2],f=c[2],d=b(an[18][1],c[1],a[1]);if(0===d){var
h=function(c,a){var
d=u.caml_int_compare(c[1],a[1]),e=a[2],f=c[2];return 0===d?b(aB[1],f,e):d};return g(_[5],h,f,e)}return d}],m=a(j[21][1],aB),$=a(j[20][1],bQ);function
ao(h,a,f,c){var
e=b(d[5],h,a);try{var
i=[0,f,b(m[22],e,c)],j=g(m[4],e,i,c);return j}catch(a){a=v(a);if(a===F)return g(m[4],e,[0,f,0],c);throw a}}function
aZ(i,h,f,a){var
c=b(d[5],i,h);try{var
k=b(m[22],c,a),l=function(a){return 1-b(an[5],a,f)},e=b(j[17][33],l,k),n=e?g(m[4],c,e,a):b(m[6],c,a);return n}catch(b){b=v(b);if(b===F)return a;throw b}}var
Q=a(aC[2],bP);function
bR(a){return[0,a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8]-1|0]}function
bS(c,a){var
d=a[8],e=b($[4],c,a[7]);return[0,a[1],a[2],a[3],a[4],a[5],a[6],e,d]}function
bT(m,c,e){var
f=b($[3],c,e[7]);if(f)var
h=f;else{var
i=c[2],n=c[1];if(i){var
j=i[1],k=j[1],o=j[2],l=function(c){var
e=c[2],p=c[1];if(e){var
f=e[1],h=f[1],q=f[2],i=b(an[5],n,p);if(i){var
j=k<h?1:0;if(j){var
r=[0,k,a(d[8],o)],s=[0,h,a(d[8],q)];return g(am[4],m,s,r)}var
l=j}else
var
l=i;return l}return 0};return b($[17],l,e[7])}var
h=0}return h}function
aD(j,g,f,e,i,a){var
h=R(o[9],j,g,f,e,i,a[6]);if(0===h[0]){var
c=h[1];if(1===f){var
k=a[8],l=a[7],m=a[6],n=c[2],p=a[3],q=a[2];return[0,b(Q[2],c,a[1]),q,p,n,0,m,l,k]}var
r=a[8],s=a[7],t=a[6],u=a[5],v=a[4],w=a[3],x=ao(g,c[2],e,a[2]);return[0,b(Q[2],c,a[1]),x,w,v,u,t,s,r]}var
d=h[1];if(1===f)return[0,a[1],a[2],a[3],d,[0,d],a[6],a[7],a[8]];var
y=a[8],z=a[7],A=a[6],B=a[5],C=a[4],D=[0,d,a[3]],E=ao(g,d,e,a[2]);return[0,a[1],E,D,C,B,A,z,y]}function
bU(c,b,a){function
d(a,b){return a[1]===o[7]?b:ao(c,a[2],a[1],b)}var
e=a[8],f=a[7],h=a[6],i=a[5],k=a[4],l=a[3],m=g(j[17][19],d,b,a[2]);return[0,g(j[17][19],Q[2],b,a[1]),m,l,k,i,h,f,e]}function
bV(f,e,c){var
g=c[2],h=b(d[5],f,e),i=b(m[22],h,g);return a(j[17][5],i)}function
bW(g,f){var
b=f;for(;;){var
c=a(Q[3],b[1]),d=a(Q[4],b[1]);if(c[1]===o[7]){var
e=[0,d,b[2],b[3],b[4],b[5],b[6],b[7],b[8]];if(b[4]===c[2])return[0,c,e];var
b=e;continue}var
h=b[8],i=b[7],j=b[6],k=b[5],l=b[4],m=b[3];return[0,c,[0,d,aZ(g,c[2],c[1],b[2]),m,l,k,j,i,h]]}}function
bX(e){var
b=[0,-1],f=$[1];function
c(a){if(a)b[1]++;return b[1]}var
g=a(d[11],1);return[0,Q[1],m[1],0,g,0,c,f,e]}function
bY(c){if(2===c[0]){var
d=c[1],e=function(a){return[3,[0,d,a+1|0]]},f=a(ai[21],d);return b(j[17][54],f,e)}return[0,c,0]}var
bZ=a(j[17][bB],bY);function
b0(b,f,e,c){var
h=a(bZ,e);function
i(e,c){var
i=c[1],f=R(aE[161],0,0,0,b,c[2],e),j=f[1],k=a(d[8],f[2]),g=r(ap[2],0,b,j,k),h=g[1];return[0,aD(b,h,0,e,g[2],i),h]}return g(j[17][19],i,h,[0,c,f])}function
b1(e,c,h,f){var
d=[0,f];function
i(i){var
f=a(aF[30],i[7]);switch(f[0]){case
0:case
2:case
3:var
h=f[1][1][1];try{var
j=b(B[bD],c,h)[1],k=g(ap[1],e,c,h);d[1]=aD(e,c,2,j,k,d[1]);var
l=0;return l}catch(a){a=v(a);if(a===F)return 0;throw a}default:return 0}}function
k(d,c,a){return b(j[17][14],i,a)}function
l(d){try{var
c=a(aF[15],d),e=c}catch(c){c=v(c);if(c!==F)throw c;var
f=b(x[16],d,b2),h=b(x[16],b3,f),i=a(n[3],h),e=g(aa[6],0,0,i)}return b(aF[14][12],k,e)}b(j[17][14],l,h);return[0,d[1],c]}function
b4(c){function
e(f,e,c){var
g=a(d[8],f),h=aE[16],i=a(ax[2],0),j=J(b5[6],0,0,i,h,g),k=a(n[14],0),l=a(a0[23],j),m=a(n[3],b6),o=b(n[36],aq[53],e),p=a(n[3],b7),q=b(n[12],p,o),r=b(n[12],q,m),s=b(n[12],r,l),t=b(n[12],s,k);return b(n[12],t,c)}var
f=a(n[3],b8),h=a(n[7],0),i=g(m[11],e,c,h),j=a(n[14],0),k=a(n[3],b9),l=b(n[12],k,j),o=b(n[12],l,i),p=b(n[12],o,f);return b(n[24],0,p)}var
q=[0,aB,[0,m[1],m[2],m[3],m[4],m[5],m[6],m[7],m[8],m[9],m[10],m[11],m[12],m[13],m[14],m[15],m[16],m[17],m[18],m[19],m[20],m[21],m[22],m[23],m[24]],$,ao,aZ,Q,bR,bS,bT,aD,bU,bV,bW,bX,b0,b1,b4];N(ah,q,"Ground_plugin.Sequent");function
t(t,l,k,s){function
c(c){a(b_[2],0);var
u=a(i[63][4],c),d=a(p[48][5],c),e=a(p[48][4],c);function
m(v,u,t){var
i=v,h=u,f=t;for(;;){if(0<i){if(h){var
o=h[2],k=h[1],l=a(Y[2][1][1],k),w=a(p[48][6],c);if(!r(B[33],d,e,l,w)){var
x=g(B[34],d,e,l);if(!b(j[17][26],x,f)){var
y=m(i-1|0,o,[0,k,f]),z=a(Y[2][1][3],k);return R(q[10],d,e,0,[0,l],z,y)}}var
i=i-1|0,h=o,f=[0,k,f];continue}var
A=a(n[3],b$);return g(aa[3],0,0,A)}return s}}var
f=m(t,u,0);if(l)var
v=a(p[48][6],c),h=R(q[10],d,e,1,o[7],v,f);else
var
h=f;return a(k,h)}return a(i[63][8],c)}function
ca(a){if(0===a[0])return a[1];throw[0,G,cb]}function
H(b){return 0===b[0]?a(h[74],[0,b[1],0]):e[66][2]}function
cc(d,c){function
f(f){try{var
k=function(b){return a(h[42],b)},l=a(p[48][4],f),m=g(q[12],l,d,c),o=a(e[66][58],m),r=b(i[68][1],o,k);return r}catch(c){c=v(c);if(c===F){var
j=a(n[3],cd);return b(e[66][4],0,j)}throw c}}return a(i[63][9],f)}function
ce(l,k,f,j,c){var
m=t(1,0,j,c),o=[0,h[16],0],p=[0,H(f),o];function
r(k){try{var
p=g(q[12],k,l,c),r=a(i[13],p),j=r}catch(c){c=v(c);if(c!==F)throw c;var
m=a(n[3],cf),j=b(e[66][4],0,m)}function
o(c){function
g(c){function
g(b){var
e=[0,a(d[21],[0,b,[0,c]]),0];return a(h[S],e)}var
j=a(e[66][58],f);return b(i[68][1],j,g)}var
j=a(e[66][58],c);return b(i[68][1],j,g)}return b(i[68][1],j,o)}var
s=[0,b(i[68][1],i[51],r),p],u=a(e[66][20],s);return g(e[66][25],u,m,k)}function
cg(c,b,a){var
d=t(0,1,b,a);return g(e[66][25],h[bB],d,c)}function
ch(f,d,c){var
g=t(0,1,d,c),i=[0,a(e[66][31],g)],j=b(h[bu],0,i);return b(e[66][12],j,f)}function
ci(f,d,c){var
i=t(1,1,d,c),j=a(e[66][31],i),k=b(e[66][3],h[17],j),l=b(e[66][12],k,f),m=t(1,1,d,c);return g(e[66][25],h[16],m,l)}function
cj(l,k,c,j,f){function
d(m){var
n=a(p[48][5],m),d=I(b(o[5],n,l),0)[1],q=t(d,0,j,f),r=[0,b(e[66][28],d,h[16]),0],s=[0,H(c),r],u=h[98],v=a(e[66][58],c),w=[0,b(i[68][1],v,u),s],x=a(e[66][20],w);return g(e[66][25],x,q,k)}return a(i[63][9],d)}function
ck(l,d,c,k,f){function
m(m){var
n=a(p[48][5],m),q=b(o[5],n,l);function
r(d){var
g=[0,t(d,0,k,f),0],i=[0,b(e[66][28],d,h[16]),g],j=[0,H(c),i];return a(e[66][20],j)}var
s=b(j[19][15],r,q),u=h[98],v=a(e[66][58],c),w=b(i[68][1],v,u);return g(e[66][26],w,s,d)}return a(i[63][9],m)}function
cl(c){var
d=h[98],f=a(e[66][58],c);return b(i[68][1],f,d)}function
cm(c,l,s,k,r,q){var
u=c[2],v=c[1];function
f(m){var
w=a(p[48][4],m),x=a(p[48][5],m),n=J(o[6],x,w,0,c,l),f=n.length-1,y=a(j[19][12],l),A=t(f,0,r,q),B=[0,b(e[66][28],f,h[16]),0],C=[0,H(k),B];function
D(r){function
c(e){var
f=I(n,e)[e+1],c=a(j[17][1],f),g=[0,[0,v,e+1|0],a(d[2][1],u)],h=[0,a(d[28],g),y],i=a(d[21],h);function
k(b){return a(d[9],c-b|0)}var
l=b(j[19][2],c,k),m=[0,b(d[z][1],c,i),l],o=[0,a(d[21],m)],p=[0,b(d[z][1],c,r),o],q=a(d[21],p);return b(d[38],q,f)}var
e=b(j[17][54],f,c);return a(h[S],e)}var
E=a(e[66][58],k),F=[0,b(i[68][1],E,D),C],G=a(e[66][20],F);return g(e[66][25],G,A,s)}return a(i[63][9],f)}function
cn(k,j,m,l,c,g,f){var
n=[0,0,k,b(d[z][1],1,j)],o=a(d[18],n),r=t(2,1,g,f),s=[0,a(e[66][31],r),0],u=[0,h[17],[0,h[17],s]],p=0,q=0,v=[0,H(c),u];function
w(i){var
c=a(d[9],2),e=[0,0,b(d[z][1],1,k),c],f=[0,i,[0,a(d[19],e)]],g=[0,0,j,a(d[21],f)],l=[0,a(d[19],g),0];return a(h[S],l)}var
x=a(e[66][58],c),y=[0,b(i[68][1],x,w),v],A=[0,a(e[66][20],y),q];function
B(b){return a(h[42],b)}var
C=a(e[66][58],c),D=[0,b(i[68][1],C,B),A],E=a(h[ag],o),F=[0,b(e[66][19],E,D),p],G=[0,t(1,0,g,f),0],I=[0,H(c),G],J=[0,a(e[66][20],[0,h[17],I]),F],K=a(h[ag],m),L=b(e[66][19],K,J);return b(e[66][12],L,l)}function
co(f,d,c){if(o[1][1])var
j=a(n[3],cp),i=b(e[66][4],0,j);else
var
i=f;var
k=t(0,1,d,c),l=a(e[66][31],k),m=b(e[66][3],h[17],l),p=b(e[66][12],m,f),q=t(0,1,d,c),r=g(e[66][25],h[16],q,p);return b(e[66][12],r,i)}function
cq(l,k,c,j,f){function
d(m){var
n=a(p[48][5],m),d=I(b(o[5],n,l),0)[1],q=[0,t(d-1|0,0,j,f),0],r=[0,b(e[66][28],d,h[16]),q],s=[0,H(c),r],u=a(e[66][20],s),v=h[98],w=a(e[66][58],c),x=b(i[68][1],w,v);return g(e[66][25],x,u,k)}return a(i[63][9],d)}function
cr(l,k,g,f,c){var
m=t(0,1,f,a(q[7],c)),n=[0,a(e[66][31],m),0],o=t(1,0,f,a(q[7],c)),r=[0,a(e[66][31],o),0],s=[0,h[16],r],u=[0,H(g),s];function
v(f){function
c(g){var
i=a(p[48][12],g),c=b(j[17][7],i,0),k=[0,f,[0,a(d[10],c)]],l=a(d[21],k),m=a(h[74],[0,c,0]),n=a(h[S],[0,l,0]);return b(e[66][3],n,m)}return a(i[63][9],c)}var
w=a(e[66][58],g),x=[0,b(i[68][1],w,v),u],y=[0,a(e[66][20],[0,h[16],x]),n],z=a(h[ag],l),A=b(e[66][19],z,y);return b(e[66][12],A,k)}function
a1(b){var
c=g(cu[2],ct,cs,b);return a(cv[45],c)}var
L=[aP,function(e){var
b=a1(cw),c=[0,[0,0,[1,a(T[40],b)[1]]],0],d=a1(cx);return[0,[0,0,[1,a(T[40],d)[1]]],c]}];function
cy(d){var
f=a(p[48][12],d);function
g(d){var
c=bo(L),e=[0,d,1],f=bt===c?L[1]:aP===c?a(a2[2],L):L;return b(h[68],f,e)}var
c=bo(L),j=b(e[66][21],g,f),k=bt===c?L[1]:aP===c?a(a2[2],L):L,l=a(h[67],k);return b(i[68][2],l,j)}var
s=[0,t,ca,H,cc,ce,cg,ch,ci,cj,ck,cl,cm,cn,co,cq,cr,a(i[63][9],cy)];N(132,s,"Ground_plugin.Rules");function
cz(e,c){function
f(e,c){var
f=a(d[ah][1],c),g=a(d[ah][1],e);return b(q[1][1],g,f)}if(0===e[0]){var
g=e[1],h=g[1],j=e[2],k=g[2];if(0===c[0]){var
i=c[1],l=c[2],m=i[2],n=i[1],p=function(b,a){return b-a|0},r=function(b,a){return b-a|0},s=b(o[3],r,p);return R(b(o[4],s,f),n,h,j,l,k,m)}return 0===h?1:-1}var
t=e[1];return 0===c[0]?0===c[1][1]?-1:1:f(t,c[1])}function
cA(c,a){return c===a?0:c===o[7]?1:a===o[7]?-1:b(an[18][1],c,a)}var
cB=[0,function(c,a){var
d=a[2],e=a[1],f=c[2],g=c[1];return r(b(o[3],cz,cA),e,g,d,f)}],ar=a(j[20][1],cB);function
a3(E,c,l){var
d=[0,ar[1]];function
e(f){var
i=f[3];if(0===i[0]){var
c=i[1];if(typeof
c==="number")var
m=1;else
if(2===c[0])var
w=c[3],k=c[2],v=c[1],h=1,m=0;else
var
m=1;if(m)var
h=0}else{var
e=i[1];if(typeof
e==="number")var
h=0;else
var
w=e[3],k=e[2],v=e[1],h=1}if(h){var
x=f[4],p=[0,1],r=[0,w],C=f[1],s=function(c,a){function
e(f,e){var
a=J(am[3],E,v,k,f,e);if(a){var
c=a[1];return 0===c[0]?(p[1]=0,d[1]=b(ar[4],[0,c,C],d[1]),0):(r[1]=1,0)}return 0}var
f=c[1];function
g(c){var
d=a[2];function
f(a){return e(c,a)}return b(j[17][14],f,d)}b(j[17][14],g,f);var
h=c[2];function
i(c){var
d=a[1];function
f(a){return e(c,a)}return b(j[17][14],f,d)}return b(j[17][14],i,h)},z=l[1],A=function(a){return s(x,a[4])};b(q[6][5],A,z);var
o=l[5],y=o?[0,o[1],0]:0;s(x,[0,y,l[3]]);var
t=p[1],u=t?r[1]:t,D=u?(d[1]=b(ar[4],[0,[1,k],f[1]],d[1]),0):u;return D}var
B=a(n[3],cC);return g(aa[3],0,0,B)}b(j[17][14],e,c);return a(ar[21],d[1])}function
a4(f,c){try{var
g=b(q[13],f,c),h=g[1],a=h[3],l=g[2];if(0===a[0]){var
i=a[1];if(typeof
i==="number")var
e=1;else
if(2===i[0])var
d=1,e=0;else
var
e=1;if(e)var
d=0}else
var
d=typeof
a[1]==="number"?0:1;if(d)var
k=a4(f,l),j=[0,[0,h,k[1]],k[2]];else
var
j=[0,0,c];return j}catch(a){a=v(a);if(a===aC[1])return[0,0,c];throw a}}var
a5=a(w[1][6],cD);function
cE(i,c,w,v,f,u){if(w===o[7])var
p=a5;else
var
C=g(ap[1],i,c,v),D=g(X[29],i,c,C),t=b(d[69],c,D)[1],E=t?t[1]:a5,p=E;function
x(b){return a(d[9],f-b|0)}var
y=b(j[17][54],f,x),n=f,m=0,e=i,l=c,k=0,A=b(d[z][4],y,u);for(;;){if(0===n)return[0,l,k,A];var
q=g(h[13],m,p,e),r=bp(cF[7],e,l,0,0,0,0,aE[z]),s=[0,[0,q],r[2][1]],B=r[1],n=n-1|0,m=[0,q,m],e=b(d[106],s,e),l=B,k=[0,s,k];continue}}var
U=[0,a4,a3,function(f,c,m,k){function
l(l){var
t=a3(a(p[48][4],l),f,k);function
u(f){if(f[2]===o[7])var
c=f[1],l=function(g,k){function
f(F){if(0===c[0]){var
f=c[1];if(0===f[1]){var
l=f[2],m=a(q[7],k),o=[0,r(s[1],0,1,g,m),0],t=a(e[66][32],o),u=a(h[bq],[0,[0,l,0]]);return b(e[66][3],u,t)}var
v=a(n[3],cJ);return b(e[66][4],0,v)}var
w=c[1],x=[0,a(e[66][22],h[41]),0],y=a(q[7],k),z=[0,r(s[1],0,1,g,y),0],A=[0,a(e[66][32],z),0];function
B(c){var
e=a(p[48][12],c),f=b(j[17][7],e,0),g=[0,[0,a(d[10],f),0]];return a(h[bq],g)}var
C=[0,a(i[63][9],B),A],D=[0,a(e[66][20],[0,h[17],C]),x],E=a(h[ag],w);return b(e[66][19],E,D)}return a(i[63][9],f)};else
var
l=function(x,k){var
c=f[2],l=f[1];function
m(y){var
f=a(p[48][4],y);if(0===l[0]){var
t=l[1],m=t[2],o=t[1],u=[0,o,b(d[5],f,m)];if(g(q[9],f,[0,c,[0,u]],k)){var
z=a(n[3],cG);return b(e[66][4],0,z)}if(0<o)var
A=function(j){function
e(e){var
q=a(p[48][4],e),f=cE(a(p[48][5],e),q,c,j,o,m),s=f[2],t=f[1],u=a(d[21],[0,j,[0,f[3]]]),k=b(d[38],u,s);try{var
A=a(p[48][5],e),B=r(ap[2],0,A,t,k),l=B}catch(b){b=v(b);if(!a(aa[20],b))throw b;var
w=a(n[3],cH),l=g(aa[6],0,0,w)}var
x=l[1],y=a(h[S],[0,k,0]),z=a(i[61][1],x);return b(i[15],z,y)}return a(i[63][9],e)},B=a(e[66][58],c),w=b(i[68][1],B,A);else
var
G=function(b){var
c=[0,a(d[21],[0,b,[0,m]]),0];return a(h[S],c)},H=a(e[66][58],c),w=b(i[68][1],H,G);var
C=b(q[8],[0,c,[0,u]],k),D=a(q[7],C),E=[0,r(s[1],1,0,x,D),0],F=[0,a(e[66][32],E),0];return a(e[66][20],[0,w,[0,h[17],F]])}var
I=l[1];if(g(q[9],f,[0,c,0],k)){var
J=a(n[3],cI);return b(e[66][4],0,J)}var
K=[0,a(e[66][22],h[41]),0],L=b(q[8],[0,c,0],k),M=a(q[7],L),N=[0,r(s[1],1,0,x,M),0],O=[0,a(e[66][32],N),0],P=[0,h[17],O];function
Q(c){function
e(e){var
f=a(p[48][12],e),g=b(j[17][7],f,0),i=[0,c,[0,a(d[10],g)]],k=[0,a(d[21],i),0];return a(h[S],k)}return a(i[63][9],e)}var
R=a(e[66][58],c),T=[0,b(i[68][1],R,Q),P],U=[0,a(e[66][20],[0,h[17],T]),K],V=a(h[ag],I);return b(e[66][19],V,U)}return a(i[63][9],m)};return l(m,k)}var
w=b(j[17][15],u,t),x=a(e[66][24],w);return b(e[66][12],x,c)}return a(i[63][9],l)}];N(134,U,"Ground_plugin.Instances");var
aG=[0,function(_,l){function
c(m){var
c=[0,w[18][1]];function
d(d){try{var
e=a(a6[23],d),f=a(T[40],e)[1];c[1]=b(w[18][7],f,c[1]);var
g=0;return g}catch(a){a=v(a);if(a===T[27])return 0;throw a}}var
f=a(a6[26],0);b(a7[15],d,f);var
h=a(w[18][12],c[1]),j=b(av[8][13],av[14],[0,w[1][12][2],h]);o[2][1]=j;function
A(C,l){function
c(w){if(u.caml_equal(a(k[12][8],0),cK)){var
$=a(p[48][4],w),aa=a(i[63][1],w),ab=[0,a(i[63][13],aa),$],ac=a(aq[79],ab);b(a8[10],0,ac)}try{var
ae=a(p[48][4],w),I=b(q[13],ae,l),h=I[2],j=I[1],d=function(b){var
c=a(p[48][4],w);return g(q[11],c,C,b)},af=0,f=function(a){return A(af,a)},c=A([0,j,C],h),D=j[3];if(0===D[0]){var
t=D[1];if(typeof
t==="number")var
y=a(s[11],j[1]);else
switch(t[0]){case
0:var
ag=t[1],ah=d(h),y=J(s[9],ag,c,j[1],f,ah);break;case
1:var
ai=t[1],aj=d(h),y=J(s[10],ai,c,j[1],f,aj);break;case
2:var
ak=a(p[48][4],w),L=b(U[1],ak,l),M=L[1],al=L[2],N=A(b(x[25],M,C),al);if(o[1][1])if(0<l[8])var
am=d(l),O=r(U[3],M,N,f,am),E=1;else
var
E=0;else
var
E=0;if(!E)var
O=N;var
y=O;break;case
3:var
an=t[1];if(o[1][1])var
ao=d(h),P=J(s[15],an,c,j[1],f,ao);else
var
P=c;var
y=P;break;default:var
m=t[2],ap=t[1];if(typeof
m==="number")var
B=c;else
switch(m[0]){case
3:var
av=m[1];if(0<l[8])if(o[1][1])var
aw=d(h),Q=J(s[16],av,c,j[1],f,aw),F=1;else
var
F=0;else
var
F=0;if(!F)var
Q=c;var
B=Q;break;case
4:var
ax=m[2],ay=m[1];if(o[1][1])var
az=d(h),S=R(s[12],ay,ax,c,j[1],f,az);else
var
S=c;var
B=S;break;case
5:var
aA=m[3],aB=m[2],aD=m[1],aE=d(h),B=bp(s[13],aD,aB,aA,c,j[1],f,aE);break;default:var
as=m[2],at=m[1],au=d(h),B=R(s[12],at,as,c,j[1],f,au)}var
ar=d(h),y=J(s[5],ap,B,j[1],f,ar)}var
K=y}else{var
T=D[1];if(typeof
T==="number")switch(T){case
0:var
aF=d(h),z=g(s[8],c,f,aF);break;case
1:var
aG=d(h),z=g(s[6],c,f,aG);break;case
2:var
aH=d(h),z=g(s[7],c,f,aH);break;case
3:var
z=c;break;default:if(o[1][1])var
aI=a(n[3],cL),V=b(e[66][4],0,aI);else
var
V=c;var
aJ=d(h),z=g(s[14],V,f,aJ)}else{var
aK=a(p[48][4],w),W=b(U[1],aK,l),X=W[1],aL=W[2],Y=A(b(x[25],X,C),aL);if(o[1][1])if(0<l[8])var
aM=d(l),Z=r(U[3],X,Y,f,aM),G=1;else
var
G=0;else
var
G=0;if(!G)var
Z=Y;var
z=Z}var
K=z}var
H=K}catch(a){a=v(a);if(a!==aC[1])throw a;var
H=_}var
ad=b(s[4],l[4],l);return b(e[66][12],ad,H)}return a(i[63][9],c)}var
t=a(i[63][4],m),y=a(a7[1],t);return a(l,function(a){var
b=0;function
c(a){return A(b,a)}return r(s[1],y,1,c,a)})}return a(i[63][9],c)}];N(139,aG,"Ground_plugin.Ground");a(aH[12],M);var
ab=[0,3];function
cM(a){return a?(ab[1]=b(x[5],a[1],0),0):(ab[1]=3,0)}var
cP=[0,0,cO,cN,function(a){return[0,ab[1]]},cM];b(a9[3],0,cP);var
as=[0,ae];function
cQ(a){return a?(as[1]=b(x[5],a[1],0),0):(as[1]=0,0)}var
cT=[0,0,cS,cR,function(a){return[0,as[1]]},cQ];b(a9[3],0,cT);var
cV=[0,a_,0],cW=[0,function(b,a){return r(cU[14],0,0,0,0)}];g(k[6][9],0,a_,cW);var
a$=[31,b(C[10],0,[0,cV,0])],aI=b(k[14][1],[0,a$],cX),ba=aI[3],bb=aI[2],bc=aI[1],cY=0,c0=[0,[0,0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(l[4],k[1][1]),f=b(l[8],e,d);return function(e){var
c=a(k[8][3],f),d=a(bd[10][2],0);return b(bc,a(bd[6],d),c)}}return a(x[2],cZ)}],cY];function
c1(b,a){return g(be[1],a[1],[0,c2,b],a[2])}b(V[87],c1,c0);var
c3=0,c5=[0,function(b){if(b)if(!b[2])return function(a){return at[6]};return a(x[2],c4)},c3];function
c6(c,a){return b(at[3],[0,c7,c],a)}b(V[87],c6,c5);var
c8=[6,a(D[12],k[1][1])],c9=[0,[0,a(l[4],k[1][1])],c8],db=[0,[0,da,[0,c$,[0,c_,[0,[1,b(C[10],0,c9)],0]]]],0];function
dc(b,a){return g(bf[1],[0,dd,b],0,a)}b(V[87],dc,db);var
de=0,dh=[0,[0,0,function(c){return c?a(x[2],df):function(f){var
c=a(ba,0),d=a(n[3],dg),e=b(n[12],d,c);return b(a8[6],0,e)}}],de];function
di(b,a){return g(be[1],a[1],[0,dj,b],a[2])}b(V[87],di,dh);var
dk=0,dm=[0,function(b){return b?a(x[2],dl):function(a){return at[5]}},dk];function
dn(c,a){return b(at[3],[0,dp,c],a)}b(V[87],dn,dm);function
dr(b,a){return g(bf[1],[0,ds,b],0,a)}b(V[87],dr,dq);var
du=a(n[3],dt),dv=b(e[66][4],0,du);function
ac(h,c,g,f){var
d=o[1][1];function
j(a){var
c=a[2],e=a[1];o[1][1]=d;return b(i[18],[0,c],e)}function
k(m){o[1][1]=h;var
j=c?c[1]:a(bb,0)[2];function
k(h){function
c(c){var
j=a(q[14],ab[1]),k=a(p[48][4],c),l=a(p[48][5],c),m=r(q[15],l,k,g,j)[1],n=a(p[48][4],c),o=a(p[48][5],c),d=r(q[16],o,n,f,m),s=d[2],t=a(h,d[1]),u=a(i[61][1],s);return b(e[66][3],u,t)}return a(i[63][9],c)}var
l=b(aG[1],j,k);o[1][1]=d;return l}var
l=a(i[63][9],k);return b(i[19],l,j)}function
bg(d,c,b){return a(k[2][24],dw[41])}function
bh(f,e,d){function
b(b){return a(aq[53],b[2])}var
c=a(a0[6],b);return a(k[2][24],c)}function
bi(d,c,b){return a(k[2][24],aq[53])}function
dx(b){return a(n[22],dy)}var
bj=r(dB[2],dA,dz,0,dx),y=a(l[2],dC);function
dD(c,d){var
e=a(l[17],E[23]),f=a(l[4],e),g=b(l[7],f,d),h=b(k[8][10],c,g),i=a(l[17],E[23]),j=a(l[5],i);return[0,c,b(l[8],j,h)]}b(bk[9],y,dD);function
dE(d,c){var
e=a(l[17],E[23]),f=a(l[5],e),g=b(l[7],f,c),h=b(k[5][2],d,g),i=a(l[17],E[23]),j=a(l[5],i);return b(l[8],j,h)}b(bk[10],y,dE);function
dF(d,c){var
e=a(l[17],E[23]),f=a(l[5],e),g=b(l[7],f,c);return b(k[12][9],d,g)}b(aJ[6],y,dF);var
dG=a(l[17],E[23]),dH=a(l[6],dG),dI=[0,a(aJ[2],dH)];b(aJ[3],y,dI);var
dJ=a(l[4],y),aK=g(D[13],D[9],dK,dJ),dL=0,dM=0;function
dN(a,c,b){return[0,a,0]}var
dO=[6,D[14][16]],dQ=[0,[0,[0,[0,0,[0,a(ad[11],dP)]],dO],dN],dM];function
dR(b,e,a,d,c){return[0,a,b]}var
dT=[0,a(ad[11],dS)],dU=[2,[6,D[14][16]],dT],dW=[0,a(ad[11],dV)],dX=[6,D[14][16]],dZ=[0,[0,[0,[0,[0,[0,0,[0,a(ad[11],dY)]],dX],dW],dU],dR],dQ];function
d0(d,c,a,f,e){b(bj,0,0);return[0,a,[0,c,d]]}var
d1=[3,[6,D[14][16]]],d2=[6,D[14][16]],d3=[6,D[14][16]],d5=[0,[0,[0,[0,[0,[0,0,[0,a(ad[11],d4)]],d3],d2],d1],d0],dZ],d6=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],d5]],dL]];g(D[22],aK,0,d6);r(k[2][1],y,bg,bh,bi);var
d7=[0,aK,0];function
d8(c){var
d=c[2],e=a(l[4],y);return[0,b(l[7],e,d)]}g(k[9][5],d9,d8,d7);var
d_=0,ea=[0,function(c){if(c){var
d=c[2];if(d){var
e=d[2];if(e)if(!e[2]){var
f=e[1],g=d[1],h=c[1],i=a(l[18],k[1][1]),j=a(l[6],i),m=b(k[12][2][7],j,h),n=a(l[6],y),o=b(k[12][2][7],n,g),p=a(l[17],E[22]),q=a(l[6],p),r=b(k[12][2][7],q,f);return function(c){var
d=a(k[12][23],c);return ac(1,b(_[15],d,m),o,r)}}}}return a(x[2],d$)},d_],ec=[0,function(c){if(c){var
d=c[2];if(d)if(!d[2]){var
e=d[1],f=c[1],g=a(l[18],k[1][1]),h=a(l[6],g),i=b(k[12][2][7],h,f),j=a(l[17],E[22]),m=a(l[6],j),n=b(k[12][2][7],m,e);return function(c){var
d=a(k[12][23],c);return ac(1,b(_[15],d,i),0,n)}}}return a(x[2],eb)},ea],ee=[0,function(c){if(c){var
d=c[2];if(d)if(!d[2]){var
e=d[1],f=c[1],g=a(l[18],k[1][1]),h=a(l[6],g),i=b(k[12][2][7],h,f),j=a(l[6],y),m=b(k[12][2][7],j,e);return function(c){var
d=a(k[12][23],c);return ac(1,b(_[15],d,i),m,0)}}}return a(x[2],ed)},ec],ef=a(bl[12],ee);g(k[6][9],0,[0,M,eg],ef);function
eh(F){var
l=[0,a(w[1][7],ei)],c=E[22],i=0,j=0;if(0===c[0]){var
m=[0,ek,[0,[1,b(C[10],0,[0,[0,[5,[0,c[1]]]],l])],j]],n=[0,a(w[1][7],el)];if(0===y[0]){var
o=[0,[1,b(C[10],0,[0,[5,[0,y[1]]],n])],m],p=[0,a(w[1][7],en)],d=k[1][1];if(0===d[0]){var
q=[0,[0,ep,[0,[1,b(C[10],0,[0,[4,[5,[0,d[1]]]],p])],o]],i],s=[0,a(w[1][7],eq)],e=E[22],r=0;if(0===e[0]){var
t=[0,es,[0,[1,b(C[10],0,[0,[0,[5,[0,e[1]]]],s])],r]],u=[0,a(w[1][7],et)],f=k[1][1];if(0===f[0]){var
v=[0,[0,ev,[0,[1,b(C[10],0,[0,[4,[5,[0,f[1]]]],u])],t]],q],x=0,z=[0,a(w[1][7],ew)];if(0===y[0]){var
A=[0,[1,b(C[10],0,[0,[5,[0,y[1]]],z])],x],B=[0,a(w[1][7],ey)],h=k[1][1];if(0===h[0]){var
D=[0,[0,eA,[0,[1,b(C[10],0,[0,[4,[5,[0,h[1]]]],B])],A]],v];return g(k[9][4],[0,M,eB],0,D)}throw[0,G,ez]}throw[0,G,ex]}throw[0,G,eu]}throw[0,G,er]}throw[0,G,eo]}throw[0,G,em]}throw[0,G,ej]}b(aH[19],eh,M);var
eC=0,eE=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(l[18],k[1][1]),f=a(l[6],e),g=b(k[12][2][7],f,d);return function(c){var
d=a(k[12][23],c);return ac(0,b(_[15],d,g),0,0)}}return a(x[2],eD)},eC],eF=a(bl[12],eE);g(k[6][9],0,[0,M,eG],eF);function
eH(i){var
f=[0,a(w[1][7],eI)],c=k[1][1],d=0,e=0;if(0===c[0]){var
h=[0,[0,eK,[0,[1,b(C[10],0,[0,[4,[5,[0,c[1]]]],f])],e]],d];return g(k[9][4],[0,M,eL],0,h)}throw[0,G,eJ]}b(aH[19],eH,M);var
bm=[0,M,ab,as,a$,bc,bb,ba,dv,ac,bg,bh,bi,bj,y,aK];N(158,bm,"Ground_plugin.G_ground");N(159,[0,o,am,q,s,U,aG,bm],"Ground_plugin");return});
