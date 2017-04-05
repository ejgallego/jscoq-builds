(function(eW){"use strict";var
aV="Firstorder",ai=145,ak=108,aU="$l",bz="already done",aS="firstorder_using",bJ=",",bB=250,bC=105,av="$t",aN="gintuition",T=110,U=148,aQ=246,bI=115,P="Extension: cannot occur",L=120,bA=113,bH="with",by=118,bx="Depth",ah="firstorder",bw=137,aT="ground_plugin",aP="Firstorder_Print_Solver",au=100,aR=109,bG="Solver",Q="plugins/firstorder/g_ground.ml4",aj=136,bF=248,aM="Firstorder_Set_Solver",aO="using",bE="-----",bD="reversible in 1st order mode",w=eW.jsoo_runtime,K=w.caml_check_bound,bt=w.caml_fresh_oo_id,c=w.caml_new_string,bu=w.caml_obj_tag,O=w.caml_register_global,u=w.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):w.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):w.caml_call_gen(a,[b,c])}function
h(a,b,c,d){return a.length==3?a(b,c,d):w.caml_call_gen(a,[b,c,d])}function
q(a,b,c,d,e){return a.length==4?a(b,c,d,e):w.caml_call_gen(a,[b,c,d,e])}function
F(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):w.caml_call_gen(a,[b,c,d,e,f])}function
aL(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):w.caml_call_gen(a,[b,c,d,e,f,g])}function
bv(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):w.caml_call_gen(a,[b,c,d,e,f,g,h])}var
e=w.caml_get_global_data(),S=c(aT),bd=[0,c(aT),c("auto_with")],g=e.Term,j=e.Util,_=e.Context,B=e.Vars,t=e.Termops,ao=e.Hipattern,ax=e.Global,al=e.Inductiveops,G=e.CClosure,s=e.Tacmach,x=e.Names,$=e.Int,H=e.Not_found,A=e.Queue,aa=e.Evd,aA=e.Reductionops,I=e.Assert_failure,n=e.Pp,a2=e.Ppconstr,ar=e.Printer,aE=e.Hints,y=e.Pervasives,ae=e.CErrors,ab=e.Globnames,ac=e.Option,aC=e.Heap,a4=e.CamlinternalLazy,i=e.Tactics,f=e.Proofview,d=e.Tacticals,a8=e.Refiner,a9=e.Sigma,k=e.Ltac_plugin,ba=e.Feedback,a$=e.List,a_=e.Classops,bq=e.Cc_plugin,bc=e.Auto,C=e.Loc,l=e.Genarg,E=e.Stdarg,bj=e.Egramml,at=e.Vernac_classifier,bi=e.Vernacinterp,bh=e.Locality,aG=e.Mltop,bb=e.Goptions,X=e.CList,D=e.Pcoq,bo=e.Genintern,aJ=e.Geninterp,ag=e.CLexer,bp=e.Array,aW=[0,1],aw=[0,G[14]],bM=c("Formula.Is_atom"),bO=[0,0,[0,0,0]],bP=c("_"),bR=c("Unify.UFAIL"),ca=c(" : "),cb=c("| "),cc=c(bE),cd=c(bE),b8=c(" : No such Hint database"),b9=c("Firstorder: "),cB=c("iff"),cC=c("not"),cy=[0,c("Init"),[0,c("Logic"),0]],cz=c("User"),cv=c(bD),cl=c("No link"),cj=c("No axiom link"),ch=[0,c("plugins/firstorder/rules.ml"),53,7],cf=c("Not the expected number of hyps"),cQ=c("not implemented ... yet"),cN=c("Untypable instance, maybe higher-order non-prenex quantification"),cM=c(bz),cP=c(bz),cH=c("can't happen"),cI=c("x"),cR=[0,0],cS=c(bD),eQ=[0,c(Q),1,0],eP=c(av),eR=[0,c(aN)],eS=c(aN),eK=c(P),eG=[0,c(Q),1,0],eE=[0,c(Q),1,0],eB=[0,c(Q),1,0],ey=[0,c(Q),1,0],ev=[0,c(Q),1,0],et=[0,c(Q),1,0],eq=[0,c(Q),1,0],ep=c("$l'"),er=[0,c(bH)],es=c(aU),eu=c(av),ew=[0,c(ah)],ex=c(aU),ez=[0,c(bH)],eA=c(av),eC=[0,c(ah)],eD=c(aU),eF=c(av),eH=[0,c(ah)],eI=c(ah),ek=c(P),ei=c(P),eg=c(P),dF=c('Deprecated syntax; use "," as separator'),dz=c(aP),dw=c(aP),dt=c(P),dr=c(aP),dn=c("Firstorder solver tactic is "),dm=c(P),dk=c(aM),dc=c(aM),c$=c(P),c9=c(aM),c6=c(P),cT=c(aT),cV=[0,c(aV),[0,c(bx),0]],cW=c("Firstorder Depth"),cZ=[0,c("Congruence"),[0,c(bx),0]],c0=c("Congruence Depth"),c4=c("Firstorder default solver"),df=[0,c(bG)],dg=[0,c(aV)],dh=[0,c("Set")],dx=[0,[0,[0,c("Print")],[0,[0,c(aV)],[0,[0,c(bG)],0]]],0],dA=c("GTauto failed"),dG=c("deprecated"),dH=c("firstorder-deprecated-syntax"),dJ=c(aS),dR=c(aS),dW=c(aO),dZ=c(bJ),d2=c(bJ),d5=c(aO),d$=c(aO),ee=c(aS),en=c(ah),eN=c(aN),b$=e.Constrextern,cA=e.Coqlib,ce=e.Control,cO=e.Typing,cK=e.Evarutil,cL=e.Environ,dD=e.Libnames,dI=e.CWarnings,eV=e.Decl_mode_plugin;function
bK(h,g,f,e,d,c){var
a=b(h,f,e);return 0===a?b(g,d,c):a}function
bL(j,i,h,g,f,e,d,c){var
a=q(j,h,g,f,e);return 0===a?b(i,d,c):a}var
Z=[bF,bM,bt(0)];function
aX(h,f){var
b=h,c=f;for(;;){var
d=a(g[aj],c);if(6===d[0]){var
e=d[3];if(0<b){var
b=b-1|0,c=e;continue}return 1+aX(0,e)|0}return 0}}function
bN(c,d){var
e=a(ax[26],c[1])[1][6],f=a(s[8],d),g=b(al[4],f,c);function
h(a){return aX(e,a)}return b(j[19][15],h,g)}function
am(f,e,d,c){var
h=a(s[8],c),i=b(al[4],h,e);function
k(c){var
e=b(g[76],c,d),h=b(g[85],f,e)[2];return a(g[83],h)[1]}return b(j[19][15],k,i)}function
ay(c){var
d=a(s[8],c),e=h(G[42],0,aw[1],d);return function(c){var
d=a(G[36],c);return b(G[46],e,d)}}function
an(m,D){var
E=ay(m),A=a(s[8],m),B=h(G[42],0,aw[1],A),C=a(G[36],D),c=b(G[47],B,C),n=a(ao[23],c);if(n){var
o=n[1],F=o[1];return[0,F,a(t[56],o[2])]}var
p=a(ao[21],c);if(p){var
q=p[1];return[5,q[2],q[3]]}var
r=a(ao[27],c);if(r){var
e=r[1],f=e[2],H=e[3],u=a(g[43],e[1]),i=u[2],d=u[1],v=a(ax[26],d),k=v[2],w=v[1],x=k[4].length-1;if(0===x)return[1,[0,d,i],f];var
I=0<H?1:0,J=function(b){var
c=w[6];return a(t[73],b)===c?1:0},l=b(j[19][28],J,k[9]);if(!a(al[19],[0,d,w,k])){var
L=I?l?1:0:1;if(L)return 1===x?[2,[0,d,i],f,l]:[3,[0,d,i],f,l]}return[6,c]}var
y=a(ao[29],c);if(y){var
z=y[1],K=z[2];return[4,a(g[43],z[1]),K]}return[6,a(E,c)]}var
bQ=[0,a(x[1][6],bP)];function
aY(l,o,e,c){var
m=[0,0],i=[0,0],k=[0,0],y=ay(l);function
f(A,d,z){var
e=A,n=z;for(;;){var
c=an(l,n);switch(c[0]){case
0:var
C=c[2];f(e,1-d,c[1]);var
n=C;continue;case
1:var
r=1-d,D=r?(m[1]=1,0):r;return D;case
4:var
v=c[2],J=c[1],L=a(o,1),M=a(g[T],L),N=K(am(1,J,v,l),0)[1],O=function(g,i,c){var
h=a(_[1][1][3],c);return f([0,M,e],d,b(B[8],g,h))},P=2-a(j[17][1],v)|0;return q(j[17][83],O,P,0,N);case
5:var
Q=c[2],R=a(o,1),e=[0,a(g[T],R),e],n=Q;continue;case
6:var
p=h(B[12],e,0,c[1]),w=1-a(g[7],p);if(w){if(d){i[1]=[0,p,i[1]];return 0}k[1]=[0,p,k[1]];var
x=0}else
var
x=w;return x;default:var
E=c[2],F=c[1];if(c[3]){var
s=a(y,h(B[12],e,0,n));if(d)i[1]=[0,s,i[1]];else
k[1]=[0,s,k[1]]}var
t=am(0,F,E,l),G=function(g,i,c){var
h=a(_[1][1][3],c);return f(e,d,b(B[8],g,h))},H=function(b){var
c=1-a(j[17][1],b)|0;return q(j[17][83],G,c,0,b)};if(d)var
I=function(a){return a?0:1},u=b(j[19][28],I,t);else
var
u=d;if(u)m[1]=1;return b(j[19][13],H,t)}}}switch(e){case
0:f(0,0,c);break;case
1:f(0,1,c);break;default:var
d=a(g[79],c),n=d[2],p=d[1],r=function(c){var
b=a(o,1);return a(g[T],b)};f(b(j[17][14],r,p),0,n);m[1]=0}return[0,m[1],[0,i[1],k[1]]]}var
o=[0,aW,aw,bK,bL,bN,am,bQ,aY,function(n,s,f,e,m){var
i=ay(e);try{var
o=a(m,0)+1|0,p=aW[1]?aY(e,m,n,f):bO,q=p[1],t=p[2];if(1===n){var
k=an(e,f);switch(k[0]){case
0:var
g=0;break;case
1:var
g=3;break;case
2:var
g=1;break;case
3:var
g=2;break;case
4:var
w=K(am(0,k[1],k[2],e),0)[1],x=a(j[17][bC],w),g=[0,o,a(_[1][1][3],x),q];break;case
5:var
g=4;break;default:throw[0,Z,k[1]]}var
r=[1,g]}else{var
c=an(e,f);switch(c[0]){case
0:var
l=c[1],y=c[2],z=a(i,l),b=an(e,l);switch(b[0]){case
0:var
d=[5,b[1],b[2],y];break;case
1:var
d=[0,b[1],b[2]];break;case
2:var
d=[1,b[1],b[2]];break;case
3:var
d=[2,b[1],b[2]];break;case
4:var
d=[4,b[1],b[2]];break;case
5:var
d=[3,l];break;default:var
d=0}var
h=[4,z,d];break;case
1:var
h=0;break;case
2:var
A=c[1];if(c[3])throw[0,Z,a(i,f)];var
h=[0,A];break;case
3:var
B=c[1];if(c[3])throw[0,Z,a(i,f)];var
h=[1,B];break;case
4:var
h=[3,c[1]];break;case
5:var
h=[2,o,c[1],q];break;default:throw[0,Z,c[1]]}var
r=[0,h]}var
v=[0,[0,s,a(i,f),r,t]];return v}catch(a){a=u(a);if(a[1]===Z)return[1,a[2]];throw a}}];O(106,o,"Ground_plugin.Formula");var
M=[bF,bR,bt(0)];function
az(U,S){var
f=a(A[2],0),n=[0,0];function
q(c,a){var
d=n[1];function
e(d){var
e=d[1];return[0,e,b(t[55],[0,[0,c,a],0],d[2])]}n[1]=[0,[0,c,a],b(j[17][12],e,d)];return 0}function
r(c){var
d=a(g[aj],c);if(2===d[0]){var
e=d[1];try{var
f=r(b($[4][2],e,n[1]));return f}catch(a){a=u(a);if(a===H)return c;throw a}}return c}b(A[3],[0,U,S],f);try{for(;;){var
y=a(A[5],f),V=y[2],h=r(b(aA[24],aa[16],y[1])),i=r(b(aA[24],aa[16],V)),e=a(g[aj],h),c=a(g[aj],i);switch(e[0]){case
2:var
o=e[1];if(2===c[0]){var
w=c[1];if(1-(o===w?1:0))if(o<w)q(w,h);else
q(o,i)}else{var
v=b(t[55],n[1],i),_=a(t[44],v);if(a($[2][2],_))var
ab=a(g[T],o),Q=b(t[54],ab,v)?0:(q(o,v),1);else
var
Q=0;if(!Q)throw[0,M,h,i]}var
d=3;break;case
6:var
ac=e[3],ad=e[2];switch(c[0]){case
2:var
d=0;break;case
5:var
d=1;break;case
6:var
E=c[3],D=c[2],C=ac,B=ad,d=4;break;default:var
d=2}break;case
7:var
ag=e[3],ah=e[2];switch(c[0]){case
2:var
d=0;break;case
5:var
d=1;break;case
7:var
E=c[3],D=c[2],C=ag,B=ah,d=4;break;default:var
d=2}break;case
9:var
F=e[2],ai=e[1];switch(c[0]){case
2:var
d=0;break;case
5:var
d=1;break;case
9:var
G=c[2];b(A[3],[0,ai,c[1]],f);var
I=F.length-1;if(I!==G.length-1)throw[0,M,h,i];var
J=I-1|0,ak=0;if(!(J<0)){var
k=ak;for(;;){var
al=K(G,k)[k+1],am=[0,K(F,k)[k+1],al];b(A[3],am,f);var
an=k+1|0;if(J!==k){var
k=an;continue}break}}var
d=3;break;default:var
d=2}break;case
13:var
L=e[4],ao=e[3],ap=e[2];switch(c[0]){case
2:var
d=0;break;case
5:var
d=1;break;case
13:var
N=c[4],aq=c[3];b(A[3],[0,ap,c[2]],f);b(A[3],[0,ao,aq],f);var
O=L.length-1;if(O!==N.length-1)throw[0,M,h,i];var
P=O-1|0,ar=0;if(!(P<0)){var
l=ar;for(;;){var
as=K(N,l)[l+1],at=[0,K(L,l)[l+1],as];b(A[3],at,f);var
au=l+1|0;if(P!==l){var
l=au;continue}break}}var
d=3;break;default:var
d=2}break;default:var
d=0}switch(d){case
0:if(2===c[0]){var
z=c[1],s=b(t[55],n[1],h),Y=a(t[44],s);if(a($[2][2],Y)){var
Z=a(g[T],z);if(b(t[54],Z,s))var
x=1;else{q(z,s);var
m=2,x=0}}else
var
x=1;if(x)throw[0,M,h,i]}else
if(5===e[0]){var
X=[0,a(t[67],h),i];b(A[3],X,f);var
m=2}else
var
m=0;break;case
1:var
m=0;break;case
2:var
m=1;break;case
3:var
m=2;break;default:b(A[3],[0,B,D],f);var
ae=a(t[56],E),af=[0,a(t[56],C),ae];b(A[3],af,f);var
m=3}switch(m){case
0:if(5===c[0]){var
W=[0,h,a(t[67],i)];b(A[3],W,f);var
p=1}else
var
p=0;break;case
1:var
p=0;break;case
2:var
p=1;break;default:var
p=2}switch(p){case
0:if(1-b(g[135],h,i))throw[0,M,h,i];var
R=0;break;case
1:var
R=0;break;default:var
R=1}continue}}catch(a){a=u(a);if(a===A[1])return n[1];throw a}}function
bS(e,b){function
c(b){if(a(g[7],b))if(a(g[30],b)===e)return 0;function
f(a,d){var
b=c(d);return 0<=a?0<=b?a+b|0:a:b}var
d=h(g[138],f,-1,b);return 0<=d?d+1|0:-1}return c(b)}function
bT(e){var
c=[0,1],d=[0,0];function
f(e,h){var
i=a(g[aj],h);if(2===i[0]){var
j=i[1];try{var
m=e+b($[4][2],j,d[1])|0,n=a(g[ak],m);return n}catch(b){b=u(b);if(b===H){var
k=c[1];c[1]++;d[1]=[0,[0,j,k],d[1]];return a(g[ak],k+e|0)}throw b}}function
l(a){return a+1|0}return q(g[140],l,f,e,h)}var
h=f(0,e);return[0,c[1]-1|0,h]}function
bU(e,d,c,i){try{var
j=az(c,i),f=b($[4][2],e,j);if(a(g[7],f))var
h=[0,[1,d]];else
var
k=bS(e,c),h=[0,[0,bT(f),k]];return h}catch(a){a=u(a);if(a[1]===M)return 0;if(a===H)return[0,[1,d]];throw a}}function
aZ(e,d,c){function
f(b){return a(g[T],e+b|0)}var
h=b(j[17][48],d,f);return b(B[13],h,c)}var
ap=[0,M,az,bU,function(e,d){var
c=e[1],f=d[2],h=d[1],i=aZ(0,c,e[2]),k=aZ(c,h,f);try{var
l=az(i,k),m=function(b){var
d=b[1]<c?1:0,e=b[2];return d?d:a(g[7],e)},n=b(j[17][22],m,l);return n}catch(a){a=u(a);if(a[1]===M)return 0;throw a}}];O(bA,ap,"Ground_plugin.Unify");function
a0(a){if(0===a[0]){var
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
0:return au;case
1:return 80;case
2:return 70;case
3:return-20;case
4:return 50;default:return-10}}}var
d=a[1];if(typeof
d==="number")switch(d){case
0:return au;case
1:return 40;case
2:return-15;case
3:return-50;default:return au}return-29}var
bV=[0,function(b,a){var
c=a0(a[3]);return a0(b[3])-c|0}],aB=[0,g[bw]],bW=[0,function(c,a){var
e=a[2],f=c[2],d=b(ab[18][1],c[1],a[1]);if(0===d){var
g=function(c,a){var
d=w.caml_int_compare(c[1],a[1]),e=a[2],f=c[2];return 0===d?b(aB[1],f,e):d};return h(ac[5],g,f,e)}return d}],m=a(j[21][1],aB),ad=a(j[20][1],bW);function
aq(d,e,c){try{var
a=[0,e,b(m[22],d,c)],f=h(m[4],d,a,c);return f}catch(a){a=u(a);if(a===H)return h(m[4],d,[0,e,0],c);throw a}}function
a1(c,e,a){try{var
f=b(m[22],c,a),g=function(a){return 1-b(ab[5],a,e)},d=b(j[17][29],g,f),i=d?h(m[4],c,d,a):b(m[6],c,a);return i}catch(b){b=u(b);if(b===H)return a;throw b}}var
R=a(aC[2],bV);function
bX(a){return[0,a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8]-1|0]}function
bY(c,a){var
d=a[8],e=b(ad[4],c,a[7]);return[0,a[1],a[2],a[3],a[4],a[5],a[6],e,d]}function
bZ(a,c){var
d=b(ad[3],a,c[7]);if(d)var
e=d;else{var
f=a[2],i=a[1];if(f){var
g=f[1],j=g[1],h=function(a){var
c=a[2],k=a[1];if(c){var
d=c[1],l=d[1],e=b(ab[5],i,k);if(e){var
f=j<l?1:0;if(f)return b(ap[4],d,g);var
h=f}else
var
h=e;return h}return 0};return b(ad[17],h,c[7])}var
e=0}return e}function
aD(f,e,i,a,h){var
g=F(o[9],f,e,i,h,a[6]);if(0===g[0]){var
c=g[1];if(1===f){var
j=a[8],k=a[7],l=a[6],m=c[2],n=a[3],p=a[2];return[0,b(R[2],c,a[1]),p,n,m,0,l,k,j]}var
q=a[8],r=a[7],s=a[6],t=a[5],u=a[4],v=a[3],w=aq(c[2],e,a[2]);return[0,b(R[2],c,a[1]),w,v,u,t,s,r,q]}var
d=g[1];if(1===f)return[0,a[1],a[2],a[3],d,[0,d],a[6],a[7],a[8]];var
x=a[8],y=a[7],z=a[6],A=a[5],B=a[4],C=[0,d,a[3]],D=aq(d,e,a[2]);return[0,a[1],D,C,B,A,z,y,x]}function
b0(b,a){function
c(a,b){return a[1]===o[7]?b:aq(a[2],a[1],b)}var
d=a[8],e=a[7],f=a[6],g=a[5],i=a[4],k=a[3],l=h(j[17][16],c,b,a[2]);return[0,h(j[17][16],R[2],b,a[1]),l,k,i,g,f,e,d]}function
b1(d,c){var
e=b(m[22],d,c[2]);return a(j[17][3],e)}function
b2(f){var
b=f;for(;;){var
c=a(R[3],b[1]),d=a(R[4],b[1]);if(c[1]===o[7]){var
e=[0,d,b[2],b[3],b[4],b[5],b[6],b[7],b[8]];if(b[4]===c[2])return[0,c,e];var
b=e;continue}var
g=b[8],h=b[7],i=b[6],j=b[5],k=b[4],l=b[3];return[0,c,[0,d,a1(c[2],c[1],b[2]),l,k,j,i,h,g]]}}function
b3(d){var
b=[0,-1],e=ad[1];function
c(a){if(a)b[1]++;return b[1]}var
f=a(g[T],1);return[0,R[1],m[1],0,f,0,c,e,d]}function
b4(c){if(2===c[0]){var
d=c[1],e=function(a){return[3,[0,d,a+1|0]]},f=a(al[21],d);return b(j[17][48],f,e)}return[0,c,0]}var
b5=a(j[17][bA],b4);function
b6(e,d,c){var
f=a(b5,e);function
g(d,c){var
f=c[2],g=c[1];function
i(a){return q(aa[161],0,0,0,a)}var
e=h(s[24],i,f,d),a=e[1];return[0,aD(0,d,b(s[15],a,e[2]),g,a),a]}return h(j[17][16],g,f,[0,d,c])}function
b7(f,e,c){var
d=[0,e];function
g(g){var
e=a(aE[30],g[7]);switch(e[0]){case
0:case
2:case
3:var
f=e[1][1][1];try{var
h=a(ab[16],f),i=b(s[15],c,f);d[1]=aD(2,h,i,d[1],c);var
j=0;return j}catch(a){a=u(a);if(a===H)return 0;throw a}default:return 0}}function
h(d,c,a){return b(j[17][11],g,a)}function
i(d){try{var
c=a(aE[15],d),e=c}catch(c){c=u(c);if(c!==H)throw c;var
f=b(y[16],d,b8),g=b(y[16],b9,f),e=a(ae[7],g)}return b(aE[14][12],h,e)}b(j[17][11],i,f);return[0,d[1],c]}function
b_(c){function
d(e,d,c){var
f=aa[16],g=a(ax[2],0),h=F(b$[6],0,0,g,f,e),i=a(n[14],0),j=a(a2[23],h),k=a(n[3],ca),l=b(n[36],ar[42],d),m=a(n[3],cb),o=b(n[12],m,l),p=b(n[12],o,k),q=b(n[12],p,j),r=b(n[12],q,i);return b(n[12],r,c)}var
e=a(n[3],cc),f=a(n[7],0),g=h(m[11],d,c,f),i=a(n[14],0),j=a(n[3],cd),k=b(n[12],j,i),l=b(n[12],k,g),o=b(n[12],l,e);return b(n[24],0,o)}var
p=[0,aB,[0,m[1],m[2],m[3],m[4],m[5],m[6],m[7],m[8],m[9],m[10],m[11],m[12],m[13],m[14],m[15],m[16],m[17],m[18],m[19],m[20],m[21],m[22],m[23],m[24]],ad,aq,a1,R,bX,bY,bZ,aD,b0,b1,b2,b3,b6,b7,b_];O(124,p,"Ground_plugin.Sequent");function
v(i,g,f,q,c){a(ce[2],0);var
m=a(s[9],c),k=a(s[8],c);function
l(u,r,o){var
f=u,e=r,d=o;for(;;){if(0<f){if(e){var
m=e[2],g=e[1],i=a(_[2][1][1],g),v=a(s[7],c);if(!h(t[41],k,i,v)){var
w=b(t[42],k,i);if(!b(j[17][23],w,d)){var
x=l(f-1|0,m,[0,g,d]),y=a(_[2][1][3],g);return F(p[10],0,[0,i],y,x,c)}}var
f=f-1|0,e=m,d=[0,g,d];continue}var
z=a(n[3],cf);return h(ae[3],0,0,z)}return q}}var
d=l(i,m,0);if(g)var
r=a(s[7],c),e=F(p[10],1,o[7],r,d,c);else
var
e=d;return b(f,e,c)}function
cg(a){if(0===a[0])return a[1];throw[0,I,ch]}function
J(b){if(0===b[0]){var
c=a(i[74],[0,b[1],0]);return a(f[67][8],c)}return d[1]}function
ci(e,c){try{var
h=function(b){var
c=a(i[42],b);return a(f[67][8],c)},j=b(p[12],e,c),k=b(d[67],j,h);return k}catch(c){c=u(c);if(c===H){var
g=a(n[3],cj);return b(d[24],0,g)}throw c}}function
ck(m,l,e,k,c){var
o=0,q=1;function
r(a){return v(q,o,k,c,a)}try{var
t=[0,a(f[67][8],i[16]),0],w=[0,J(e),t],x=function(c){function
h(b){var
d=[0,a(g[L],[0,b,[0,c]]),0],e=a(i[U],d);return a(f[67][8],e)}return b(d[67],e,h)},y=b(p[12],m,c),z=[0,b(d[67],y,x),w],A=a(d[7],z),j=A}catch(c){c=u(c);if(c!==H)throw c;var
s=a(n[3],cl),j=b(d[24],0,s)}return h(d[33],j,r,l)}function
cm(e,c,b){var
g=1,j=0;function
k(a){return v(j,g,c,b,a)}var
l=a(f[67][8],i[121]);return h(d[33],l,k,e)}function
cn(g,e,c){var
h=1,j=0;function
k(a){return v(j,h,e,c,a)}var
l=a(d[22],k),m=[0,a(f[67][1],l)],n=b(i[111],0,m),o=a(f[67][8],n);return b(d[4],o,g)}function
co(g,e,c){var
j=1,k=1;function
l(a){return v(k,j,e,c,a)}var
m=a(d[22],l),n=a(f[67][8],i[17]),o=b(d[5],n,m),p=b(d[4],o,g),q=1,r=1;function
s(a){return v(r,q,e,c,a)}var
t=a(f[67][8],i[16]);return h(d[33],t,s,p)}function
cp(l,k,e,j,h,c){var
g=K(b(o[5],l,c),0)[1],m=0;function
n(a){return v(g,m,j,h,a)}var
p=a(f[67][8],i[16]),r=[0,b(d[26],g,p),0],s=[0,J(e),r],t=b(d[70][58],e,i[99]),u=[0,a(f[67][8],t),s],w=a(d[7],u);return q(d[33],w,n,k,c)}function
cq(l,k,e,h,g,c){var
m=b(o[5],l,c);function
n(c){var
j=0,k=0,l=[0,function(a){return v(c,k,h,g,a)},j],m=a(f[67][8],i[16]),n=[0,b(d[26],c,m),l],o=[0,J(e),n];return a(d[7],o)}var
p=b(j[19][15],n,m),r=b(d[70][58],e,i[99]),s=a(f[67][8],r);return q(d[35],s,p,k,c)}function
cr(c){var
e=b(d[70][58],c,i[99]);return a(f[67][8],e)}function
cs(c,l,r,k,p,n,h){var
s=c[2],u=c[1],m=q(o[6],0,c,l,h),e=m.length-1,w=a(j[19][12],l),x=0;function
y(a){return v(e,x,p,n,a)}var
z=a(f[67][8],i[16]),A=[0,b(d[26],e,z),0],C=[0,J(k),A];function
D(q){function
c(d){var
e=K(m,d)[d+1],c=a(j[17][1],e),f=[0,a(g[127],[0,[0,u,d+1|0],s]),w],h=a(g[L],f);function
i(b){return a(g[ak],c-b|0)}var
k=b(j[19][2],c,i),l=[0,b(B[8],c,h),k],n=[0,a(g[L],l)],o=[0,b(B[8],c,q),n],p=a(g[L],o);return b(t[23],p,e)}var
d=b(j[17][48],e,c),h=a(i[U],d);return a(f[67][8],h)}var
E=[0,b(d[67],k,D),C],F=a(d[7],E);return q(d[33],F,y,r,h)}function
ct(k,j,m,l,c,h,e){var
n=[0,0,k,b(B[8],1,j)],o=a(g[117],n),p=0,q=0,r=0,s=1,t=2;function
u(a){return v(t,s,h,e,a)}var
w=[0,a(d[22],u),r],x=[0,a(f[67][8],i[17]),w],y=[0,a(f[67][8],i[17]),x],z=[0,J(c),y];function
A(l){var
c=a(g[ak],2),d=[0,0,b(B[8],1,k),c],e=[0,l,[0,a(g[by],d)]],h=[0,0,j,a(g[L],e)],m=[0,a(g[by],h),0],n=a(i[U],m);return a(f[67][8],n)}var
C=[0,b(d[67],c,A),z],D=[0,a(d[7],C),q];function
E(b){var
c=a(i[42],b);return a(f[67][8],c)}var
F=[0,b(d[67],c,E),D],G=a(i[ai],o),H=a(f[67][8],G),I=[0,b(d[11],H,F),p],K=0,M=0,N=1,O=[0,function(a){return v(N,M,h,e,a)},K],P=[0,J(c),O],Q=[0,a(f[67][8],i[17]),P],R=[0,a(d[7],Q),I],S=a(i[ai],m),T=a(f[67][8],S),V=b(d[11],T,R);return b(d[4],V,l)}function
cu(g,e,c){if(o[1][1])var
k=a(n[3],cv),j=b(d[24],0,k);else
var
j=g;var
l=1,m=0;function
p(a){return v(m,l,e,c,a)}var
q=a(d[22],p),r=a(f[67][8],i[17]),s=b(d[5],r,q),t=b(d[4],s,g),u=1,w=0;function
x(a){return v(w,u,e,c,a)}var
y=a(f[67][8],i[16]),z=h(d[33],y,x,t);return b(d[4],z,j)}function
cw(l,k,e,j,h,c){var
g=K(b(o[5],l,c),0)[1],m=0,n=0,p=g-1|0,r=[0,function(a){return v(p,n,j,h,a)},m],s=a(f[67][8],i[16]),t=[0,b(d[26],g,s),r],u=[0,J(e),t],w=a(d[7],u),x=b(d[70][58],e,i[99]),y=a(f[67][8],x);return q(d[33],y,w,k,c)}function
cx(l,k,j,e,c){var
m=0,n=a(p[7],c),o=1,q=0;function
r(a){return v(q,o,e,n,a)}var
t=[0,a(d[22],r),m],u=0,w=a(p[7],c),x=0,y=1;function
z(a){return v(y,x,e,w,a)}var
A=[0,a(d[22],z),u],B=[0,a(f[67][8],i[16]),A],C=[0,J(j),B];function
D(j,c){var
e=b(s[11],c,1),k=[0,j,[0,a(g[aR],e)]],l=a(g[L],k),m=a(i[74],[0,e,0]),n=a(f[67][8],m),o=a(i[U],[0,l,0]),p=a(f[67][8],o);return h(d[5],p,n,c)}var
E=[0,b(d[67],j,D),C],F=[0,a(f[67][8],i[16]),E],G=[0,a(d[7],F),t],H=a(i[ai],l),I=a(f[67][8],H),K=b(d[11],I,G);return b(d[4],K,k)}function
a3(a){return h(cA[4],cz,cy,a)}var
N=[aQ,function(e){var
b=a3(cB),c=[0,[0,0,[1,a(g[41],b)[1]]],0],d=a3(cC);return[0,[0,0,[1,a(g[41],d)[1]]],c]}];function
cD(c){if(c){var
d=bu(N),g=[0,c[1],1],h=bB===d?N[1]:aQ===d?a(a4[2],N):N,j=b(i[68],h,g);return a(f[67][8],j)}var
e=bu(N),k=bB===e?N[1]:aQ===e?a(a4[2],N):N,l=a(i[67],k);return a(f[67][8],l)}var
r=[0,v,cg,J,ci,ck,cm,cn,co,cp,cq,cr,cs,ct,cu,cw,cx,a(d[56],cD)];O(131,r,"Ground_plugin.Rules");function
cE(c,a){if(0===c[0]){var
d=c[1],e=d[1],g=c[2],h=d[2];if(0===a[0]){var
f=a[1],i=a[2],j=f[2],k=f[1],l=p[1][1],m=function(b,a){return b-a|0},n=function(b,a){return b-a|0},q=b(o[3],n,m);return aL(b(o[4],q,l),k,e,g,i,h,j)}return 0===e?1:-1}var
r=c[1];return 0===a[0]?0===a[1][1]?-1:1:b(p[1][1],r,a[1])}function
cF(c,a){return c===a?0:c===o[7]?1:a===o[7]?-1:b(ab[18][1],c,a)}var
cG=[0,function(c,a){var
d=a[2],e=a[1],f=c[2],g=c[1];return q(b(o[3],cE,cF),e,g,d,f)}],as=a(j[20][1],cG);function
a5(c,l){var
d=[0,as[1]];function
e(f){var
i=f[3];if(0===i[0]){var
c=i[1];if(typeof
c==="number")var
m=1;else
if(2===c[0])var
x=c[3],k=c[2],w=c[1],g=1,m=0;else
var
m=1;if(m)var
g=0}else{var
e=i[1];if(typeof
e==="number")var
g=0;else
var
x=e[3],k=e[2],w=e[1],g=1}if(g){var
y=f[4],r=[0,1],s=[0,x],D=f[1],t=function(c,a){function
e(f,e){var
a=q(ap[3],w,k,f,e);if(a){var
c=a[1];return 0===c[0]?(r[1]=0,d[1]=b(as[4],[0,c,D],d[1]),0):(s[1]=1,0)}return 0}var
f=c[1];function
g(c){var
d=a[2];function
f(a){return e(c,a)}return b(j[17][11],f,d)}b(j[17][11],g,f);var
h=c[2];function
i(c){var
d=a[1];function
f(a){return e(c,a)}return b(j[17][11],f,d)}return b(j[17][11],i,h)},A=l[1],B=function(a){return t(y,a[4])};b(p[6][5],B,A);var
o=l[5],z=o?[0,o[1],0]:0;t(y,[0,z,l[3]]);var
u=r[1],v=u?s[1]:u,E=v?(d[1]=b(as[4],[0,[1,k],f[1]],d[1]),0):v;return E}var
C=a(n[3],cH);return h(ae[3],0,0,C)}b(j[17][11],e,c);return a(as[21],d[1])}function
a6(b){try{var
f=a(p[13],b),g=f[1],c=g[3],k=f[2];if(0===c[0]){var
h=c[1];if(typeof
h==="number")var
e=1;else
if(2===h[0])var
d=1,e=0;else
var
e=1;if(e)var
d=0}else
var
d=typeof
c[1]==="number"?0:1;if(d)var
j=a6(k),i=[0,[0,g,j[1]],j[2]];else
var
i=[0,0,b];return i}catch(a){a=u(a);if(a===aC[1])return[0,0,b];throw a}}var
a7=a(x[1][6],cI);function
cJ(y,x,c,d,w){var
n=a(s[8],c),p=a(a8[2],c);if(y===o[7])var
q=a7;else
var
G=b(s[15],c,x),H=h(aA[25],n,p,G),v=a(g[34],H)[1],I=v?v[1]:a7,q=I;function
z(b){return a(g[ak],d-b|0)}var
A=b(j[17][48],d,z),m=d,l=0,k=n,f=p,e=0,C=b(B[13],A,w);for(;;){if(0===m)return[0,f,e,C];var
r=h(i[14],l,q,c),D=a(a9[21][2],f),t=bv(cK[7],k,D,0,0,0,0,aa[bC]),E=t[1][1],u=[0,[0,r],E],F=a(a9[6],t[2]),m=m-1|0,l=[0,r,l],k=b(cL[20],u,k),f=F,e=[0,u,e];continue}}var
V=[0,a6,a5,function(m,k,l,e,c){var
v=a5(m,e);function
w(j){if(j[2]===o[7])var
c=j[1],k=function(h,j){if(0===c[0]){var
e=c[1];if(0===e[1]){var
k=e[2],l=a(p[7],j),m=[0,q(r[1],0,1,h,l),0],o=a(d[20],m),t=a(i[bI],[0,[0,k,0]]),u=a(f[67][8],t);return b(d[5],u,o)}var
v=a(n[3],cQ);return b(d[24],0,v)}var
w=c[1],x=a(f[67][8],i[41]),y=[0,a(d[21],x),0],z=a(p[7],j),A=[0,q(r[1],0,1,h,z),0],B=[0,a(d[20],A),0],C=[0,function(c){var
d=b(s[11],c,1),e=[0,[0,a(g[aR],d),0]],h=a(i[bI],e);return b(f[67][8],h,c)},B],D=[0,a(f[67][8],i[17]),C],E=[0,a(d[7],D),y],F=a(i[ai],w),G=a(f[67][8],F);return b(d[11],G,E)};else
var
k=function(w,k){var
c=j[2],l=j[1];if(0===l[0]){var
e=l[1],m=e[2],o=e[1];if(b(p[9],[0,c,[0,e]],k)){var
x=a(n[3],cM);return b(d[24],0,x)}if(0<o)var
y=function(k,e){var
j=cJ(c,k,e,o,m),p=j[2],r=j[1],v=a(g[L],[0,k,[0,j[3]]]),l=b(t[23],v,p);try{var
A=a(s[8],e),B=q(cO[2],0,A,r,l),n=B}catch(b){b=u(b);if(!a(ae[21],b))throw b;var
n=a(ae[7],cN)}var
w=n[1],x=a(i[U],[0,l,0]),y=a(f[67][8],x),z=a(a8[11],w);return h(d[5],z,y,e)},v=b(d[67],c,y);else
var
E=function(b){var
c=[0,a(g[L],[0,b,[0,m]]),0],d=a(i[U],c);return a(f[67][8],d)},v=b(d[67],c,E);var
z=b(p[8],[0,c,[0,e]],k),A=a(p[7],z),B=[0,q(r[1],1,0,w,A),0],C=[0,a(d[20],B),0],D=[0,v,[0,a(f[67][8],i[17]),C]];return a(d[7],D)}var
F=l[1];if(b(p[9],[0,c,0],k)){var
G=a(n[3],cP);return b(d[24],0,G)}var
H=a(f[67][8],i[41]),I=[0,a(d[21],H),0],J=b(p[8],[0,c,0],k),K=a(p[7],J),M=[0,q(r[1],1,0,w,K),0],N=[0,a(d[20],M),0],O=[0,a(f[67][8],i[17]),N];function
P(d,c){var
e=b(s[11],c,1),h=[0,d,[0,a(g[aR],e)]],j=[0,a(g[L],h),0],k=a(i[U],j);return b(f[67][8],k,c)}var
Q=[0,b(d[67],c,P),O],R=[0,a(f[67][8],i[17]),Q],S=[0,a(d[7],R),I],T=a(i[ai],F),V=a(f[67][8],T);return b(d[11],V,S)};return k(l,e)}var
x=b(j[17][12],w,v),y=a(d[19],x);return h(d[4],y,k,c)}];O(bw,V,"Ground_plugin.Instances");var
aF=[0,function(X,t,e){var
c=[0,x[18][1]];function
i(d){try{var
e=a(a_[23],d),f=a(g[41],e)[1];c[1]=b(x[18][7],f,c[1]);var
h=0;return h}catch(a){a=u(a);if(a===g[28])return 0;throw a}}var
j=a(a_[26],0);b(a$[11],i,j);var
l=a(x[18][12],c[1]),m=b(G[8][13],G[14],[0,x[1][12][2],l]);o[2][1]=m;function
v(z,j,E){if(w.caml_equal(a(k[12][8],0),cR)){var
Y=a(ar[66],E);b(ba[10],0,Y)}try{var
H=a(p[13],j),g=H[2],i=H[1],e=function(a){return b(p[11],z,a)},_=0,f=function(a,b){return v(_,a,b)},c=function(a){return v([0,i,z],g,a)},A=i[3];if(0===A[0]){var
m=A[1];if(typeof
m==="number")var
s=a(r[11],i[1]);else
switch(m[0]){case
0:var
$=m[1],aa=e(g),s=F(r[9],$,c,i[1],f,aa);break;case
1:var
ab=m[1],ac=e(g),s=F(r[10],ab,c,i[1],f,ac);break;case
2:var
J=a(V[1],j),K=J[1],ad=J[2],ae=b(y[22],K,z),L=function(a){return v(ae,ad,a)};if(o[1][1])if(0<j[8])var
af=e(j),M=q(V[3],K,L,f,af),B=1;else
var
B=0;else
var
B=0;if(!B)var
M=L;var
s=M;break;case
3:var
ag=m[1];if(o[1][1])var
ah=e(g),N=F(r[15],ag,c,i[1],f,ah);else
var
N=c;var
s=N;break;default:var
l=m[2],ai=m[1];if(typeof
l==="number")var
x=c;else
switch(l[0]){case
3:var
an=l[1];if(0<j[8])if(o[1][1])var
ao=e(g),O=F(r[16],an,c,i[1],f,ao),C=1;else
var
C=0;else
var
C=0;if(!C)var
O=c;var
x=O;break;case
4:var
ap=l[2],aq=l[1];if(o[1][1])var
as=e(g),P=aL(r[12],aq,ap,c,i[1],f,as);else
var
P=c;var
x=P;break;case
5:var
at=l[3],au=l[2],av=l[1],aw=e(g),x=bv(r[13],av,au,at,c,i[1],f,aw);break;default:var
ak=l[2],al=l[1],am=e(g),x=aL(r[12],al,ak,c,i[1],f,am)}var
aj=e(g),s=F(r[5],ai,x,i[1],f,aj)}var
I=s}else{var
Q=A[1];if(typeof
Q==="number")switch(Q){case
0:var
ax=e(g),t=h(r[8],c,f,ax);break;case
1:var
ay=e(g),t=h(r[6],c,f,ay);break;case
2:var
az=e(g),t=h(r[7],c,f,az);break;case
3:var
t=c;break;default:if(o[1][1])var
aA=a(n[3],cS),R=b(d[24],0,aA);else
var
R=c;var
aB=e(g),t=h(r[14],R,f,aB)}else{var
S=a(V[1],j),T=S[1],aD=S[2],aE=b(y[22],T,z),U=function(a){return v(aE,aD,a)};if(o[1][1])if(0<j[8])var
aF=e(j),W=q(V[3],T,U,f,aF),D=1;else
var
D=0;else
var
D=0;if(!D)var
W=U;var
t=W}var
I=t}var
G=I}catch(a){a=u(a);if(a!==aC[1])throw a;var
G=X}var
Z=b(r[4],j[4],j);return h(d[4],Z,G,E)}var
f=a(t,e),z=f[2],A=f[1],B=0;function
C(a,b){return v(B,a,b)}var
D=a(s[9],e),E=a(a$[1],D);return F(r[1],E,1,C,A,z)}];O(142,aF,"Ground_plugin.Ground");a(aG[12],cT);var
af=[0,3];function
cU(a){return a?(af[1]=b(y[5],a[1],0),0):(af[1]=3,0)}var
cX=[0,1,0,cW,cV,function(a){return[0,af[1]]},cU];b(bb[3],0,cX);var
W=[0,au];function
cY(a){return a?(W[1]=b(y[5],a[1],0),0):(W[1]=0,0)}var
c1=[0,1,0,c0,cZ,function(a){return[0,W[1]]},cY];b(bb[3],0,c1);var
c2=[0,bd,0],c3=[0,function(b,a){return q(bc[14],0,0,0,0)}];h(k[6][9],0,bd,c3);var
be=[31,C[4],c2,0],aH=b(k[14][1],[0,be],c4),bf=aH[3],aI=aH[2],bg=aH[1],c5=0,c7=[0,[0,0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(l[4],k[1][1]),f=b(l[8],e,d);return function(e){var
c=a(k[8][3],f),d=a(bh[10][2],0);return b(bg,a(bh[6],d),c)}}return a(y[2],c6)}],c5];function
c8(b,a){return h(bi[1],a[1],[0,c9,b],a[2])}b(X[80],c8,c7);var
c_=0,da=[0,function(b){if(b)if(!b[2])return function(a){return at[6]};return a(y[2],c$)},c_];function
db(c,a){return b(at[3],[0,dc,c],a)}b(X[80],db,da);var
dd=[6,a(D[12],k[1][1])],de=a(l[4],k[1][1]),di=[0,[0,dh,[0,dg,[0,df,[0,[1,C[4],de,dd],0]]]],0];function
dj(b,a){return h(bj[1],[0,dk,b],0,a)}b(X[80],dj,di);var
dl=0,dp=[0,[0,0,function(c){return c?a(y[2],dm):function(f){var
c=a(bf,0),d=a(n[3],dn),e=b(n[12],d,c);return b(ba[6],0,e)}}],dl];function
dq(b,a){return h(bi[1],a[1],[0,dr,b],a[2])}b(X[80],dq,dp);var
ds=0,du=[0,function(b){return b?a(y[2],dt):function(a){return at[5]}},ds];function
dv(c,a){return b(at[3],[0,dw,c],a)}b(X[80],dv,du);function
dy(b,a){return h(bj[1],[0,dz,b],0,a)}b(X[80],dy,dx);var
dB=a(n[3],dA),dC=b(d[24],0,dB);function
Y(i,b,g,e,d){var
c=o[1][1];try{o[1][1]=i;var
j=b?b[1]:a(aI,0)[2],k=function(c){var
d=a(p[14],af[1]),b=h(p[15],g,d,c);return h(p[16],e,b[1],b[2])},l=a(f[67][8],j),m=h(aF[1],l,k,d);o[1][1]=c;return m}catch(a){a=u(a);o[1][1]=c;throw a}}function
bk(d,c,b){return a(k[2][23],dD[41])}function
bl(f,e,d){function
b(b){return a(ar[42],b[2])}var
c=a(a2[6],b);return a(k[2][23],c)}function
bm(d,c,b){return a(k[2][23],ar[42])}function
dE(b){return a(n[22],dF)}var
bn=q(dI[2],dH,dG,0,dE),z=a(l[2],dJ);function
dK(c,d){var
e=a(l[17],E[22]),f=a(l[4],e),g=b(l[7],f,d),h=b(k[8][10],c,g),i=a(l[17],E[22]),j=a(l[5],i);return[0,c,b(l[8],j,h)]}b(bo[7],z,dK);function
dL(d,c){var
e=a(l[17],E[22]),f=a(l[5],e),g=b(l[7],f,c),h=b(k[5][2],d,g),i=a(l[17],E[22]),j=a(l[5],i);return b(l[8],j,h)}b(bo[8],z,dL);function
dM(d,c){var
e=a(l[17],E[22]),f=a(l[5],e),g=b(l[7],f,c);return b(k[12][9],d,g)}b(aJ[6],z,dM);var
dN=a(l[17],E[22]),dO=a(l[6],dN),dP=[0,a(aJ[2],dO)];b(aJ[3],z,dP);var
dQ=a(l[4],z),aK=h(D[13],D[9],dR,dQ),dS=0,dT=0;function
dU(a,c,b){return[0,a,0]}var
dV=[6,D[14][16]],dX=[0,[0,[0,[0,0,[0,a(ag[12],dW)]],dV],dU],dT];function
dY(b,e,a,d,c){return[0,a,b]}var
d0=[0,a(ag[12],dZ)],d1=[2,[6,D[14][16]],d0],d3=[0,a(ag[12],d2)],d4=[6,D[14][16]],d6=[0,[0,[0,[0,[0,[0,0,[0,a(ag[12],d5)]],d4],d3],d1],dY],dX];function
d7(d,c,a,f,e){b(bn,0,0);return[0,a,[0,c,d]]}var
d8=[3,[6,D[14][16]]],d9=[6,D[14][16]],d_=[6,D[14][16]],ea=[0,[0,[0,[0,[0,[0,0,[0,a(ag[12],d$)]],d_],d9],d8],d7],d6],eb=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],ea]],dS]];h(D[22],aK,0,eb);q(k[2][1],z,bk,bl,bm);var
ec=[0,aK,0];function
ed(c){var
d=c[2],e=a(l[4],z);return[0,b(l[7],e,d)]}h(k[9][5],ee,ed,ec);var
ef=0,eh=[0,function(c){if(c){var
d=c[2];if(d){var
e=d[2];if(e)if(!e[2]){var
g=e[1],h=d[1],i=c[1],j=a(l[18],k[1][1]),m=a(l[6],j),n=b(k[12][2][7],m,i),o=a(l[6],z),p=b(k[12][2][7],o,h),q=a(l[17],E[21]),r=a(l[6],q),s=b(k[12][2][7],r,g);return function(c){var
d=a(k[12][19],c),e=b(ac[15],d,n),g=1;function
h(a){return Y(g,e,p,s,a)}return a(f[67][1],h)}}}}return a(y[2],eg)},ef],ej=[0,function(c){if(c){var
d=c[2];if(d)if(!d[2]){var
e=d[1],g=c[1],h=a(l[18],k[1][1]),i=a(l[6],h),j=b(k[12][2][7],i,g),m=a(l[17],E[21]),n=a(l[6],m),o=b(k[12][2][7],n,e);return function(c){var
e=a(k[12][19],c),d=0,g=b(ac[15],e,j),h=1;function
i(a){return Y(h,g,d,o,a)}return a(f[67][1],i)}}}return a(y[2],ei)},eh],el=[0,function(c){if(c){var
d=c[2];if(d)if(!d[2]){var
e=d[1],g=c[1],h=a(l[18],k[1][1]),i=a(l[6],h),j=b(k[12][2][7],i,g),m=a(l[6],z),n=b(k[12][2][7],m,e);return function(c){var
e=a(k[12][19],c),d=0,g=b(ac[15],e,j),h=1;function
i(a){return Y(h,g,n,d,a)}return a(f[67][1],i)}}}return a(y[2],ek)},ej],em=a(bp[12],el);h(k[6][9],0,[0,S,en],em);function
eo(B){var
j=a(x[1][7],ep),b=E[21],g=0,i=0;if(0===b[0]){var
l=[0,er,[0,[1,C[4],[0,[5,[0,b[1]]]],j],i]],m=a(x[1][7],es);if(0===z[0]){var
n=[0,[1,C[4],[5,[0,z[1]]],m],l],o=a(x[1][7],eu),c=k[1][1];if(0===c[0]){var
p=[0,[0,ew,[0,[1,C[4],[4,[5,[0,c[1]]]],o],n]],g],r=a(x[1][7],ex),d=E[21],q=0;if(0===d[0]){var
s=[0,ez,[0,[1,C[4],[0,[5,[0,d[1]]]],r],q]],t=a(x[1][7],eA),e=k[1][1];if(0===e[0]){var
u=[0,[0,eC,[0,[1,C[4],[4,[5,[0,e[1]]]],t],s]],p],v=0,w=a(x[1][7],eD);if(0===z[0]){var
y=[0,[1,C[4],[5,[0,z[1]]],w],v],A=a(x[1][7],eF),f=k[1][1];if(0===f[0])return h(k[9][4],[0,S,eI],0,[0,[0,eH,[0,[1,C[4],[4,[5,[0,f[1]]]],A],y]],u]);throw[0,I,eG]}throw[0,I,eE]}throw[0,I,eB]}throw[0,I,ey]}throw[0,I,ev]}throw[0,I,et]}throw[0,I,eq]}b(aG[19],eo,S);var
eJ=0,eL=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(l[18],k[1][1]),g=a(l[6],e),h=b(k[12][2][7],g,d);return function(c){var
g=a(k[12][19],c),d=0,e=0,i=b(ac[15],g,h),j=0;function
l(a){return Y(j,i,e,d,a)}return a(f[67][1],l)}}return a(y[2],eK)},eJ],eM=a(bp[12],eL);h(k[6][9],0,[0,S,eN],eM);function
eO(f){var
e=a(x[1][7],eP),b=k[1][1],c=0,d=0;if(0===b[0])return h(k[9][4],[0,S,eS],0,[0,[0,eR,[0,[1,C[4],[4,[5,[0,b[1]]]],e],d]],c]);throw[0,I,eQ]}b(aG[19],eO,S);function
eT(q){var
g=b(bq[3][4],W[1],0),i=a(aI,0)[2],c=0,e=0,j=[0,b(d[70][3],i,g)],k=1;function
l(a){return Y(k,j,e,c,a)}var
m=a(f[67][1],l),n=b(bq[3][4],W[1],0),o=h(bc[18],0,0,0),p=b(d[70][12],o,n);return b(d[70][12],p,m)}var
eU=a(f[13],0),br=b(f[68][1],eU,eT);a(eV[3][3],br);var
bs=[0,S,af,W,be,bg,aI,bf,dC,Y,bk,bl,bm,bn,z,aK,br];O(163,bs,"Ground_plugin.G_ground");O(164,[0,o,ap,p,r,V,aF,bs],"Ground_plugin");return});
