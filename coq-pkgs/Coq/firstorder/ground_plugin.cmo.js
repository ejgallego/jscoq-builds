(function(e4){"use strict";var
bF=142,bH=131,aZ="Firstorder",ai=140,ak=145,al=112,aY="$l",bC="already done",aW="firstorder_using",bP=",",bE=250,bG=105,ay="$t",aS="gintuition",U=148,G=124,aV=246,bO=115,P="Extension: cannot occur",ah=113,bD=122,bN="with",bB="Depth",aj="firstorder",aX="ground_plugin",aU="Firstorder_Print_Solver",ax=100,bM="Solver",Q="plugins/firstorder/g_ground.ml4",bL=248,aR="Firstorder_Set_Solver",aT="using",bK="-----",T=114,bJ=121,bI="reversible in 1st order mode",v=e4.jsoo_runtime,L=v.caml_check_bound,by=v.caml_fresh_oo_id,c=v.caml_new_string,bz=v.caml_obj_tag,O=v.caml_register_global,t=v.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):v.caml_call_gen(a,[b])}function
b(a,b,c){return a.length==2?a(b,c):v.caml_call_gen(a,[b,c])}function
i(a,b,c,d){return a.length==3?a(b,c,d):v.caml_call_gen(a,[b,c,d])}function
p(a,b,c,d,e){return a.length==4?a(b,c,d,e):v.caml_call_gen(a,[b,c,d,e])}function
F(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):v.caml_call_gen(a,[b,c,d,e,f])}function
aQ(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):v.caml_call_gen(a,[b,c,d,e,f,g])}function
bA(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):v.caml_call_gen(a,[b,c,d,e,f,g,h])}var
e=v.caml_get_global_data(),S=c(aX),bo=[0,c(aX),c("auto_with")],f=e.Term,j=e.Util,Z=e.Context,C=e.Vars,w=e.Termops,an=e.Hipattern,az=e.Global,am=e.Inductiveops,H=e.CClosure,r=e.Tacmach,x=e.Names,$=e.Int,J=e.Not_found,B=e.Queue,aa=e.Evd,aC=e.Reductionops,I=e.Assert_failure,m=e.Pp,a4=e.Ppconstr,ar=e.Printer,aE=e.Hints,y=e.Pervasives,ad=e.CErrors,ac=e.Globnames,ab=e.Option,aF=e.Heap,a7=e.CamlinternalLazy,h=e.Tactics,g=e.Proofview,d=e.Tacticals,a_=e.Refiner,a9=e.Sigma,z=e.Tacinterp,bd=e.Feedback,bc=e.List,be=e.Classops,bl=e.Cc_plugin,bm=e.Auto,s=e.Constrarg,E=e.Loc,aK=e.Tacentries,k=e.Genarg,av=e.Stdarg,bg=e.Tacintern,aw=e.Pptactic,bk=e.Egramml,au=e.Vernac_classifier,bf=e.Vernacinterp,bh=e.Locality,aJ=e.Tacenv,D=e.Pcoq,aL=e.Mltop,bi=e.Goptions,aM=e.Geninterp,bj=e.Genintern,W=e.CList,af=e.CLexer,bn=e.Array,a0=[0,1],aA=[0,H[14]],bS=c("Formula.Is_atom"),bU=[0,0,[0,0,0]],bV=c("_"),bX=c("Unify.UFAIL"),cg=c(" : "),ch=c("| "),ci=c(bK),cj=c(bK),cd=c(" : No such Hint database"),ce=c("Firstorder: "),cH=c("iff"),cI=c("not"),cF=[0,c("Init"),[0,c("Logic"),0]],cG=c("User"),cC=c(bI),cs=c("No link"),cq=c("No axiom link"),co=[0,c("plugins/firstorder/rules.ml"),52,7],cm=c("Not the expected number of hyps"),cW=c("not implemented ... yet"),cU=c("Untypable instance, maybe higher-order non-prenex quantification"),cT=c(bC),cV=c(bC),cQ=c("can't happen"),cR=c("x"),cX=[0,0],cY=c(bI),eZ=[0,c(Q),1,0],eY=c(ay),e0=[0,c(aS)],e1=c(aS),eT=c(P),eP=[0,c(Q),1,0],eN=[0,c(Q),1,0],eK=[0,c(Q),1,0],eH=[0,c(Q),1,0],eE=[0,c(Q),1,0],eC=[0,c(Q),1,0],ez=[0,c(Q),1,0],ey=c("$l'"),eA=[0,c(bN)],eB=c(aY),eD=c(ay),eF=[0,c(aj)],eG=c(aY),eI=[0,c(bN)],eJ=c(ay),eL=[0,c(aj)],eM=c(aY),eO=c(ay),eQ=[0,c(aj)],eR=c(aj),et=c(P),er=c(P),ep=c(P),dP=c('Deprecated syntax; use "," as separator'),dK=c(aU),dH=c(aU),dE=c(P),dC=c(aU),dz=c("Firstorder solver tactic is "),dy=c(P),dw=c(aR),dn=c(aR),dk=c(P),di=c(aR),df=c(P),c4=c(aX),c6=[0,c(aZ),[0,c(bB),0]],c7=c("Firstorder Depth"),c_=[0,c("Congruence"),[0,c(bB),0]],c$=c("Congruence Depth"),dd=c("Firstorder default solver"),dr=[0,c(bM)],ds=[0,c(aZ)],dt=[0,c("Set")],dI=[0,[0,[0,c("Print")],[0,[0,c(aZ)],[0,[0,c(bM)],0]]],0],dL=c("GTauto failed"),dQ=c("deprecated"),dR=c("firstorder-deprecated-syntax"),dS=c(aW),d0=c(aW),d5=c(aT),d8=c(bP),d$=c(bP),ec=c(aT),ei=c(aT),en=c(aW),ew=c(aj),eW=c(aS),b1=e.Constrextern,ck=e.Coqlib,cl=e.Control,cK=e.Typing,cL=e.Evarutil,cM=e.Environ,c0=e.Tacsubst,c1=e.Libnames,cZ=e.Tactic_option,c2=e.Decl_mode_plugin,c3=e.CWarnings;function
bQ(h,g,f,e,d,c){var
a=b(h,f,e);return 0===a?b(g,d,c):a}function
bR(j,i,h,g,f,e,d,c){var
a=p(j,h,g,f,e);return 0===a?b(i,d,c):a}var
_=[bL,bS,by(0)];function
a1(h,g){var
b=h,c=g;for(;;){var
d=a(f[ai],c);if(6===d[0]){var
e=d[3];if(0<b){var
b=b-1|0,c=e;continue}return 1+a1(0,e)|0}return 0}}function
bT(c,d){var
e=a(az[26],c[1])[1][6],f=a(r[8],d),g=b(am[4],f,c);function
h(a){return a1(e,a)}return b(j[19][15],h,g)}function
ao(g,e,d,c){var
h=a(r[8],c),i=b(am[4],h,e);function
k(c){var
e=b(f[76],c,d),h=b(f[85],g,e)[2];return a(f[83],h)[1]}return b(j[19][15],k,i)}function
aB(c){var
d=a(r[8],c),e=i(H[42],0,aA[1],d);return function(c){var
d=a(H[36],c);return b(H[46],e,d)}}function
ap(m,D){var
E=aB(m),A=a(r[8],m),B=i(H[42],0,aA[1],A),C=a(H[36],D),c=b(H[47],B,C),n=a(an[23],c);if(n){var
o=n[1],F=o[1];return[0,F,a(w[56],o[2])]}var
p=a(an[21],c);if(p){var
q=p[1];return[5,q[2],q[3]]}var
s=a(an[27],c);if(s){var
e=s[1],g=e[2],G=e[3],t=a(f[43],e[1]),h=t[2],d=t[1],u=a(az[26],d),k=u[2],v=u[1],x=k[4].length-1;if(0===x)return[1,[0,d,h],g];var
I=0<G?1:0,J=function(b){var
c=v[6];return a(w[71],b)===c?1:0},l=b(j[19][28],J,k[9]);if(!a(am[19],[0,d,v,k])){var
L=I?l?1:0:1;if(L)return 1===x?[2,[0,d,h],g,l]:[3,[0,d,h],g,l]}return[6,c]}var
y=a(an[29],c);if(y){var
z=y[1],K=z[2];return[4,a(f[43],z[1]),K]}return[6,a(E,c)]}var
bW=[0,a(x[1][5],bV)];function
a2(l,o,e,c){var
m=[0,0],h=[0,0],k=[0,0],y=aB(l);function
g(A,d,z){var
e=A,n=z;for(;;){var
c=ap(l,n);switch(c[0]){case
0:var
B=c[2];g(e,1-d,c[1]);var
n=B;continue;case
1:var
r=1-d,D=r?(m[1]=1,0):r;return D;case
4:var
v=c[2],J=c[1],K=a(o,1),M=a(f[T],K),N=L(ao(1,J,v,l),0)[1],O=function(f,i,c){var
h=a(Z[1][1][3],c);return g([0,M,e],d,b(C[8],f,h))},P=2-a(j[17][1],v)|0;return p(j[17][83],O,P,0,N);case
5:var
Q=c[2],R=a(o,1),e=[0,a(f[T],R),e],n=Q;continue;case
6:var
q=i(C[11],e,0,c[1]),w=1-a(f[7],q);if(w){if(d){h[1]=[0,q,h[1]];return 0}k[1]=[0,q,k[1]];var
x=0}else
var
x=w;return x;default:var
E=c[2],F=c[1];if(c[3]){var
s=a(y,i(C[11],e,0,n));if(d)h[1]=[0,s,h[1]];else
k[1]=[0,s,k[1]]}var
t=ao(0,F,E,l),G=function(f,i,c){var
h=a(Z[1][1][3],c);return g(e,d,b(C[8],f,h))},H=function(b){var
c=1-a(j[17][1],b)|0;return p(j[17][83],G,c,0,b)};if(d)var
I=function(a){return a?0:1},u=b(j[19][28],I,t);else
var
u=d;if(u)m[1]=1;return b(j[19][13],H,t)}}}switch(e){case
0:g(0,0,c);break;case
1:g(0,1,c);break;default:var
d=a(f[79],c),n=d[2],q=d[1],r=function(c){var
b=a(o,1);return a(f[T],b)};g(b(j[17][14],r,q),0,n);m[1]=0}return[0,m[1],[0,h[1],k[1]]]}var
n=[0,a0,aA,bQ,bR,bT,ao,bW,a2,function(n,s,f,e,m){var
i=aB(e);try{var
o=a(m,0)+1|0,p=a0[1]?a2(e,m,n,f):bU,q=p[1],u=p[2];if(1===n){var
k=ap(e,f);switch(k[0]){case
0:var
g=0;break;case
1:var
g=3;break;case
2:var
g=1;break;case
3:var
g=2;break;case
4:var
w=L(ao(0,k[1],k[2],e),0)[1],x=a(j[17][bG],w),g=[0,o,a(Z[1][1][3],x),q];break;case
5:var
g=4;break;default:throw[0,_,k[1]]}var
r=[1,g]}else{var
c=ap(e,f);switch(c[0]){case
0:var
l=c[1],y=c[2],z=a(i,l),b=ap(e,l);switch(b[0]){case
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
A=c[1];if(c[3])throw[0,_,a(i,f)];var
h=[0,A];break;case
3:var
B=c[1];if(c[3])throw[0,_,a(i,f)];var
h=[1,B];break;case
4:var
h=[3,c[1]];break;case
5:var
h=[2,o,c[1],q];break;default:throw[0,_,c[1]]}var
r=[0,h]}var
v=[0,[0,s,a(i,f),r,u]];return v}catch(a){a=t(a);if(a[1]===_)return[1,a[2]];throw a}}];O(106,n,"Ground_plugin.Formula");var
M=[bL,bX,by(0)];function
aD(U,S){var
g=a(B[2],0),n=[0,0];function
q(c,a){var
d=n[1];function
e(d){var
e=d[1];return[0,e,b(w[55],[0,[0,c,a],0],d[2])]}n[1]=[0,[0,c,a],b(j[17][12],e,d)];return 0}function
r(c){var
d=a(f[ai],c);if(2===d[0]){var
e=d[1];try{var
g=r(b($[4][2],e,n[1]));return g}catch(a){a=t(a);if(a===J)return c;throw a}}return c}b(B[3],[0,U,S],g);try{for(;;){var
y=a(B[5],g),V=y[2],h=r(b(aC[24],aa[16],y[1])),i=r(b(aC[24],aa[16],V)),e=a(f[ai],h),c=a(f[ai],i);switch(e[0]){case
2:var
o=e[1];if(2===c[0]){var
v=c[1];if(1-(o===v?1:0))if(o<v)q(v,h);else
q(o,i)}else{var
u=b(w[55],n[1],i),_=a(w[44],u);if(a($[2][2],_))var
ab=a(f[T],o),Q=b(w[54],ab,u)?0:(q(o,u),1);else
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
E=c[3],D=c[2],C=ac,A=ad,d=4;break;default:var
d=2}break;case
7:var
ag=e[3],ah=e[2];switch(c[0]){case
2:var
d=0;break;case
5:var
d=1;break;case
7:var
E=c[3],D=c[2],C=ag,A=ah,d=4;break;default:var
d=2}break;case
9:var
F=e[2],aj=e[1];switch(c[0]){case
2:var
d=0;break;case
5:var
d=1;break;case
9:var
G=c[2];b(B[3],[0,aj,c[1]],g);var
H=F.length-1;if(H!==G.length-1)throw[0,M,h,i];var
I=H-1|0,ak=0;if(!(I<0)){var
k=ak;for(;;){var
al=L(G,k)[k+1],am=[0,L(F,k)[k+1],al];b(B[3],am,g);var
an=k+1|0;if(I!==k){var
k=an;continue}break}}var
d=3;break;default:var
d=2}break;case
13:var
K=e[4],ao=e[3],ap=e[2];switch(c[0]){case
2:var
d=0;break;case
5:var
d=1;break;case
13:var
N=c[4],aq=c[3];b(B[3],[0,ap,c[2]],g);b(B[3],[0,ao,aq],g);var
O=K.length-1;if(O!==N.length-1)throw[0,M,h,i];var
P=O-1|0,ar=0;if(!(P<0)){var
l=ar;for(;;){var
as=L(N,l)[l+1],at=[0,L(K,l)[l+1],as];b(B[3],at,g);var
au=l+1|0;if(P!==l){var
l=au;continue}break}}var
d=3;break;default:var
d=2}break;default:var
d=0}switch(d){case
0:if(2===c[0]){var
z=c[1],s=b(w[55],n[1],h),Y=a(w[44],s);if(a($[2][2],Y)){var
Z=a(f[T],z);if(b(w[54],Z,s))var
x=1;else{q(z,s);var
m=2,x=0}}else
var
x=1;if(x)throw[0,M,h,i]}else
if(5===e[0]){var
X=[0,a(f[99],h),i];b(B[3],X,g);var
m=2}else
var
m=0;break;case
1:var
m=0;break;case
2:var
m=1;break;case
3:var
m=2;break;default:b(B[3],[0,A,D],g);var
ae=a(w[56],E),af=[0,a(w[56],C),ae];b(B[3],af,g);var
m=3}switch(m){case
0:if(5===c[0]){var
W=[0,h,a(f[99],i)];b(B[3],W,g);var
p=1}else
var
p=0;break;case
1:var
p=0;break;case
2:var
p=1;break;default:var
p=2}switch(p){case
0:if(1-b(f[139],h,i))throw[0,M,h,i];var
R=0;break;case
1:var
R=0;break;default:var
R=1}continue}}catch(a){a=t(a);if(a===B[1])return n[1];throw a}}function
bY(e,b){function
c(b){if(a(f[7],b))if(a(f[30],b)===e)return 0;function
g(a,d){var
b=c(d);return 0<=a?0<=b?a+b|0:a:b}var
d=i(f[bF],g,-1,b);return 0<=d?d+1|0:-1}return c(b)}function
bZ(e){var
c=[0,1],d=[0,0];function
g(e,h){var
i=a(f[ai],h);if(2===i[0]){var
j=i[1];try{var
m=e+b($[4][2],j,d[1])|0,n=a(f[al],m);return n}catch(b){b=t(b);if(b===J){var
k=c[1];c[1]++;d[1]=[0,[0,j,k],d[1]];return a(f[al],k+e|0)}throw b}}function
l(a){return a+1|0}return p(f[144],l,g,e,h)}var
h=g(0,e);return[0,c[1]-1|0,h]}function
b0(e,d,c,i){try{var
j=aD(c,i),g=b($[4][2],e,j);if(a(f[7],g))var
h=[0,[1,d]];else
var
k=bY(e,c),h=[0,[0,bZ(g),k]];return h}catch(a){a=t(a);if(a[1]===M)return 0;if(a===J)return[0,[1,d]];throw a}}function
a3(e,d,c){function
g(b){return a(f[T],e+b|0)}var
h=b(j[17][48],d,g);return b(C[12],h,c)}var
aq=[0,M,aD,b0,function(e,d){var
c=e[1],g=d[2],h=d[1],i=a3(0,c,e[2]),k=a3(c,h,g);try{var
l=aD(i,k),m=function(b){var
d=b[1]<c?1:0,e=b[2];return d?d:a(f[7],e)},n=b(j[17][22],m,l);return n}catch(a){a=t(a);if(a[1]===M)return 0;throw a}}];O(ah,aq,"Ground_plugin.Unify");function
a5(a){if(0===a[0]){var
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
0:return ax;case
1:return 80;case
2:return 70;case
3:return-20;case
4:return 50;default:return-10}}}var
d=a[1];if(typeof
d==="number")switch(d){case
0:return ax;case
1:return 40;case
2:return-15;case
3:return-50;default:return ax}return-29}var
b2=[0,function(b,a){var
c=a5(a[3]);return a5(b[3])-c|0}],aG=[0,f[141]],b3=[0,function(c,a){var
e=a[2],f=c[2],d=b(ac[18][1],c[1],a[1]);if(0===d){var
g=function(c,a){var
d=v.caml_int_compare(c[1],a[1]),e=a[2],f=c[2];return 0===d?b(aG[1],f,e):d};return i(ab[5],g,f,e)}return d}],l=a(j[21][1],aG),ae=a(j[20][1],b3);function
as(d,e,c){try{var
a=[0,e,b(l[22],d,c)],f=i(l[4],d,a,c);return f}catch(a){a=t(a);if(a===J)return i(l[4],d,[0,e,0],c);throw a}}function
a6(c,e,a){try{var
f=b(l[22],c,a),g=function(a){return 1-b(ac[5],a,e)},d=b(j[17][29],g,f),h=d?i(l[4],c,d,a):b(l[6],c,a);return h}catch(b){b=t(b);if(b===J)return a;throw b}}var
R=a(aF[2],b2);function
b4(a){var
b=a.slice();b[8]=a[8]-1|0;return b}function
b5(d,a){var
c=a.slice();c[7]=b(ae[4],d,a[7]);return c}function
b6(a,c){var
d=b(ae[3],a,c[7]);if(d)var
e=d;else{var
f=a[2],i=a[1];if(f){var
g=f[1],j=g[1],h=function(a){var
c=a[2],k=a[1];if(c){var
d=c[1],l=d[1],e=b(ac[5],i,k);if(e){var
f=j<l?1:0;if(f)return b(aq[4],d,g);var
h=f}else
var
h=e;return h}return 0};return b(ae[16],h,c[7])}var
e=0}return e}function
aH(f,e,l,a,k){var
g=F(n[9],f,e,l,k,a[6]);if(0===g[0]){var
c=g[1];if(1===f){var
m=a[8],o=a[7],p=a[6],q=c[2],r=a[3],s=a[2];return[0,b(R[2],c,a[1]),s,r,q,0,p,o,m]}var
h=a.slice();h[1]=b(R[2],c,a[1]);h[2]=as(c[2],e,a[2]);return h}var
d=g[1];if(1===f){var
i=a.slice();i[4]=d;i[5]=[0,d];return i}var
j=a.slice();j[2]=as(d,e,a[2]);j[3]=[0,d,a[3]];return j}function
b7(c,a){function
d(a,b){return a[1]===n[7]?b:as(a[2],a[1],b)}var
b=a.slice();b[1]=i(j[17][16],R[2],c,a[1]);b[2]=i(j[17][16],d,c,a[2]);return b}function
b8(d,c){var
e=b(l[22],d,c[2]);return a(j[17][3],e)}function
b9(g){var
b=g;for(;;){var
c=a(R[3],b[1]),f=a(R[4],b[1]);if(c[1]===n[7]){var
d=b.slice();d[1]=f;if(b[4]===c[2])return[0,c,d];var
b=d;continue}var
e=b.slice();e[1]=f;e[2]=a6(c[2],c[1],b[2]);return[0,c,e]}}function
b_(d){var
b=[0,-1],e=ae[1];function
c(a){if(a)b[1]++;return b[1]}var
g=a(f[T],1);return[0,R[1],l[1],0,g,0,c,e,d]}function
b$(c){if(2===c[0]){var
d=c[1],e=function(a){return[3,[0,d,a+1|0]]},f=a(am[21],d);return b(j[17][48],f,e)}return[0,c,0]}var
ca=a(j[17][ah],b$);function
cb(e,d,c){var
f=a(ca,e);function
g(d,c){var
f=c[2],g=c[1];function
h(a){return p(aa[161],0,0,0,a)}var
e=i(r[24],h,f,d),a=e[1];return[0,aH(0,d,b(r[15],a,e[2]),g,a),a]}return i(j[17][16],g,f,[0,d,c])}function
cc(f,e,c){var
d=[0,e];function
g(g){var
e=a(aE[30],g[7]);switch(e[0]){case
1:case
4:case
5:return 0;default:var
f=e[1][1][1];try{var
h=a(ac[16],f),i=b(r[15],c,f);d[1]=aH(2,h,i,d[1],c);var
j=0;return j}catch(a){a=t(a);if(a===J)return 0;throw a}}}function
h(d,c,a){return b(j[17][11],g,a)}function
i(d){try{var
c=a(aE[15],d),e=c}catch(c){c=t(c);if(c!==J)throw c;var
f=b(y[16],d,cd),g=b(y[16],ce,f),e=a(ad[6],g)}return b(aE[14][12],h,e)}b(j[17][11],i,f);return[0,d[1],c]}function
cf(c){function
d(e,d,c){var
f=aa[16],g=a(az[2],0),h=F(b1[6],0,0,g,f,e),i=a(m[17],0),j=a(a4[23],h),k=a(m[1],cg),l=b(m[51],ar[42],d),n=a(m[1],ch),o=b(m[13],n,l),p=b(m[13],o,k),q=b(m[13],p,j),r=b(m[13],q,i);return b(m[13],r,c)}var
e=a(m[1],ci),f=a(m[9],0),g=i(l[11],d,c,f),h=a(m[17],0),j=a(m[1],cj),k=b(m[13],j,h),n=b(m[13],k,g),o=b(m[13],n,e);return b(m[27],0,o)}var
o=[0,aG,[0,l[1],l[2],l[3],l[4],l[5],l[6],l[7],l[8],l[9],l[10],l[11],l[12],l[13],l[14],l[15],l[16],l[17],l[18],l[19],l[20],l[21],l[22],l[23],l[24]],ae,as,a6,R,b4,b5,b6,aH,b7,b8,b9,b_,cb,cc,cf];O(G,o,"Ground_plugin.Sequent");function
u(h,g,f,p,c){a(cl[2],0);var
q=a(r[9],c),k=a(r[8],c);function
l(t,s,q){var
f=t,e=s,d=q;for(;;){if(0<f){if(e){var
n=e[2],g=e[1],h=a(Z[2][1][1],g),u=a(r[7],c);if(!i(w[41],k,h,u)){var
v=b(w[42],k,h);if(!b(j[17][23],v,d)){var
x=l(f-1|0,n,[0,g,d]),y=a(Z[2][1][3],g);return F(o[10],0,[0,h],y,x,c)}}var
f=f-1|0,e=n,d=[0,g,d];continue}var
z=a(m[1],cm);return i(ad[3],0,0,z)}return p}}var
d=l(h,q,0);if(g)var
s=a(r[7],c),e=F(o[10],1,n[7],s,d,c);else
var
e=d;return b(f,e,c)}function
cn(a){if(0===a[0])return a[1];throw[0,I,co]}function
K(b){if(0===b[0]){var
c=a(h[74],[0,b[1],0]);return a(g[66][8],c)}return d[1]}function
cp(e,c){try{var
i=function(b){var
c=a(h[42],b);return a(g[66][8],c)},j=b(o[12],e,c),k=b(d[67],j,i);return k}catch(c){c=t(c);if(c===J){var
f=a(m[1],cq);return b(d[24],0,f)}throw c}}function
cr(n,l,e,k,c){var
p=0,q=1;function
r(a){return u(q,p,k,c,a)}try{var
v=[0,a(g[66][8],h[16]),0],w=[0,K(e),v],x=function(c){function
i(b){var
d=[0,a(f[G],[0,b,[0,c]]),0],e=a(h[U],d);return a(g[66][8],e)}return b(d[67],e,i)},y=b(o[12],n,c),z=[0,b(d[67],y,x),w],A=a(d[7],z),j=A}catch(c){c=t(c);if(c!==J)throw c;var
s=a(m[1],cs),j=b(d[24],0,s)}return i(d[33],j,r,l)}function
ct(e,c,b){var
f=1,j=0;function
k(a){return u(j,f,c,b,a)}var
l=a(g[66][8],h[bJ]);return i(d[33],l,k,e)}function
cu(f,e,c){var
i=1,j=0;function
k(a){return u(j,i,e,c,a)}var
l=a(d[22],k),m=[0,a(g[66][1],l)],n=b(h[111],0,m),o=a(g[66][8],n);return b(d[4],o,f)}function
cv(f,e,c){var
j=1,k=1;function
l(a){return u(k,j,e,c,a)}var
m=a(d[22],l),n=a(g[66][8],h[17]),o=b(d[5],n,m),p=b(d[4],o,f),q=1,r=1;function
s(a){return u(r,q,e,c,a)}var
t=a(g[66][8],h[16]);return i(d[33],t,s,p)}function
cw(l,k,e,j,i,c){var
f=L(b(n[5],l,c),0)[1],m=0;function
o(a){return u(f,m,j,i,a)}var
q=a(g[66][8],h[16]),r=[0,b(d[26],f,q),0],s=[0,K(e),r],t=b(d[70][57],e,h[99]),v=[0,a(g[66][8],t),s],w=a(d[7],v);return p(d[33],w,o,k,c)}function
cx(l,k,e,i,f,c){var
m=b(n[5],l,c);function
o(c){var
j=0,k=0,l=[0,function(a){return u(c,k,i,f,a)},j],m=a(g[66][8],h[16]),n=[0,b(d[26],c,m),l],o=[0,K(e),n];return a(d[7],o)}var
q=b(j[19][15],o,m),r=b(d[70][57],e,h[99]),s=a(g[66][8],r);return p(d[35],s,q,k,c)}function
cy(c){var
e=b(d[70][57],c,h[99]);return a(g[66][8],e)}function
cz(c,l,r,k,q,o,i){var
s=c[2],t=c[1],m=p(n[6],0,c,l,i),e=m.length-1,v=a(j[19][12],l),x=0;function
y(a){return u(e,x,q,o,a)}var
z=a(g[66][8],h[16]),A=[0,b(d[26],e,z),0],B=[0,K(k),A];function
D(q){function
c(d){var
e=L(m,d)[d+1],c=a(j[17][1],e),g=[0,a(f[bH],[0,[0,t,d+1|0],s]),v],h=a(f[G],g);function
i(b){return a(f[al],c-b|0)}var
k=b(j[19][2],c,i),l=[0,b(C[8],c,h),k],n=[0,a(f[G],l)],o=[0,b(C[8],c,q),n],p=a(f[G],o);return b(w[23],p,e)}var
d=b(j[17][48],e,c),i=a(h[U],d);return a(g[66][8],i)}var
E=[0,b(d[67],k,D),B],F=a(d[7],E);return p(d[33],F,y,r,i)}function
cA(k,j,m,l,c,i,e){var
n=[0,0,k,b(C[8],1,j)],o=a(f[bJ],n),p=0,q=0,r=0,s=1,t=2;function
v(a){return u(t,s,i,e,a)}var
w=[0,a(d[22],v),r],x=[0,a(g[66][8],h[17]),w],y=[0,a(g[66][8],h[17]),x],z=[0,K(c),y];function
A(l){var
c=a(f[al],2),d=[0,0,b(C[8],1,k),c],e=[0,l,[0,a(f[bD],d)]],i=[0,0,j,a(f[G],e)],m=[0,a(f[bD],i),0],n=a(h[U],m);return a(g[66][8],n)}var
B=[0,b(d[67],c,A),z],D=[0,a(d[7],B),q];function
E(b){var
c=a(h[42],b);return a(g[66][8],c)}var
F=[0,b(d[67],c,E),D],H=a(h[ak],o),I=a(g[66][8],H),J=[0,b(d[11],I,F),p],L=0,M=0,N=1,O=[0,function(a){return u(N,M,i,e,a)},L],P=[0,K(c),O],Q=[0,a(g[66][8],h[17]),P],R=[0,a(d[7],Q),J],S=a(h[ak],m),T=a(g[66][8],S),V=b(d[11],T,R);return b(d[4],V,l)}function
cB(f,e,c){if(n[1][1])var
k=a(m[1],cC),j=b(d[24],0,k);else
var
j=f;var
l=1,o=0;function
p(a){return u(o,l,e,c,a)}var
q=a(d[22],p),r=a(g[66][8],h[17]),s=b(d[5],r,q),t=b(d[4],s,f),v=1,w=0;function
x(a){return u(w,v,e,c,a)}var
y=a(g[66][8],h[16]),z=i(d[33],y,x,t);return b(d[4],z,j)}function
cD(l,k,e,j,i,c){var
f=L(b(n[5],l,c),0)[1],m=0,o=0,q=f-1|0,r=[0,function(a){return u(q,o,j,i,a)},m],s=a(g[66][8],h[16]),t=[0,b(d[26],f,s),r],v=[0,K(e),t],w=a(d[7],v),x=b(d[70][57],e,h[99]),y=a(g[66][8],x);return p(d[33],y,w,k,c)}function
cE(l,k,j,e,c){var
m=0,n=a(o[7],c),p=1,q=0;function
s(a){return u(q,p,e,n,a)}var
t=[0,a(d[22],s),m],v=0,w=a(o[7],c),x=0,y=1;function
z(a){return u(y,x,e,w,a)}var
A=[0,a(d[22],z),v],B=[0,a(g[66][8],h[16]),A],C=[0,K(j),B];function
D(j,c){var
e=b(r[11],c,1),k=[0,j,[0,a(f[ah],e)]],l=a(f[G],k),m=a(h[74],[0,e,0]),n=a(g[66][8],m),o=a(h[U],[0,l,0]),p=a(g[66][8],o);return i(d[5],p,n,c)}var
E=[0,b(d[67],j,D),C],F=[0,a(g[66][8],h[16]),E],H=[0,a(d[7],F),t],I=a(h[ak],l),J=a(g[66][8],I),L=b(d[11],J,H);return b(d[4],L,k)}function
a8(a){return i(ck[4],cG,cF,a)}var
N=[aV,function(e){var
b=a8(cH),c=[0,[0,0,[1,a(f[41],b)[1]]],0],d=a8(cI);return[0,[0,0,[1,a(f[41],d)[1]]],c]}];function
cJ(c){if(c){var
d=bz(N),f=[0,c[1],1],i=bE===d?N[1]:aV===d?a(a7[2],N):N,j=b(h[68],i,f);return a(g[66][8],j)}var
e=bz(N),k=bE===e?N[1]:aV===e?a(a7[2],N):N,l=a(h[67],k);return a(g[66][8],l)}var
q=[0,u,cn,K,cp,cr,ct,cu,cv,cw,cx,cy,cz,cA,cB,cD,cE,a(d[56],cJ)];O(bH,q,"Ground_plugin.Rules");function
cN(c,a){if(0===c[0]){var
d=c[1],e=d[1],g=c[2],h=d[2];if(0===a[0]){var
f=a[1],i=a[2],j=f[2],k=f[1],l=o[1][1],m=function(b,a){return b-a|0},p=function(b,a){return b-a|0},q=b(n[3],p,m);return aQ(b(n[4],q,l),k,e,g,i,h,j)}return 0===e?1:-1}var
r=c[1];return 0===a[0]?0===a[1][1]?-1:1:b(o[1][1],r,a[1])}function
cO(c,a){return c===a?0:c===n[7]?1:a===n[7]?-1:b(ac[18][1],c,a)}var
cP=[0,function(c,a){var
d=a[2],e=a[1],f=c[2],g=c[1];return p(b(n[3],cN,cO),e,g,d,f)}],at=a(j[20][1],cP);function
a$(c,l){var
d=[0,at[1]];function
e(f){var
h=f[3];if(0===h[0]){var
c=h[1];if(typeof
c==="number")var
n=1;else
if(2===c[0])var
x=c[3],k=c[2],w=c[1],g=1,n=0;else
var
n=1;if(n)var
g=0}else{var
e=h[1];if(typeof
e==="number")var
g=0;else
var
x=e[3],k=e[2],w=e[1],g=1}if(g){var
y=f[4],r=[0,1],s=[0,x],D=f[1],t=function(c,a){function
e(f,e){var
a=p(aq[3],w,k,f,e);if(a){var
c=a[1];return 0===c[0]?(r[1]=0,d[1]=b(at[4],[0,c,D],d[1]),0):(s[1]=1,0)}return 0}var
f=c[1];function
g(c){var
d=a[2];function
f(a){return e(c,a)}return b(j[17][11],f,d)}b(j[17][11],g,f);var
h=c[2];function
i(c){var
d=a[1];function
f(a){return e(c,a)}return b(j[17][11],f,d)}return b(j[17][11],i,h)},A=l[1],B=function(a){return t(y,a[4])};b(o[6][5],B,A);var
q=l[5],z=q?[0,q[1],0]:0;t(y,[0,z,l[3]]);var
u=r[1],v=u?s[1]:u,E=v?(d[1]=b(at[4],[0,[1,k],f[1]],d[1]),0):v;return E}var
C=a(m[1],cQ);return i(ad[3],0,0,C)}b(j[17][11],e,c);return a(at[20],d[1])}function
ba(b){try{var
f=a(o[13],b),g=f[1],c=g[3],k=f[2];if(0===c[0]){var
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
j=ba(k),i=[0,[0,g,j[1]],j[2]];else
var
i=[0,0,b];return i}catch(a){a=t(a);if(a===aF[1])return[0,0,b];throw a}}var
bb=a(x[1][5],cR);function
cS(y,x,c,d,w){var
o=a(r[8],c),p=a(a_[2],c);if(y===n[7])var
q=bb;else
var
G=b(r[15],c,x),H=i(aC[25],o,p,G),v=a(f[34],H)[1],I=v?v[1]:bb,q=I;function
z(b){return a(f[al],d-b|0)}var
A=b(j[17][48],d,z),m=d,l=0,k=o,g=p,e=0,B=b(C[12],A,w);for(;;){if(0===m)return[0,g,e,B];var
s=i(h[14],l,q,c),D=a(a9[21][2],g),t=bA(cL[7],k,D,0,0,0,0,aa[bG]),E=t[1][1],u=[0,[0,s],E],F=a(a9[6],t[2]),m=m-1|0,l=[0,s,l],k=b(cM[20],u,k),g=F,e=[0,u,e];continue}}var
V=[0,ba,a$,function(s,k,l,e,c){var
u=a$(s,e);function
v(j){if(j[2]===n[7])var
c=j[1],k=function(i,j){if(0===c[0]){var
e=c[1];if(0===e[1]){var
k=e[2],l=a(o[7],j),n=[0,p(q[1],0,1,i,l),0],s=a(d[20],n),t=a(h[bO],[0,[0,k,0]]),u=a(g[66][8],t);return b(d[5],u,s)}var
v=a(m[1],cW);return b(d[24],0,v)}var
w=c[1],x=a(g[66][8],h[41]),y=[0,a(d[21],x),0],z=a(o[7],j),A=[0,p(q[1],0,1,i,z),0],B=[0,a(d[20],A),0],C=[0,function(c){var
d=b(r[11],c,1),e=[0,[0,a(f[ah],d),0]],i=a(h[bO],e);return b(g[66][8],i,c)},B],D=[0,a(g[66][8],h[17]),C],E=[0,a(d[7],D),y],F=a(h[ak],w),G=a(g[66][8],F);return b(d[11],G,E)};else
var
k=function(v,k){var
c=j[2],l=j[1];if(0===l[0]){var
e=l[1],n=e[2],s=e[1];if(b(o[9],[0,c,[0,e]],k)){var
x=a(m[1],cT);return b(d[24],0,x)}if(0<s)var
y=function(k,e){var
j=cS(c,k,e,s,n),o=j[2],q=j[1],u=a(f[G],[0,k,[0,j[3]]]),l=b(w[23],u,o);try{var
A=a(r[8],e),B=p(cK[2],0,A,q,l),m=B}catch(b){b=t(b);if(!a(ad[22],b))throw b;var
m=a(ad[6],cU)}var
v=m[1],x=a(h[U],[0,l,0]),y=a(g[66][8],x),z=a(a_[11],v);return i(d[5],z,y,e)},u=b(d[67],c,y);else
var
E=function(b){var
c=[0,a(f[G],[0,b,[0,n]]),0],d=a(h[U],c);return a(g[66][8],d)},u=b(d[67],c,E);var
z=b(o[8],[0,c,[0,e]],k),A=a(o[7],z),B=[0,p(q[1],1,0,v,A),0],C=[0,a(d[20],B),0],D=[0,u,[0,a(g[66][8],h[17]),C]];return a(d[7],D)}var
F=l[1];if(b(o[9],[0,c,0],k)){var
H=a(m[1],cV);return b(d[24],0,H)}var
I=a(g[66][8],h[41]),J=[0,a(d[21],I),0],K=b(o[8],[0,c,0],k),L=a(o[7],K),M=[0,p(q[1],1,0,v,L),0],N=[0,a(d[20],M),0],O=[0,a(g[66][8],h[17]),N];function
P(d,c){var
e=b(r[11],c,1),i=[0,d,[0,a(f[ah],e)]],j=[0,a(f[G],i),0],k=a(h[U],j);return b(g[66][8],k,c)}var
Q=[0,b(d[67],c,P),O],R=[0,a(g[66][8],h[17]),Q],S=[0,a(d[7],R),J],T=a(h[ak],F),V=a(g[66][8],T);return b(d[11],V,S)};return k(l,e)}var
x=b(j[17][12],v,u),y=a(d[19],x);return i(d[4],y,k,c)}];O(137,V,"Ground_plugin.Instances");var
aI=[0,function(X,s,e){var
c=[0,x[18][1]];function
h(d){try{var
e=a(be[23],d),g=a(f[41],e)[1];c[1]=b(x[18][7],g,c[1]);var
h=0;return h}catch(a){a=t(a);if(a===f[28])return 0;throw a}}var
j=a(be[26],0);b(bc[11],h,j);var
k=a(x[18][12],c[1]),l=b(H[8][13],H[14],[0,x[1][11][2],k]);n[2][1]=l;function
u(x,j,E){if(v.caml_equal(a(z[8],0),cX)){var
Y=a(ar[66],E);b(bd[16],0,Y)}try{var
H=a(o[13],j),g=H[2],h=H[1],e=function(a){return b(o[11],x,a)},_=0,f=function(a,b){return u(_,a,b)},c=function(a){return u([0,h,x],g,a)},A=h[3];if(0===A[0]){var
l=A[1];if(typeof
l==="number")var
r=a(q[11],h[1]);else
switch(l[0]){case
0:var
$=l[1],aa=e(g),r=F(q[9],$,c,h[1],f,aa);break;case
1:var
ab=l[1],ac=e(g),r=F(q[10],ab,c,h[1],f,ac);break;case
2:var
J=a(V[1],j),K=J[1],ad=J[2],ae=b(y[22],K,x),L=function(a){return u(ae,ad,a)};if(n[1][1])if(0<j[8])var
af=e(j),M=p(V[3],K,L,f,af),B=1;else
var
B=0;else
var
B=0;if(!B)var
M=L;var
r=M;break;case
3:var
ag=l[1];if(n[1][1])var
ah=e(g),N=F(q[15],ag,c,h[1],f,ah);else
var
N=c;var
r=N;break;default:var
k=l[2],ai=l[1];if(typeof
k==="number")var
w=c;else
switch(k[0]){case
3:var
an=k[1];if(0<j[8])if(n[1][1])var
ao=e(g),O=F(q[16],an,c,h[1],f,ao),C=1;else
var
C=0;else
var
C=0;if(!C)var
O=c;var
w=O;break;case
4:var
ap=k[2],aq=k[1];if(n[1][1])var
as=e(g),P=aQ(q[12],aq,ap,c,h[1],f,as);else
var
P=c;var
w=P;break;case
5:var
at=k[3],au=k[2],av=k[1],aw=e(g),w=bA(q[13],av,au,at,c,h[1],f,aw);break;default:var
ak=k[2],al=k[1],am=e(g),w=aQ(q[12],al,ak,c,h[1],f,am)}var
aj=e(g),r=F(q[5],ai,w,h[1],f,aj)}var
I=r}else{var
Q=A[1];if(typeof
Q==="number")switch(Q){case
0:var
ax=e(g),s=i(q[8],c,f,ax);break;case
1:var
ay=e(g),s=i(q[6],c,f,ay);break;case
2:var
az=e(g),s=i(q[7],c,f,az);break;case
3:var
s=c;break;default:if(n[1][1])var
aA=a(m[1],cY),R=b(d[24],0,aA);else
var
R=c;var
aB=e(g),s=i(q[14],R,f,aB)}else{var
S=a(V[1],j),T=S[1],aC=S[2],aD=b(y[22],T,x),U=function(a){return u(aD,aC,a)};if(n[1][1])if(0<j[8])var
aE=e(j),W=p(V[3],T,U,f,aE),D=1;else
var
D=0;else
var
D=0;if(!D)var
W=U;var
s=W}var
I=s}var
G=I}catch(a){a=t(a);if(a!==aF[1])throw a;var
G=X}var
Z=b(q[4],j[4],j);return i(d[4],Z,G,E)}var
g=a(s,e),w=g[2],A=g[1],B=0;function
C(a,b){return u(B,a,b)}var
D=a(r[9],e),E=a(bc[1],D);return F(q[1],E,1,C,A,w)}];O(bF,aI,"Ground_plugin.Ground");a(aL[12],c4);var
ag=[0,3];function
c5(a){return a?(ag[1]=b(y[5],a[1],0),0):(ag[1]=3,0)}var
c8=[0,1,0,c7,c6,function(a){return[0,ag[1]]},c5];b(bi[3],0,c8);var
X=[0,ax];function
c9(a){return a?(X[1]=b(y[5],a[1],0),0):(X[1]=0,0)}var
da=[0,1,0,c$,c_,function(a){return[0,X[1]]},c9];b(bi[3],0,da);var
db=[0,bo,0],dc=[0,function(b,a){return p(bm[14],0,0,0,0)}];i(aJ[9],0,bo,dc);var
bp=[31,E[4],db,0],aN=b(cZ[1],[0,bp],dd),bq=aN[3],aO=aN[2],br=aN[1],de=0,dg=[0,[0,0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(k[4],s[14]),f=b(k[8],e,d);return function(e){var
c=a(bg[3],f),d=a(bh[10][2],0);return b(br,a(bh[6],d),c)}}return a(y[2],df)}],de];function
dh(b,a){return i(bf[1],a[1],[0,di,b],a[2])}b(W[80],dh,dg);var
dj=0,dl=[0,function(b){if(b)if(!b[2])return function(a){return au[6]};return a(y[2],dk)},dj];function
dm(c,a){return b(au[3],[0,dn,c],a)}b(W[80],dm,dl);var
dp=[6,a(D[12],s[14])],dq=a(k[4],s[14]),du=[0,[0,dt,[0,ds,[0,dr,[0,[1,E[4],dq,dp],0]]]],0];function
dv(b,a){return i(bk[1],[0,dw,b],0,a)}b(W[80],dv,du);var
dx=0,dA=[0,[0,0,function(c){return c?a(y[2],dy):function(f){var
c=a(bq,0),d=a(m[1],dz),e=b(m[13],d,c);return b(bd[12],0,e)}}],dx];function
dB(b,a){return i(bf[1],a[1],[0,dC,b],a[2])}b(W[80],dB,dA);var
dD=0,dF=[0,function(b){return b?a(y[2],dE):function(a){return au[5]}},dD];function
dG(c,a){return b(au[3],[0,dH,c],a)}b(W[80],dG,dF);function
dJ(b,a){return i(bk[1],[0,dK,b],0,a)}b(W[80],dJ,dI);var
dM=a(m[1],dL),dN=b(d[24],0,dM);function
Y(h,b,f,e,d){var
c=n[1][1];try{n[1][1]=h;var
j=b?b[1]:a(aO,0)[2],k=function(c){var
d=a(o[14],ag[1]),b=i(o[15],f,d,c);return i(o[16],e,b[1],b[2])},l=a(g[66][8],j),m=i(aI[1],l,k,d);n[1][1]=c;return m}catch(a){a=t(a);n[1][1]=c;throw a}}function
bs(d,c,b){return a(aw[24],c1[41])}function
bt(f,e,d){function
b(b){return a(ar[42],b[2])}var
c=a(a4[6],b);return a(aw[24],c)}function
bu(d,c,b){return a(aw[24],ar[42])}function
dO(b){return a(m[25],dP)}var
bv=p(c3[2],dR,dQ,0,dO),A=a(k[2],dS);function
dT(c,d){var
e=a(k[17],s[18]),f=a(k[4],e),g=b(k[7],f,d),h=b(bg[10],c,g),i=a(k[17],s[18]),j=a(k[5],i);return[0,c,b(k[8],j,h)]}b(bj[5],A,dT);function
dU(d,c){var
e=a(k[17],s[18]),f=a(k[5],e),g=b(k[7],f,c),h=b(c0[2],d,g),i=a(k[17],s[18]),j=a(k[5],i);return b(k[8],j,h)}b(bj[6],A,dU);function
dV(d,c){var
e=a(k[17],s[18]),f=a(k[5],e),g=b(k[7],f,c);return b(z[9],d,g)}b(aM[6],A,dV);var
dW=a(k[17],s[18]),dX=a(k[6],dW),dY=[0,a(aM[2],dX)];b(aM[3],A,dY);var
dZ=a(k[4],A),aP=i(D[13],D[9],d0,dZ),d1=0,d2=0;function
d3(a,c,b){return[0,a,0]}var
d4=[6,D[14][15]],d6=[0,[0,[0,[0,0,[0,a(af[12],d5)]],d4],d3],d2];function
d7(b,e,a,d,c){return[0,a,b]}var
d9=[0,a(af[12],d8)],d_=[2,[6,D[14][15]],d9],ea=[0,a(af[12],d$)],eb=[6,D[14][15]],ed=[0,[0,[0,[0,[0,[0,0,[0,a(af[12],ec)]],eb],ea],d_],d7],d6];function
ee(d,c,a,f,e){b(bv,0,0);return[0,a,[0,c,d]]}var
ef=[3,[6,D[14][15]]],eg=[6,D[14][15]],eh=[6,D[14][15]],ej=[0,[0,[0,[0,[0,[0,0,[0,a(af[12],ei)]],eh],eg],ef],ee],ed],ek=[0,0,[0,[0,0,0,[0,[0,0,function(a){return 0}],ej]],d1]];i(D[23],aP,0,ek);p(aw[1],A,bs,bt,bu);var
el=[0,aP,0];function
em(c){var
d=c[2],e=a(k[4],A);return[0,b(k[7],e,d)]}i(aK[5],en,em,el);var
eo=0,eq=[0,function(c){if(c){var
d=c[2];if(d){var
e=d[2];if(e)if(!e[2]){var
f=e[1],h=d[1],i=c[1],j=a(k[18],s[14]),l=a(k[6],j),m=b(z[2][7],l,i),n=a(k[6],A),o=b(z[2][7],n,h),p=a(k[17],av[7]),q=a(k[6],p),r=b(z[2][7],q,f);return function(c){var
d=a(z[19],c),e=b(ab[15],d,m),f=1;function
h(a){return Y(f,e,o,r,a)}return a(g[66][1],h)}}}}return a(y[2],ep)},eo],es=[0,function(c){if(c){var
d=c[2];if(d)if(!d[2]){var
e=d[1],f=c[1],h=a(k[18],s[14]),i=a(k[6],h),j=b(z[2][7],i,f),l=a(k[17],av[7]),m=a(k[6],l),n=b(z[2][7],m,e);return function(c){var
e=a(z[19],c),d=0,f=b(ab[15],e,j),h=1;function
i(a){return Y(h,f,d,n,a)}return a(g[66][1],i)}}}return a(y[2],er)},eq],eu=[0,function(c){if(c){var
d=c[2];if(d)if(!d[2]){var
e=d[1],f=c[1],h=a(k[18],s[14]),i=a(k[6],h),j=b(z[2][7],i,f),l=a(k[6],A),m=b(z[2][7],l,e);return function(c){var
e=a(z[19],c),d=0,f=b(ab[15],e,j),h=1;function
i(a){return Y(h,f,m,d,a)}return a(g[66][1],i)}}}return a(y[2],et)},es],ev=a(bn[12],eu);i(aJ[9],0,[0,S,ew],ev);function
ex(B){var
j=a(x[1][6],ey),c=av[7],h=0,i=0;if(0===c[0]){var
k=[0,eA,[0,[1,E[4],[0,[5,[0,c[1]]]],j],i]],l=a(x[1][6],eB);if(0===A[0]){var
m=[0,[1,E[4],[5,[0,A[1]]],l],k],n=a(x[1][6],eD),d=s[14];if(0===d[0]){var
o=[0,[0,eF,[0,[1,E[4],[4,[5,[0,d[1]]]],n],m]],h],q=a(x[1][6],eG),e=av[7],p=0;if(0===e[0]){var
r=[0,eI,[0,[1,E[4],[0,[5,[0,e[1]]]],q],p]],t=a(x[1][6],eJ),f=s[14];if(0===f[0]){var
u=[0,[0,eL,[0,[1,E[4],[4,[5,[0,f[1]]]],t],r]],o],v=0,w=a(x[1][6],eM);if(0===A[0]){var
y=[0,[1,E[4],[5,[0,A[1]]],w],v],z=a(x[1][6],eO),g=s[14];if(0===g[0])return b(aK[4],[0,S,eR],[0,[0,eQ,[0,[1,E[4],[4,[5,[0,g[1]]]],z],y]],u]);throw[0,I,eP]}throw[0,I,eN]}throw[0,I,eK]}throw[0,I,eH]}throw[0,I,eE]}throw[0,I,eC]}throw[0,I,ez]}b(aL[19],ex,S);var
eS=0,eU=[0,function(c){if(c)if(!c[2]){var
d=c[1],e=a(k[18],s[14]),f=a(k[6],e),h=b(z[2][7],f,d);return function(c){var
f=a(z[19],c),d=0,e=0,i=b(ab[15],f,h),j=0;function
k(a){return Y(j,i,e,d,a)}return a(g[66][1],k)}}return a(y[2],eT)},eS],eV=a(bn[12],eU);i(aJ[9],0,[0,S,eW],eV);function
eX(g){var
f=a(x[1][6],eY),c=s[14],d=0,e=0;if(0===c[0])return b(aK[4],[0,S,e1],[0,[0,e0,[0,[1,E[4],[4,[5,[0,c[1]]]],f],e]],d]);throw[0,I,eZ]}b(aL[19],eX,S);function
e2(q){var
f=b(bl[3][4],X[1],0),h=a(aO,0)[2],c=0,e=0,j=[0,b(d[70][3],h,f)],k=1;function
l(a){return Y(k,j,e,c,a)}var
m=a(g[66][1],l),n=b(bl[3][4],X[1],0),o=i(bm[18],0,0,0),p=b(d[70][12],o,n);return b(d[70][12],p,m)}var
e3=a(g[13],0),bw=b(g[67][1],e3,e2);a(c2[3][3],bw);var
bx=[0,S,ag,X,bp,br,aO,bq,dN,Y,bs,bt,bu,bv,A,aP,bw];O(170,bx,"Ground_plugin.G_ground");O(171,[0,n,aq,o,q,V,aI,bx],"Ground_plugin");return});
