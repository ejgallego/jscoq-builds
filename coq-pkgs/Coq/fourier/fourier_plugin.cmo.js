(function(dF){"use strict";var
bI=617,bC=145,bD="eqTLR",ax="Rmult",K=140,aA="R0",aB="Ropp",Y="Rle",az="Rminus",bB="fourier_plugin",Q=124,bA="eqTRL",X="Rlt",e=246,bH="Fourier",aj="fourier",$="Rge",bz="plugins/fourier/fourier.ml",bG=113,aw="plugins/fourier/fourierR.ml",_="Rgt",av="Rplus",Z=248,au="Rinv",by="R",bF="Reals",ay="R1",bE="Rdiv",o=dF.jsoo_runtime,j=o.caml_check_bound,bx=o.caml_div,W=o.caml_fresh_oo_id,s=o.caml_mul,b=o.caml_new_string,ai=o.caml_register_global,h=o.caml_string_notequal,F=o.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):o.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):o.caml_call_gen(a,[b,c])}function
V(a,b,c,d){return a.length==3?a(b,c,d):o.caml_call_gen(a,[b,c,d])}function
dE(a,b,c,d,e){return a.length==4?a(b,c,d,e):o.caml_call_gen(a,[b,c,d,e])}var
m=o.caml_get_global_data(),z=[0,0,1],aD=[0,1,1],ah=b(bB),R=m.Assert_failure,l=m.List,G=m.Pervasives,p=m.Term,f=m.Tactics,g=m.Tacticals,P=m.Proofview,I=m.Coqlib,M=m.Names,bs=m.CErrors,bt=m.Equality,aq=m.Array,bv=m.Tacenv,bu=m.Mltop,bS=[0,b(bz),176,9],bQ=[0,b(bz),147,9],bO=b("empty ineq"),bK=b("/"),bR=b("Fourier.Contradiction"),bX=b("not_of_constant"),bZ=b(bE),b0=b(au),b1=b(az),b2=b(ax),b3=b(aB),b4=b(av),b5=b(aA),b6=b(ay),b8=b(bE),b9=b(au),b_=b(az),b$=b(ax),ca=b(aB),cb=b(av),cc=b(aA),cd=b(ay),dl=[0,b("Coq"),[0,b(aj),[0,b(bH),0]]],dm=b("new_hyp_for_fourier"),dt=b($),du=b(_),dv=b(Y),dw=b(X),dn=b("No inequalities"),dp=b("fourier failed"),ds=[0,b(aw),bI,11],dq=[0,b(aw),bI,30],dd=b($),de=b(_),df=b(Y),dg=b(X),dh=b(bD),di=b(bA),dj=[0,b(aw),426,6],db=b("Rlt_not_le_frac_opp"),da=b("Rnot_le_le"),c$=b("Rnot_lt_lt"),c_=b("Rfourier_le_le"),c9=b("Rfourier_le_lt"),c8=b("Rfourier_lt_le"),c7=b("Rfourier_lt_lt"),c6=b("Rfourier_le"),c5=b("Rfourier_lt"),c4=b("Rfourier_not_lt_ge"),c3=b("Rfourier_not_le_gt"),c2=b("Rfourier_not_gt_le"),c1=b("Rfourier_not_ge_lt"),c0=b("Rfourier_eqRL_to_le"),cZ=b("Rfourier_eqLR_to_le"),cY=b("Rfourier_ge_to_le"),cX=b("Rfourier_gt_to_lt"),cW=b("Rle_not_lt"),cV=b("Rnot_lt0"),cU=b("Rle_mult_inv_pos"),cT=b("Rle_zero_1"),cS=b("Rle_zero_zero"),cR=b("Rlt_mult_inv_pos"),cQ=b("Rle_zero_pos_plus1"),cP=b("Rlt_zero_pos_plus1"),cO=b("Rlt_zero_1"),cL=b("Rinv_1"),cM=[0,b(bF),[0,b("RIneq"),0]],cK=b(ay),cJ=b(aA),cI=b(au),cH=b(aB),cG=b(av),cF=b(ax),cE=b(az),cC=b(by),cA=b($),cz=b(Y),cx=b(_),cw=b(X),cg=b($),ch=b(_),ci=b(Y),cj=b(X),ck=b(X),cl=b(Y),cm=b(_),cn=b($),cp=b(by),cq=b(bA),cr=b(bD),bV=b("Coq.Reals.Rdefinitions"),bW=b("constant_not_of_R"),bY=b("FourierR.NoRational"),b7=b("FourierR.NoLinear"),cf=b("FourierR.NoIneq"),ct=b(bH),cv=[0,b(bF),[0,b("Rdefinitions"),0]],cN=[0,b(aj),[0,b("Fourier_util"),0]],dk=b("FourierR.GoalDone"),dz=b(aj),dB=b("fourierz"),dx=b(bB),dD=b(aj),bP=m.Failure,dr=m.Contradiction,cs=m.CamlinternalLazy,co=m.Globnames,bU=m.Not_found,bT=m.Hashtbl,dA=m.Loc;function
bJ(b){a(G[29],b[1]);a(G[27],bK);return a(G[29],b[2])}function
aC(d,c){var
b=d,a=c;for(;;){if(0===a)return b;var
e=o.caml_mod(b,a),b=a,a=e;continue}}function
H(b){var
a=0<=b[2]?b:[0,-b[1]|0,-b[2]|0];if(0===a[1])return z;var
c=aC(a[1],a[2]),d=0<=c?c:-c|0,e=bx(a[2],d);return[0,bx(a[1],d),e]}function
aE(a){return H([0,-a[1]|0,a[2]])}function
ak(b,a){return H([0,s(b[1],a[2])+s(a[1],b[2])|0,s(b[2],a[2])])}function
bL(b,a){return H([0,s(b[1],a[2])-s(a[1],b[2])|0,s(b[2],a[2])])}function
al(b,a){return H([0,s(b[1],a[1]),s(b[2],a[2])])}function
bM(a){return H([0,a[2],a[1]])}function
bN(b,a){return H([0,s(b[1],a[2]),s(b[2],a[1])])}function
aa(b,a){return s(b[1],a[2])<s(a[1],b[2])?1:0}function
aF(b,a){return s(b[1],a[2])<=s(a[1],b[2])?1:0}function
A(b,a){a[1]=[0,b,a[1]];return 0}function
aG(a){var
b=[0,0],d=[0,0],e=[0,0];function
f(a){var
c=a[1];if(c){var
f=c[1];return aa(f,z)?A(a,d):aa(z,f)?A(a,b):A(a,e)}throw[0,bP,bO]}c(l[11],f,a);return[0,d[1],[0,e[1],[0,b[1],0]]]}function
aH(d){var
b=[0,0],h=a(l[1],d);function
e(e){var
a=[0,0],f=(h-b[1]|0)-1|0,i=e[2],j=e[1],k=1;if(!(f<1)){var
d=k;for(;;){A(z,a);var
n=d+1|0;if(f!==d){var
d=n;continue}break}}A(aD,a);var
g=b[1],l=1;if(!(g<1)){var
c=l;for(;;){A(z,a);var
m=c+1|0;if(g!==c){var
c=m;continue}break}}b[1]=b[1]+1|0;return[0,j,a[1],i]}return c(l[13],e,d)}function
aI(b,a){var
c=b[3],d=c||a[3],e=V(l[19],ak,b[2],a[2]);return[0,V(l[19],ak,b[1],a[1]),e,d]}function
am(b,a){var
d=a[3],e=a[2];function
f(a){return al(b,a)}var
g=c(l[13],f,e),h=a[1];function
i(a){return al(b,a)}return[0,c(l[13],i,h),g,d]}function
an(b){var
c=b[3],d=b[2];return[0,a(l[4],b[1]),d,c]}function
ao(b){return a(l[3],b[1])}function
aJ(b,d){var
a=[0,0];function
e(b){function
e(c){var
d=aE(ao(b)),e=ao(c),f=am(d,c);return A(an(aI(am(e,b),f)),a)}return c(l[11],e,d)}c(l[11],e,b);return a[1]}function
aK(e){var
a=aG(e);if(a){var
b=a[2];if(b){var
d=b[2];if(d)if(!d[2]){var
f=b[1],g=aJ(a[1],d[1]),h=c(l[13],an,f);return c(G[22],h,g)}}}throw[0,R,bQ]}function
aL(d){var
f=a(l[3],d)[1],g=a(l[1],f),b=[0,aH(d)],e=g-1|0,h=1;if(!(e<1)){var
c=h;for(;;){b[1]=aK(b[1]);var
i=c+1|0;if(e!==c){var
c=i;continue}break}}return b[1]}var
ap=[Z,bR,W(0)],i=[0,bJ,aC,z,aD,H,aE,ak,bL,al,bM,bN,aa,aF,A,aG,aH,aI,am,an,ao,aJ,aK,aL,ap,function(a){var
b=aL(a);function
d(a){var
b=a[1];if(b)if(!b[2]){var
c=a[3],d=b[1],i=a[2],f=aa(d,z),g=f?1-c:f;if(g)var
e=g;else
var
h=aF(d,z),e=h?c:h;if(e)throw[0,ap,[0,[0,d,c,i],0]];return e}throw[0,R,bS]}try{c(l[11],d,b);var
e=0;return e}catch(a){a=F(a);if(a[1]===ap)return a[2];throw a}}];ai(110,i,"Fourier_plugin.Fourier");var
r=a(bT[18],[0,p[136],p[151]]);function
t(c){var
b=i[3];return[0,a(r[1],50),b]}function
aM(b,a){try{var
d=c(r[7],b[1],a);return d}catch(a){a=F(a);if(a===bU)return i[3];throw a}}function
v(a,b,d){var
e=aM(a,b),f=c(i[7],e,d);V(r[9],a[1],b,f);return a}function
w(a,b){var
d=c(i[7],a[2],b);return[0,a[1],d]}function
aN(b){var
a=i[4];return w(t(0),a)}function
aO(d,b){var
a=t(0),e=d[1];function
f(c,b){v(a,c,b);return 0}c(r[11],f,e);var
g=b[1];function
h(c,b){v(a,c,b);return 0}c(r[11],h,g);var
i=b[2];return w(w(a,d[2]),i)}function
B(e,d){var
b=t(0),f=e[1];function
g(c,a){v(b,c,a);return 0}c(r[11],g,f);var
h=d[1];function
j(d,c){v(b,d,a(i[6],c));return 0}c(r[11],j,h);var
k=a(i[6],d[2]);return w(w(b,e[2]),k)}function
aP(b,a){var
d=t(0),e=a[1];function
f(e,a){v(d,e,c(i[9],b,a));return 0}c(r[11],f,e);return w(d,c(i[9],b,a[2]))}function
L(d){var
b=a(M[bG],d),c=b[1];if(0===c[0]){var
e=b[3],f=c[1];if(o.caml_equal(b[2],M[5][6]))if(o.caml_string_equal(a(M[5][8],f),bV))return a(M[6][5],e)}return bW}function
ab(d){var
c=d;for(;;){var
b=a(p[K],c);switch(b[0]){case
5:var
c=b[1];continue;case
10:return L(b[1][1]);default:return bX}}}var
C=[Z,bY,W(0)];function
q(k){var
f=k;for(;;){var
e=a(p[K],f);switch(e[0]){case
5:var
f=e[1];continue;case
9:var
b=e[2],d=ab(e[1]);if(h(d,bZ)){if(h(d,b0)){if(h(d,b1)){if(h(d,b2)){if(h(d,b3)){if(h(d,b4))throw C;var
l=q(j(b,1)[2]),m=q(j(b,0)[1]);return c(i[7],m,l)}var
n=q(j(b,0)[1]);return a(i[6],n)}var
o=q(j(b,1)[2]),r=q(j(b,0)[1]);return c(i[9],r,o)}var
s=q(j(b,1)[2]),t=q(j(b,0)[1]);return c(i[8],t,s)}var
u=q(j(b,0)[1]);return a(i[10],u)}var
v=q(j(b,1)[2]),w=q(j(b,0)[1]);return c(i[11],w,v);case
10:var
g=L(e[1][1]);if(h(g,b5)){if(h(g,b6))throw C;return i[4]}return i[3];default:throw C}}}var
S=[Z,b7,W(0)];function
n(k){try{var
f=a(p[K],k);switch(f[0]){case
5:var
g=n(f[1]);break;case
9:var
b=f[2],e=ab(f[1]);if(h(e,b8))if(h(e,b9))if(h(e,b_))if(h(e,b$))if(h(e,ca)){if(h(e,cb))throw S;var
y=n(j(b,1)[2]),d=aO(n(j(b,0)[1]),y)}else
var
z=n(j(b,0)[1]),d=aP(a(i[6],i[4]),z);else
try{var
l=q(j(b,0)[1]);try{var
G=q(j(b,1)[2]),H=c(i[9],l,G),I=w(t(0),H),m=I}catch(a){a=F(a);if(a!==C)throw a;var
E=j(b,1)[2],m=v(t(0),E,l)}var
d=m}catch(a){a=F(a);if(a!==C)throw a;var
A=q(j(b,1)[2]),D=j(b,0)[1],d=v(t(0),D,A)}else
var
J=n(j(b,1)[2]),d=B(n(j(b,0)[1]),J);else
var
M=q(j(b,0)[1]),N=a(i[10],M),d=w(t(0),N);else{var
o=q(j(b,1)[2]);try{var
Q=q(j(b,0)[1]),R=c(i[11],Q,o),T=w(t(0),R),r=T}catch(c){c=F(c);if(c!==C)throw c;var
O=a(i[10],o),P=j(b,0)[1],r=v(t(0),P,O)}var
d=r}var
g=d;break;case
10:var
s=L(f[1][1]);if(h(s,cc)){if(h(s,cd))throw S;var
u=aN(0)}else
var
u=t(0);var
g=u;break;default:throw S}return g}catch(a){a=F(a);if(a!==C)if(a!==S)throw a;var
x=i[4];return v(t(0),k,x)}}function
ce(b){var
a=[0,0];function
d(c,b){a[1]=[0,[0,b,c],a[1]];return 0}c(r[11],d,b);return a[1]}var
D=[Z,cf,W(0)];function
aQ(o){var
e=o[1],l=a(p[K],o[2]);if(9===l[0]){var
f=l[2],m=a(p[K],l[1]);switch(m[0]){case
10:var
s=m[1][1];if(2===f.length-1){var
b=j(f,0)[1],d=j(f,1)[2],g=L(s);if(h(g,cg)){if(h(g,ch)){if(h(g,ci)){if(h(g,cj))throw D;var
t=n(d);return[0,[0,e,ck,b,d,B(n(b),t),1],0]}var
u=n(d);return[0,[0,e,cl,b,d,B(n(b),u),0],0]}var
v=n(b);return[0,[0,e,cm,d,b,B(n(d),v),1],0]}var
w=n(b);return[0,[0,e,cn,d,b,B(n(d),w),0],0]}break;case
11:var
q=m[1][1];if(1-c(co[5],[2,[0,q[1],q[2]]],I[30]))throw D;var
x=j(f,0)[1],i=j(f,1)[2],k=j(f,2)[3],r=a(p[K],x);if(10===r[0]){if(h(L(r[1][1]),cp))throw D;var
y=n(i),z=[0,[0,e,cq,k,i,B(n(k),y),0],0],A=n(k);return[0,[0,e,cr,i,k,B(n(i),A),0],z]}throw D}throw D}throw D}function
aR(e){var
b=[0,-1],d=a(r[1],50);function
f(a){var
e=a[5][1];function
f(a,f){var
e=1-c(r[10],d,a);return e?(b[1]=b[1]+1|0,V(r[5],d,a,b[1])):e}return c(r[11],f,e)}c(l[11],f,e);function
g(e){var
f=o.caml_make_vect(b[1]+1|0,i[3]),g=e[5][1];function
h(e,b){var
a=c(r[7],d,e);return j(f,a)[a+1]=b}c(r[11],h,g);var
k=e[6],l=[0,a(i[6],e[5][2]),0],m=a(aq[11],f);return[0,c(G[22],m,l),k]}var
h=c(l[13],g,e);return a(i[25],h)}function
d(b){var
c=o.caml_obj_tag(b);return 250===c?b[1]:e===c?a(cs[2],b):b}var
ac=a(I[4],ct),aS=[e,function(b){return a(I[43],0)}],aT=[e,function(b){return a(I[50],0)}],aU=[e,function(b){return a(I[53],0)}],cu=[e,function(b){return a(I[41],0)}],u=a(ac,cv),aV=[e,function(b){return a(u,cw)}],cy=[e,function(b){return a(u,cx)}],aW=[e,function(b){return a(u,cz)}],cB=[e,function(b){return a(u,cA)}],cD=[e,function(b){return a(u,cC)}],aX=[e,function(b){return a(u,cE)}],N=[e,function(b){return a(u,cF)}],ad=[e,function(b){return a(u,cG)}],aY=[e,function(b){return a(u,cH)}],ar=[e,function(b){return a(u,cI)}],aZ=[e,function(b){return a(u,cJ)}],T=[e,function(b){return a(u,cK)}],a0=[e,function(a){return c(ac,cM,cL)}],k=a(ac,cN),ae=[e,function(b){return a(k,cO)}],af=[e,function(b){return a(k,cP)}],a1=[e,function(b){return a(k,cQ)}],a2=[e,function(b){return a(k,cR)}],a3=[e,function(b){return a(k,cS)}],a4=[e,function(b){return a(k,cT)}],a5=[e,function(b){return a(k,cU)}],a6=[e,function(b){return a(k,cV)}],a7=[e,function(b){return a(k,cW)}],a8=[e,function(b){return a(k,cX)}],a9=[e,function(b){return a(k,cY)}],a_=[e,function(b){return a(k,cZ)}],a$=[e,function(b){return a(k,c0)}],ba=[e,function(b){return a(k,c1)}],bb=[e,function(b){return a(k,c2)}],bc=[e,function(b){return a(k,c3)}],bd=[e,function(b){return a(k,c4)}],be=[e,function(b){return a(k,c5)}],bf=[e,function(b){return a(k,c6)}],bg=[e,function(b){return a(k,c7)}],bh=[e,function(b){return a(k,c8)}],bi=[e,function(b){return a(k,c9)}],bj=[e,function(b){return a(k,c_)}],bk=[e,function(b){return a(k,c$)}],bl=[e,function(b){return a(k,da)}],bm=[e,function(b){return a(k,db)}];function
dc(a){return 1===a[2]?1:0}function
x(a){return[0,a[1],a[2]]}function
as(e){var
f=a(G[6],e);if(0===f)return d(aZ);var
b=[0,d(T)],g=f-1|0,h=1;if(!(g<1)){var
c=h;for(;;){var
k=b[1],l=[0,d(T),k],m=[0,d(ad),l];b[1]=a(p[Q],m);var
n=c+1|0;if(g!==c){var
c=n;continue}break}}if(0<=e)return b[1];var
i=[0,b[1]],j=[0,d(aY),i];return a(p[Q],j)}function
O(c){var
b=x(c),e=b[1],f=[0,as(b[2])],g=[0,d(ar),f],h=a(p[Q],g),i=[0,as(e),h],j=[0,d(N),i];return a(p[Q],j)}function
E(D,j){var
m=j[2],n=j[1],o=d(ae),b=[0,a(f[85],o)],p=d(ae),e=[0,a(f[85],p)],k=n-1|0,q=1;if(!(k<1)){var
i=q;for(;;){var
z=b[1],A=d(af),B=a(f[85],A);b[1]=c(g[70][3],B,z);var
C=i+1|0;if(k!==i){var
i=C;continue}break}}var
l=m-1|0,r=1;if(!(l<1)){var
h=r;for(;;){var
v=e[1],w=d(af),x=a(f[85],w);e[1]=c(g[70][3],x,v);var
y=h+1|0;if(l!==h){var
h=y;continue}break}}var
s=[0,b[1],[0,e[1],0]],t=d(a2),u=a(f[85],t);return c(g[70][19],u,s)}function
bn(F,j){var
k=j[1],o=j[2];if(0===k)var
p=d(a3),l=a(f[85],p);else
var
E=d(a4),l=a(f[85],E);var
b=[0,l],q=d(ae),e=[0,a(f[85],q)],m=k-1|0,r=1;if(!(m<1)){var
i=r;for(;;){var
A=b[1],B=d(a1),C=a(f[85],B);b[1]=c(g[70][3],C,A);var
D=i+1|0;if(m!==i){var
i=D;continue}break}}var
n=o-1|0,s=1;if(!(n<1)){var
h=s;for(;;){var
w=e[1],x=d(af),y=a(f[85],x);e[1]=c(g[70][3],y,w);var
z=h+1|0;if(n!==h){var
h=z;continue}break}}var
t=[0,b[1],[0,e[1],0]],u=d(a5),v=a(f[85],u);return c(g[70][19],v,t)}function
bo(h,b){var
e=b[1],i=b[2];if(0===e){var
j=d(a6);return a(f[85],j)}var
k=bn(h,[0,-e|0,i]),l=d(a7),m=a(f[85],l);return c(g[70][3],m,k)}function
bp(e,b){var
h=E(e,[0,-b[1]|0,b[2]]),i=d(bm),j=a(f[85],i);return c(g[70][3],j,h)}var
bq=f[45];function
J(i){var
b=a(bq,i[1]),e=i[2];if(h(e,dd)){if(h(e,de)){if(h(e,df)){if(h(e,dg)){if(h(e,dh)){if(h(e,di))throw[0,R,dj];var
j=d(a$),k=a(f[85],j);return c(g[70][3],k,b)}var
l=d(a_),m=a(f[85],l);return c(g[70][3],m,b)}return b}return b}var
n=d(a8),o=a(f[85],n);return c(g[70][3],o,b)}var
p=d(a9),q=a(f[85],p);return c(g[70][3],q,b)}function
br(a){function
b(a){return 0===a[0]?[0,a[1],a[2]]:[0,a[1],a[3]]}return c(l[13],b,a)}function
y(c){var
b=a(aq[11],c),d=a(l[4],b),e=a(aq[12],d),f=[0,a(l[3],b),e];return a(p[Q],f)}var
ag=[Z,dk,W(0)];function
U(e){var
b=[0,function(b){var
Z=a(P[62][3],b);a(I[11],dl);var
_=a(p[99],Z),n=a(M[1][5],dm);try{var
Y=a(p[K],_);if(9===Y[0]){var
v=ab(Y[1]);if(h(v,dt))if(h(v,du))if(h(v,dv)){if(h(v,dw))throw ag;var
bw=U(0),bx=a(f[22],n),by=d(ba),bz=a(f[85],by),bA=c(g[70][3],bz,bx),w=c(g[70][3],bA,bw)}else
var
bB=U(0),bD=a(f[22],n),bE=d(bb),bF=a(f[85],bE),bH=c(g[70][3],bF,bD),w=c(g[70][3],bH,bB);else
var
bI=U(0),bJ=a(f[22],n),bK=d(bc),bL=a(f[85],bK),bM=c(g[70][3],bL,bJ),w=c(g[70][3],bM,bI);else
var
bN=U(0),bO=a(f[22],n),bP=d(bd),bQ=a(f[85],bP),bR=c(g[70][3],bQ,bO),w=c(g[70][3],bR,bN);return w}throw ag}catch(h){h=F(h);if(h===ag){var
$=br(a(P[62][4],b)),aa=function(b){var
c=b[2];return[0,a(p[bG],b[1]),c]},m=[0,0],ac=c(l[13],aa,$),ae=function(a){try{var
b=m[1],d=aQ(a);m[1]=c(G[22],d,b);var
e=0;return e}catch(a){a=F(a);if(a===D)return 0;throw a}};c(l[11],ae,ac);if(0===m[1])a(bs[6],dn);var
q=aR(m[1]),r=[0,a(P[13],0)];if(0===q)a(bs[6],dp);else{if(q)if(q[2])var
H=0;else{var
z=q[1],L=z[2],A=z[1],B=[0,0],af=c(l[40],m[1],z[3]),ah=function(a){var
b=a[2],c=o.caml_notequal(b,i[3]),d=a[1],e=c?(B[1]=[0,[0,d,b],B[1]],0):c;return e};c(l[11],ah,af);var
C=B[1];if(!C)throw[0,R,ds];var
S=C[2],V=C[1],s=V[2],j=V[1],k=[0,j[6]],ai=j[3],aj=O(s),t=[0,y([0,d(N),aj,ai])],ak=j[4],al=O(s),u=[0,y([0,d(N),al,ak])],am=function(b){var
c=b[2],a=b[1],e=k[1],f=e||a[6];k[1]=f;var
g=a[3],h=O(c),i=y([0,d(N),h,g]),j=t[1];t[1]=y([0,d(ad),j,i]);var
l=a[4],m=O(c),n=y([0,d(N),m,l]),o=u[1];u[1]=y([0,d(ad),o,n]);return 0};c(l[11],am,S);var
an=u[1],ao=t[1],ap=k[1]?d(aV):d(aW),W=y([0,ap,ao,an]),aq=O(A);if(j[6])var
as=[0,E(b,x(s)),0],at=[0,J(j),as],au=d(be),av=a(f[85],au),X=c(g[70][19],av,at);else
var
bn=[0,E(b,x(s)),0],bq=[0,J(j),bn],bu=d(bf),bv=a(f[85],bu),X=c(g[70][19],bv,bq);var
e=[0,X];k[1]=j[6];var
aw=function(j){var
i=j[2],h=j[1];if(k[1])if(h[6]){var
l=[0,E(b,x(i)),0],m=[0,J(h),l],n=[0,e[1],m],o=d(bg),p=a(f[85],o);e[1]=c(g[70][19],p,n)}else{var
s=[0,E(b,x(i)),0],t=[0,J(h),s],u=[0,e[1],t],v=d(bh),w=a(f[85],v);e[1]=c(g[70][19],w,u)}else
if(h[6]){var
y=[0,E(b,x(i)),0],z=[0,J(h),y],A=[0,e[1],z],B=d(bi),C=a(f[85],B);e[1]=c(g[70][19],C,A)}else{var
D=[0,E(b,x(i)),0],F=[0,J(h),D],G=[0,e[1],F],H=d(bj),I=a(f[85],H);e[1]=c(g[70][19],I,G)}var
q=k[1],r=q||h[6];k[1]=r;return 0};c(l[11],aw,S);var
ax=L?bo(b,x(A)):bp(b,x(A)),ay=[0,e[1],0],az=0,aA=0,aB=function(b){var
e=d(a0),h=a(f[85],e),i=a(f[85],b);return c(g[70][3],i,h)},aC=d(aS),aD=[0,c(g[70][57],aC,aB),aA],aE=a(P[13],0),aF=a(P[13],0),aG=[0,c(g[70][12],aF,aE),aD],aH=d(T),aI=[0,d(T)],aJ=[0,d(ar),aI],aK=a(p[Q],aJ),aL=c(bt[12],aK,aH),aM=[0,ax,[0,c(g[70][19],aL,aG),az]],aN=t[1],aO=u[1],aP=y([0,d(aX),aO,aN]),aY=c(bt[12],aP,aq),aZ=c(g[70][19],aY,aM),a1=L?d(bk):d(bl),a2=a(f[85],a1),a3=c(g[70][3],a2,aZ),a4=y([0,d(aU),W]),a5=a(f[52],a4),a6=[0,c(g[70][3],a5,a3),ay],a7=a(f[bC],W);r[1]=c(g[70][19],a7,a6);var
a8=[0,r[1],0],a9=a(dr[2],0),a_=[0,c(g[70][3],f[16],a9),a8],a$=d(aT),bm=a(f[bC],a$);r[1]=c(g[70][19],bm,a_);var
H=1}else
var
H=0;if(!H)throw[0,R,dq]}return r[1]}throw h}}];return a(P[62][9],b)}var
at=[0,r,t,aM,v,w,aN,aO,B,aP,L,ab,C,q,S,n,ce,D,aQ,aR,d,ac,aS,aT,aU,cu,u,aV,cy,aW,cB,cD,aX,N,ad,aY,ar,aZ,T,a0,k,ae,af,a1,a2,a3,a4,a5,a6,a7,a8,a9,a_,a$,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,dc,x,as,O,E,bn,bo,bp,bq,J,br,y,ag,U];ai(125,at,"Fourier_plugin.FourierR");a(bu[12],dx);function
dy(d){var
b=[28,[0,0,[31,dA[4],[0,[0,ah,dz],0],0]]],c=a(M[1][5],dB);return dE(bv[4],1,0,c,b)}var
dC=[0,function(c,b){return a(at[80],0)}];V(bv[9],0,[0,ah,dD],dC);c(bu[19],dy,ah);var
bw=[0,ah];ai(129,bw,"Fourier_plugin.G_fourier");ai(130,[0,i,at,bw],"Fourier_plugin");return});
