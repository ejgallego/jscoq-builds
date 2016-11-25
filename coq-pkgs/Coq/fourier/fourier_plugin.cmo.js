(function(dF){"use strict";var
bI=617,bC=145,bD="eqTLR",ax="Rmult",K=140,aA="R0",aB="Ropp",Y="Rle",az="Rminus",bB="fourier_plugin",Q=124,bA="eqTRL",X="Rlt",e=246,bH="Fourier",aj="fourier",$="Rge",bz="plugins/fourier/fourier.ml",bG=113,aw="plugins/fourier/fourierR.ml",_="Rgt",av="Rplus",Z=248,au="Rinv",by="R",bF="Reals",ay="R1",bE="Rdiv",o=dF.jsoo_runtime,j=o.caml_check_bound,bx=o.caml_div,W=o.caml_fresh_oo_id,s=o.caml_mul,b=o.caml_new_string,ai=o.caml_register_global,h=o.caml_string_notequal,F=o.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):o.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):o.caml_call_gen(a,[b,c])}function
V(a,b,c,d){return a.length==3?a(b,c,d):o.caml_call_gen(a,[b,c,d])}function
dE(a,b,c,d,e){return a.length==4?a(b,c,d,e):o.caml_call_gen(a,[b,c,d,e])}var
m=o.caml_get_global_data(),z=[0,0,1],aD=[0,1,1],ah=b(bB),R=m.Assert_failure,l=m.List,G=m.Pervasives,p=m.Term,f=m.Tactics,g=m.Tacticals,L=m.Proofview,I=m.Coqlib,M=m.Names,aN=m.CErrors,aM=m.Equality,aq=m.Array,bu=m.Tacenv,bv=m.Mltop,bS=[0,b(bz),176,9],bQ=[0,b(bz),147,9],bP=b("empty ineq"),bL=b("/"),bR=b("Fourier.Contradiction"),b0=b("not_of_constant"),b2=b(bE),b3=b(au),b4=b(az),b5=b(ax),b6=b(aB),b7=b(av),b8=b(aA),b9=b(ay),b$=b(bE),ca=b(au),cb=b(az),cc=b(ax),cd=b(aB),ce=b(av),cf=b(aA),cg=b(ay),dm=[0,b("Coq"),[0,b(aj),[0,b(bH),0]]],dn=b("new_hyp_for_fourier"),dt=b($),du=b(_),dv=b(Y),dw=b(X),dp=b("No inequalities"),dq=b("fourier failed"),ds=[0,b(aw),bI,11],dr=[0,b(aw),bI,30],de=b($),df=b(_),dg=b(Y),dh=b(X),di=b(bD),dj=b(bA),dk=[0,b(aw),426,6],dc=b("Rlt_not_le_frac_opp"),db=b("Rnot_le_le"),da=b("Rnot_lt_lt"),c$=b("Rfourier_le_le"),c_=b("Rfourier_le_lt"),c9=b("Rfourier_lt_le"),c8=b("Rfourier_lt_lt"),c7=b("Rfourier_le"),c6=b("Rfourier_lt"),c5=b("Rfourier_not_lt_ge"),c4=b("Rfourier_not_le_gt"),c3=b("Rfourier_not_gt_le"),c2=b("Rfourier_not_ge_lt"),c1=b("Rfourier_eqRL_to_le"),c0=b("Rfourier_eqLR_to_le"),cZ=b("Rfourier_ge_to_le"),cY=b("Rfourier_gt_to_lt"),cX=b("Rle_not_lt"),cW=b("Rnot_lt0"),cV=b("Rle_mult_inv_pos"),cU=b("Rle_zero_1"),cT=b("Rle_zero_zero"),cS=b("Rlt_mult_inv_pos"),cR=b("Rle_zero_pos_plus1"),cQ=b("Rlt_zero_pos_plus1"),cP=b("Rlt_zero_1"),cM=b("Rinv_1"),cN=[0,b(bF),[0,b("RIneq"),0]],cL=b(ay),cK=b(aA),cJ=b(au),cI=b(aB),cH=b(av),cG=b(ax),cF=b(az),cD=b(by),cB=b($),cA=b(Y),cy=b(_),cx=b(X),cj=b($),ck=b(_),cl=b(Y),cm=b(X),cn=b(X),co=b(Y),cp=b(_),cq=b($),cr=b(by),cs=b(bA),ct=b(bD),bY=b("Coq.Reals.Rdefinitions"),bZ=b("constant_not_of_R"),b1=b("FourierR.NoRational"),b_=b("FourierR.NoLinear"),ci=b("FourierR.NoIneq"),cu=b(bH),cw=[0,b(bF),[0,b("Rdefinitions"),0]],cO=[0,b(aj),[0,b("Fourier_util"),0]],dl=b("FourierR.GoalDone"),dA=b(aj),dB=b("fourierz"),dy=b(bB),dD=b(aj),bJ=m.Failure,bW=m.Contradiction,bX=m.CamlinternalLazy,bV=m.Globnames,bT=m.Not_found,bU=m.Hashtbl,dx=m.Loc;function
bK(b){a(G[29],b[1]);a(G[27],bL);return a(G[29],b[2])}function
aC(d,c){var
b=d,a=c;for(;;){if(0===a)return b;var
e=o.caml_mod(b,a),b=a,a=e;continue}}function
H(b){var
a=0<=b[2]?b:[0,-b[1]|0,-b[2]|0];if(0===a[1])return z;var
c=aC(a[1],a[2]),d=0<=c?c:-c|0,e=bx(a[2],d);return[0,bx(a[1],d),e]}function
aE(a){return H([0,-a[1]|0,a[2]])}function
ak(b,a){return H([0,s(b[1],a[2])+s(a[1],b[2])|0,s(b[2],a[2])])}function
bM(b,a){return H([0,s(b[1],a[2])-s(a[1],b[2])|0,s(b[2],a[2])])}function
al(b,a){return H([0,s(b[1],a[1]),s(b[2],a[2])])}function
bN(a){return H([0,a[2],a[1]])}function
bO(b,a){return H([0,s(b[1],a[2]),s(b[2],a[1])])}function
aa(b,a){return s(b[1],a[2])<s(a[1],b[2])?1:0}function
aF(b,a){return s(b[1],a[2])<=s(a[1],b[2])?1:0}function
A(b,a){a[1]=[0,b,a[1]];return 0}function
aG(a){var
b=[0,0],d=[0,0],e=[0,0];function
f(a){var
c=a[1];if(c){var
f=c[1];return aa(f,z)?A(a,d):aa(z,f)?A(a,b):A(a,e)}throw[0,bJ,bP]}c(l[11],f,a);return[0,d[1],[0,e[1],[0,b[1],0]]]}function
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
ap=[Z,bR,W(0)],i=[0,bK,aC,z,aD,H,aE,ak,bM,al,bN,bO,aa,aF,A,aG,aH,aI,am,an,ao,aJ,aK,aL,ap,function(a){var
b=aL(a);function
d(a){var
b=a[1];if(b)if(!b[2]){var
c=a[3],d=b[1],i=a[2],f=aa(d,z),g=f?1-c:f;if(g)var
e=g;else
var
h=aF(d,z),e=h?c:h;if(e)throw[0,ap,[0,[0,d,c,i],0]];return e}throw[0,R,bS]}try{c(l[11],d,b);var
e=0;return e}catch(a){a=F(a);if(a[1]===ap)return a[2];throw a}}];ai(110,i,"Fourier_plugin.Fourier");var
r=a(bU[18],[0,p[136],p[151]]);function
t(c){var
b=i[3];return[0,a(r[1],50),b]}function
aO(b,a){try{var
d=c(r[7],b[1],a);return d}catch(a){a=F(a);if(a===bT)return i[3];throw a}}function
v(a,b,d){var
e=aO(a,b),f=c(i[7],e,d);V(r[9],a[1],b,f);return a}function
w(a,b){var
d=c(i[7],a[2],b);return[0,a[1],d]}function
aP(b){var
a=i[4];return w(t(0),a)}function
aQ(d,b){var
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
aR(b,a){var
d=t(0),e=a[1];function
f(e,a){v(d,e,c(i[9],b,a));return 0}c(r[11],f,e);return w(d,c(i[9],b,a[2]))}function
N(d){var
b=a(M[bG],d),c=b[1];if(0===c[0]){var
e=b[3],f=c[1];if(o.caml_equal(b[2],M[5][6]))if(o.caml_string_equal(a(M[5][8],f),bY))return a(M[6][5],e)}return bZ}function
ab(d){var
c=d;for(;;){var
b=a(p[K],c);switch(b[0]){case
5:var
c=b[1];continue;case
10:return N(b[1][1]);default:return b0}}}var
C=[Z,b1,W(0)];function
q(k){var
f=k;for(;;){var
e=a(p[K],f);switch(e[0]){case
5:var
f=e[1];continue;case
9:var
b=e[2],d=ab(e[1]);if(h(d,b2)){if(h(d,b3)){if(h(d,b4)){if(h(d,b5)){if(h(d,b6)){if(h(d,b7))throw C;var
l=q(j(b,1)[2]),m=q(j(b,0)[1]);return c(i[7],m,l)}var
n=q(j(b,0)[1]);return a(i[6],n)}var
o=q(j(b,1)[2]),r=q(j(b,0)[1]);return c(i[9],r,o)}var
s=q(j(b,1)[2]),t=q(j(b,0)[1]);return c(i[8],t,s)}var
u=q(j(b,0)[1]);return a(i[10],u)}var
v=q(j(b,1)[2]),w=q(j(b,0)[1]);return c(i[11],w,v);case
10:var
g=N(e[1][1]);if(h(g,b8)){if(h(g,b9))throw C;return i[4]}return i[3];default:throw C}}}var
S=[Z,b_,W(0)];function
n(k){try{var
f=a(p[K],k);switch(f[0]){case
5:var
g=n(f[1]);break;case
9:var
b=f[2],e=ab(f[1]);if(h(e,b$))if(h(e,ca))if(h(e,cb))if(h(e,cc))if(h(e,cd)){if(h(e,ce))throw S;var
y=n(j(b,1)[2]),d=aQ(n(j(b,0)[1]),y)}else
var
z=n(j(b,0)[1]),d=aR(a(i[6],i[4]),z);else
try{var
l=q(j(b,0)[1]);try{var
G=q(j(b,1)[2]),H=c(i[9],l,G),I=w(t(0),H),m=I}catch(a){a=F(a);if(a!==C)throw a;var
E=j(b,1)[2],m=v(t(0),E,l)}var
d=m}catch(a){a=F(a);if(a!==C)throw a;var
A=q(j(b,1)[2]),D=j(b,0)[1],d=v(t(0),D,A)}else
var
J=n(j(b,1)[2]),d=B(n(j(b,0)[1]),J);else
var
L=q(j(b,0)[1]),M=a(i[10],L),d=w(t(0),M);else{var
o=q(j(b,1)[2]);try{var
Q=q(j(b,0)[1]),R=c(i[11],Q,o),T=w(t(0),R),r=T}catch(c){c=F(c);if(c!==C)throw c;var
O=a(i[10],o),P=j(b,0)[1],r=v(t(0),P,O)}var
d=r}var
g=d;break;case
10:var
s=N(f[1][1]);if(h(s,cf)){if(h(s,cg))throw S;var
u=aP(0)}else
var
u=t(0);var
g=u;break;default:throw S}return g}catch(a){a=F(a);if(a!==C)if(a!==S)throw a;var
x=i[4];return v(t(0),k,x)}}function
ch(b){var
a=[0,0];function
d(c,b){a[1]=[0,[0,b,c],a[1]];return 0}c(r[11],d,b);return a[1]}var
D=[Z,ci,W(0)];function
aS(o){var
e=o[1],l=a(p[K],o[2]);if(9===l[0]){var
f=l[2],m=a(p[K],l[1]);switch(m[0]){case
10:var
s=m[1][1];if(2===f.length-1){var
b=j(f,0)[1],d=j(f,1)[2],g=N(s);if(h(g,cj)){if(h(g,ck)){if(h(g,cl)){if(h(g,cm))throw D;var
t=n(d);return[0,[0,e,cn,b,d,B(n(b),t),1],0]}var
u=n(d);return[0,[0,e,co,b,d,B(n(b),u),0],0]}var
v=n(b);return[0,[0,e,cp,d,b,B(n(d),v),1],0]}var
w=n(b);return[0,[0,e,cq,d,b,B(n(d),w),0],0]}break;case
11:var
q=m[1][1];if(1-c(bV[5],[2,[0,q[1],q[2]]],I[30]))throw D;var
x=j(f,0)[1],i=j(f,1)[2],k=j(f,2)[3],r=a(p[K],x);if(10===r[0]){if(h(N(r[1][1]),cr))throw D;var
y=n(i),z=[0,[0,e,cs,k,i,B(n(k),y),0],0],A=n(k);return[0,[0,e,ct,i,k,B(n(i),A),0],z]}throw D}throw D}throw D}function
aT(e){var
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
c=o.caml_obj_tag(b);return 250===c?b[1]:e===c?a(bX[2],b):b}var
ac=a(I[4],cu),aU=[e,function(b){return a(I[43],0)}],aV=[e,function(b){return a(I[50],0)}],aW=[e,function(b){return a(I[53],0)}],cv=[e,function(b){return a(I[41],0)}],u=a(ac,cw),aX=[e,function(b){return a(u,cx)}],cz=[e,function(b){return a(u,cy)}],aY=[e,function(b){return a(u,cA)}],cC=[e,function(b){return a(u,cB)}],cE=[e,function(b){return a(u,cD)}],aZ=[e,function(b){return a(u,cF)}],O=[e,function(b){return a(u,cG)}],ad=[e,function(b){return a(u,cH)}],a0=[e,function(b){return a(u,cI)}],ar=[e,function(b){return a(u,cJ)}],a1=[e,function(b){return a(u,cK)}],T=[e,function(b){return a(u,cL)}],a2=[e,function(a){return c(ac,cN,cM)}],k=a(ac,cO),ae=[e,function(b){return a(k,cP)}],af=[e,function(b){return a(k,cQ)}],a3=[e,function(b){return a(k,cR)}],a4=[e,function(b){return a(k,cS)}],a5=[e,function(b){return a(k,cT)}],a6=[e,function(b){return a(k,cU)}],a7=[e,function(b){return a(k,cV)}],a8=[e,function(b){return a(k,cW)}],a9=[e,function(b){return a(k,cX)}],a_=[e,function(b){return a(k,cY)}],a$=[e,function(b){return a(k,cZ)}],ba=[e,function(b){return a(k,c0)}],bb=[e,function(b){return a(k,c1)}],bc=[e,function(b){return a(k,c2)}],bd=[e,function(b){return a(k,c3)}],be=[e,function(b){return a(k,c4)}],bf=[e,function(b){return a(k,c5)}],bg=[e,function(b){return a(k,c6)}],bh=[e,function(b){return a(k,c7)}],bi=[e,function(b){return a(k,c8)}],bj=[e,function(b){return a(k,c9)}],bk=[e,function(b){return a(k,c_)}],bl=[e,function(b){return a(k,c$)}],bm=[e,function(b){return a(k,da)}],bn=[e,function(b){return a(k,db)}],bo=[e,function(b){return a(k,dc)}];function
dd(a){return 1===a[2]?1:0}function
x(a){return[0,a[1],a[2]]}function
as(e){var
f=a(G[6],e);if(0===f)return d(a1);var
b=[0,d(T)],g=f-1|0,h=1;if(!(g<1)){var
c=h;for(;;){var
k=b[1],l=[0,d(T),k],m=[0,d(ad),l];b[1]=a(p[Q],m);var
n=c+1|0;if(g!==c){var
c=n;continue}break}}if(0<=e)return b[1];var
i=[0,b[1]],j=[0,d(a0),i];return a(p[Q],j)}function
P(c){var
b=x(c),e=b[1],f=[0,as(b[2])],g=[0,d(ar),f],h=a(p[Q],g),i=[0,as(e),h],j=[0,d(O),i];return a(p[Q],j)}function
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
s=[0,b[1],[0,e[1],0]],t=d(a4),u=a(f[85],t);return c(g[70][19],u,s)}function
bp(F,j){var
k=j[1],o=j[2];if(0===k)var
p=d(a5),l=a(f[85],p);else
var
E=d(a6),l=a(f[85],E);var
b=[0,l],q=d(ae),e=[0,a(f[85],q)],m=k-1|0,r=1;if(!(m<1)){var
i=r;for(;;){var
A=b[1],B=d(a3),C=a(f[85],B);b[1]=c(g[70][3],C,A);var
D=i+1|0;if(m!==i){var
i=D;continue}break}}var
n=o-1|0,s=1;if(!(n<1)){var
h=s;for(;;){var
w=e[1],x=d(af),y=a(f[85],x);e[1]=c(g[70][3],y,w);var
z=h+1|0;if(n!==h){var
h=z;continue}break}}var
t=[0,b[1],[0,e[1],0]],u=d(a7),v=a(f[85],u);return c(g[70][19],v,t)}function
bq(h,b){var
e=b[1],i=b[2];if(0===e){var
j=d(a8);return a(f[85],j)}var
k=bp(h,[0,-e|0,i]),l=d(a9),m=a(f[85],l);return c(g[70][3],m,k)}function
br(e,b){var
h=E(e,[0,-b[1]|0,b[2]]),i=d(bo),j=a(f[85],i);return c(g[70][3],j,h)}var
bs=f[45];function
J(i){var
b=a(bs,i[1]),e=i[2];if(h(e,de)){if(h(e,df)){if(h(e,dg)){if(h(e,dh)){if(h(e,di)){if(h(e,dj))throw[0,R,dk];var
j=d(bb),k=a(f[85],j);return c(g[70][3],k,b)}var
l=d(ba),m=a(f[85],l);return c(g[70][3],m,b)}return b}return b}var
n=d(a_),o=a(f[85],n);return c(g[70][3],o,b)}var
p=d(a$),q=a(f[85],p);return c(g[70][3],q,b)}function
bt(a){function
b(a){return 0===a[0]?[0,a[1],a[2]]:[0,a[1],a[3]]}return c(l[13],b,a)}function
y(c){var
b=a(aq[11],c),d=a(l[4],b),e=a(aq[12],d),f=[0,a(l[3],b),e];return a(p[Q],f)}var
ag=[Z,dl,W(0)];function
U(e){var
b=[0,function(b){var
Z=a(L[62][3],b);a(I[11],dm);var
_=a(p[99],Z),n=a(M[1][5],dn);try{var
Y=a(p[K],_);if(9===Y[0]){var
v=ab(Y[1]);if(h(v,dt))if(h(v,du))if(h(v,dv)){if(h(v,dw))throw ag;var
bw=U(0),bx=a(f[22],n),by=d(bc),bz=a(f[85],by),bA=c(g[70][3],bz,bx),w=c(g[70][3],bA,bw)}else
var
bB=U(0),bD=a(f[22],n),bE=d(bd),bF=a(f[85],bE),bH=c(g[70][3],bF,bD),w=c(g[70][3],bH,bB);else
var
bI=U(0),bJ=a(f[22],n),bK=d(be),bL=a(f[85],bK),bM=c(g[70][3],bL,bJ),w=c(g[70][3],bM,bI);else
var
bN=U(0),bO=a(f[22],n),bP=d(bf),bQ=a(f[85],bP),bR=c(g[70][3],bQ,bO),w=c(g[70][3],bR,bN);return w}throw ag}catch(h){h=F(h);if(h===ag){var
$=bt(a(L[62][4],b)),aa=function(b){var
c=b[2];return[0,a(p[bG],b[1]),c]},m=[0,0],ac=c(l[13],aa,$),ae=function(a){try{var
b=m[1],d=aS(a);m[1]=c(G[22],d,b);var
e=0;return e}catch(a){a=F(a);if(a===D)return 0;throw a}};c(l[11],ae,ac);if(0===m[1])a(aN[6],dp);var
q=aT(m[1]),r=[0,a(L[13],0)];if(0===q)a(aN[6],dq);else{if(q)if(q[2])var
H=0;else{var
z=q[1],N=z[2],A=z[1],B=[0,0],af=c(l[40],m[1],z[3]),ah=function(a){var
b=a[2],c=o.caml_notequal(b,i[3]),d=a[1],e=c?(B[1]=[0,[0,d,b],B[1]],0):c;return e};c(l[11],ah,af);var
C=B[1];if(!C)throw[0,R,ds];var
S=C[2],V=C[1],s=V[2],j=V[1],k=[0,j[6]],ai=j[3],aj=P(s),t=[0,y([0,d(O),aj,ai])],ak=j[4],al=P(s),u=[0,y([0,d(O),al,ak])],am=function(b){var
c=b[2],a=b[1],e=k[1],f=e||a[6];k[1]=f;var
g=a[3],h=P(c),i=y([0,d(O),h,g]),j=t[1];t[1]=y([0,d(ad),j,i]);var
l=a[4],m=P(c),n=y([0,d(O),m,l]),o=u[1];u[1]=y([0,d(ad),o,n]);return 0};c(l[11],am,S);var
an=u[1],ao=t[1],ap=k[1]?d(aX):d(aY),W=y([0,ap,ao,an]),aq=P(A);if(j[6])var
as=[0,E(b,x(s)),0],at=[0,J(j),as],au=d(bg),av=a(f[85],au),X=c(g[70][19],av,at);else
var
bp=[0,E(b,x(s)),0],bs=[0,J(j),bp],bu=d(bh),bv=a(f[85],bu),X=c(g[70][19],bv,bs);var
e=[0,X];k[1]=j[6];var
aw=function(j){var
i=j[2],h=j[1];if(k[1])if(h[6]){var
l=[0,E(b,x(i)),0],m=[0,J(h),l],n=[0,e[1],m],o=d(bi),p=a(f[85],o);e[1]=c(g[70][19],p,n)}else{var
s=[0,E(b,x(i)),0],t=[0,J(h),s],u=[0,e[1],t],v=d(bj),w=a(f[85],v);e[1]=c(g[70][19],w,u)}else
if(h[6]){var
y=[0,E(b,x(i)),0],z=[0,J(h),y],A=[0,e[1],z],B=d(bk),C=a(f[85],B);e[1]=c(g[70][19],C,A)}else{var
D=[0,E(b,x(i)),0],F=[0,J(h),D],G=[0,e[1],F],H=d(bl),I=a(f[85],H);e[1]=c(g[70][19],I,G)}var
q=k[1],r=q||h[6];k[1]=r;return 0};c(l[11],aw,S);var
ax=N?bq(b,x(A)):br(b,x(A)),ay=[0,e[1],0],az=0,aA=0,aB=function(b){var
e=d(a2),h=a(f[85],e),i=a(f[85],b);return c(g[70][3],i,h)},aC=d(aU),aD=[0,c(g[70][57],aC,aB),aA],aE=a(L[13],0),aF=a(L[13],0),aG=[0,c(g[70][12],aF,aE),aD],aH=d(T),aI=[0,d(T)],aJ=[0,d(ar),aI],aK=a(p[Q],aJ),aL=c(aM[12],aK,aH),aO=[0,ax,[0,c(g[70][19],aL,aG),az]],aP=t[1],aQ=u[1],aR=y([0,d(aZ),aQ,aP]),a0=c(aM[12],aR,aq),a1=c(g[70][19],a0,aO),a3=N?d(bm):d(bn),a4=a(f[85],a3),a5=c(g[70][3],a4,a1),a6=y([0,d(aW),W]),a7=a(f[52],a6),a8=[0,c(g[70][3],a7,a5),ay],a9=a(f[bC],W);r[1]=c(g[70][19],a9,a8);var
a_=[0,r[1],0],a$=a(bW[2],0),ba=[0,c(g[70][3],f[16],a$),a_],bb=d(aV),bo=a(f[bC],bb);r[1]=c(g[70][19],bo,ba);var
H=1}else
var
H=0;if(!H)throw[0,R,dr]}return r[1]}throw h}}];return a(L[62][9],b)}var
at=[0,r,t,aO,v,w,aP,aQ,B,aR,N,ab,C,q,S,n,ch,D,aS,aT,d,ac,aU,aV,aW,cv,u,aX,cz,aY,cC,cE,aZ,O,ad,a0,ar,a1,T,a2,k,ae,af,a3,a4,a5,a6,a7,a8,a9,a_,a$,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,dd,x,as,P,E,bp,bq,br,bs,J,bt,y,ag,U];ai(125,at,"Fourier_plugin.FourierR");a(bv[12],dy);function
dz(d){var
b=[28,[0,0,[31,dx[4],[0,[0,ah,dA],0],0]]],c=a(M[1][5],dB);return dE(bu[4],1,0,c,b)}var
dC=[0,function(c,b){return a(at[80],0)}];V(bu[9],0,[0,ah,dD],dC);c(bv[19],dz,ah);var
bw=[0,ah];ai(129,bw,"Fourier_plugin.G_fourier");ai(130,[0,i,at,bw],"Fourier_plugin");return});
