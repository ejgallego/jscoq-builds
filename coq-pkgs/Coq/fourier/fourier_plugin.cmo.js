(function(dM){"use strict";var
aF=123,bJ="eqTLR",aA="Rmult",aD="R0",aE="Ropp",$="Rle",aC="Rminus",bP=250,bI="eqTRL",bO=631,_="Rlt",T=119,d=246,bN="Fourier",am="fourier",ac="Rge",bH="plugins/fourier/fourier.ml",az="plugins/fourier/fourierR.ml",bM=144,ab="Rgt",ay="Rplus",O=135,aa=248,ax="Rinv",bG="R",bL="Reals",bF=147,aB="R1",bK="Rdiv",p=dM.jsoo_runtime,j=p.caml_check_bound,bD=p.caml_div,Z=p.caml_fresh_oo_id,t=p.caml_mul,b=p.caml_new_string,bE=p.caml_obj_tag,al=p.caml_register_global,h=p.caml_string_notequal,I=p.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):p.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):p.caml_call_gen(a,[b,c])}function
H(a,b,c,d){return a.length==3?a(b,c,d):p.caml_call_gen(a,[b,c,d])}function
dL(a,b,c,d,e){return a.length==4?a(b,c,d,e):p.caml_call_gen(a,[b,c,d,e])}var
m=p.caml_get_global_data(),B=[0,0,1],aH=[0,1,1],Y=b("fourier_plugin"),U=m.Assert_failure,l=m.List,J=m.Pervasives,s=m.Term,e=m.Tactics,f=m.Tacticals,w=m.EConstr,N=m.Proofview,L=m.Coqlib,Q=m.Names,bx=m.Pp,by=m.CErrors,bz=m.Equality,at=m.Array,ag=m.Universes,aW=m.CamlinternalLazy,bB=m.Ltac_plugin,bA=m.Mltop,bZ=[0,b(bH),176,9],bX=[0,b(bH),bF,9],bV=b("empty ineq"),bR=b("/"),bY=b("Fourier.Contradiction"),b4=b("not_of_constant"),b6=b(bK),b7=b(ax),b8=b(aC),b9=b(aA),b_=b(aE),b$=b(ay),ca=b(aD),cb=b(aB),cd=b(bK),ce=b(ax),cf=b(aC),cg=b(aA),ch=b(aE),ci=b(ay),cj=b(aD),ck=b(aB),dt=[0,b("Coq"),[0,b(am),[0,b(bN),0]]],dv=b("new_hyp_for_fourier"),dB=b(ac),dC=b(ab),dD=b($),dE=b(_),dw=b("No inequalities"),dx=b("fourier failed"),dA=[0,b(az),bO,11],dy=[0,b(az),bO,30],dj=b(ac),dk=b(ab),dl=b($),dm=b(_),dn=b(bJ),dp=b(bI),dq=[0,b(az),436,6],dh=b("Rlt_not_le_frac_opp"),dg=b("Rnot_le_le"),df=b("Rnot_lt_lt"),de=b("Rfourier_le_le"),dd=b("Rfourier_le_lt"),dc=b("Rfourier_lt_le"),db=b("Rfourier_lt_lt"),da=b("Rfourier_le"),c$=b("Rfourier_lt"),c_=b("Rfourier_not_lt_ge"),c9=b("Rfourier_not_le_gt"),c8=b("Rfourier_not_gt_le"),c7=b("Rfourier_not_ge_lt"),c6=b("Rfourier_eqRL_to_le"),c5=b("Rfourier_eqLR_to_le"),c4=b("Rfourier_ge_to_le"),c3=b("Rfourier_gt_to_lt"),c2=b("Rle_not_lt"),c1=b("Rnot_lt0"),c0=b("Rle_mult_inv_pos"),cZ=b("Rle_zero_1"),cY=b("Rle_zero_zero"),cX=b("Rlt_mult_inv_pos"),cW=b("Rle_zero_pos_plus1"),cV=b("Rlt_zero_pos_plus1"),cU=b("Rlt_zero_1"),cR=b("Rinv_1"),cS=[0,b(bL),[0,b("RIneq"),0]],cQ=b(aB),cP=b(aD),cO=b(ax),cN=b(aE),cM=b(ay),cL=b(aA),cK=b(aC),cI=b(bG),cG=b(ac),cF=b($),cD=b(ab),cC=b(_),cz=b(bN),cn=b(ac),co=b(ab),cp=b($),cq=b(_),cr=b(_),cs=b($),ct=b(ab),cu=b(ac),cw=b(bG),cx=b(bI),cy=b(bJ),b2=b("Coq.Reals.Rdefinitions"),b3=b("constant_not_of_R"),b5=b("FourierR.NoRational"),cc=b("FourierR.NoLinear"),cm=b("FourierR.NoIneq"),cB=[0,b(bL),[0,b("Rdefinitions"),0]],cT=[0,b(am),[0,b("Fourier_util"),0]],dr=b("FourierR.GoalDone"),dG=b(am),dI=b("fourierz"),dK=b(am),bW=m.Failure,ds=m.Tacmach,du=m.Termops,dz=m.Contradiction,cv=m.Globnames,b1=m.Not_found,b0=m.Hashtbl,dH=m.Loc;function
bQ(b){a(J[32],b[1]);a(J[30],bR);return a(J[32],b[2])}function
aG(d,c){var
b=d,a=c;for(;;){if(0===a)return b;var
e=p.caml_mod(b,a),b=a,a=e;continue}}function
K(b){var
a=0<=b[2]?b:[0,-b[1]|0,-b[2]|0];if(0===a[1])return B;var
c=aG(a[1],a[2]),d=0<=c?c:-c|0,e=bD(a[2],d);return[0,bD(a[1],d),e]}function
aI(a){return K([0,-a[1]|0,a[2]])}function
an(b,a){return K([0,t(b[1],a[2])+t(a[1],b[2])|0,t(b[2],a[2])])}function
bS(b,a){return K([0,t(b[1],a[2])-t(a[1],b[2])|0,t(b[2],a[2])])}function
ao(b,a){return K([0,t(b[1],a[1]),t(b[2],a[2])])}function
bT(a){return K([0,a[2],a[1]])}function
bU(b,a){return K([0,t(b[1],a[2]),t(b[2],a[1])])}function
ad(b,a){return t(b[1],a[2])<t(a[1],b[2])?1:0}function
aJ(b,a){return t(b[1],a[2])<=t(a[1],b[2])?1:0}function
C(b,a){a[1]=[0,b,a[1]];return 0}function
aK(a){var
b=[0,0],d=[0,0],e=[0,0];function
f(a){var
c=a[1];if(c){var
f=c[1];return ad(f,B)?C(a,d):ad(B,f)?C(a,b):C(a,e)}throw[0,bW,bV]}c(l[15],f,a);return[0,d[1],[0,e[1],[0,b[1],0]]]}function
aL(d){var
b=[0,0],h=a(l[1],d);function
e(e){var
a=[0,0],f=(h-b[1]|0)-1|0,i=e[2],j=e[1],k=1;if(!(f<1)){var
d=k;for(;;){C(B,a);var
n=d+1|0;if(f!==d){var
d=n;continue}break}}C(aH,a);var
g=b[1],l=1;if(!(g<1)){var
c=l;for(;;){C(B,a);var
m=c+1|0;if(g!==c){var
c=m;continue}break}}b[1]=b[1]+1|0;return[0,j,a[1],i]}return c(l[17],e,d)}function
aM(b,a){var
c=b[3],d=c||a[3],e=H(l[23],an,b[2],a[2]);return[0,H(l[23],an,b[1],a[1]),e,d]}function
ap(b,a){var
d=a[3],e=a[2];function
f(a){return ao(b,a)}var
g=c(l[17],f,e),h=a[1];function
i(a){return ao(b,a)}return[0,c(l[17],i,h),g,d]}function
aq(b){var
c=b[3],d=b[2];return[0,a(l[6],b[1]),d,c]}function
ar(b){return a(l[5],b[1])}function
aN(b,d){var
a=[0,0];function
e(b){function
e(c){var
d=aI(ar(b)),e=ar(c),f=ap(d,c);return C(aq(aM(ap(e,b),f)),a)}return c(l[15],e,d)}c(l[15],e,b);return a[1]}function
aO(e){var
a=aK(e);if(a){var
b=a[2];if(b){var
d=b[2];if(d)if(!d[2]){var
f=b[1],g=aN(a[1],d[1]),h=c(l[17],aq,f);return c(J[25],h,g)}}}throw[0,U,bX]}function
aP(d){var
f=a(l[5],d)[1],g=a(l[1],f),b=[0,aL(d)],e=g-1|0,h=1;if(!(e<1)){var
c=h;for(;;){b[1]=aO(b[1]);var
i=c+1|0;if(e!==c){var
c=i;continue}break}}return b[1]}var
as=[aa,bY,Z(0)],i=[0,bQ,aG,B,aH,K,aI,an,bS,ao,bT,bU,ad,aJ,C,aK,aL,aM,ap,aq,ar,aN,aO,aP,as,function(a){var
b=aP(a);function
d(a){var
b=a[1];if(b)if(!b[2]){var
c=a[3],d=b[1],i=a[2],f=ad(d,B),g=f?1-c:f;if(g)var
e=g;else
var
h=aJ(d,B),e=h?c:h;if(e)throw[0,as,[0,[0,d,c,i],0]];return e}throw[0,U,bZ]}try{c(l[15],d,b);var
e=0;return e}catch(a){a=I(a);if(a[1]===as)return a[2];throw a}}];al(109,i,"Fourier_plugin.Fourier");var
r=a(b0[19],[0,s[131],s[bF]]);function
u(c){var
b=i[3];return[0,a(r[1],50),b]}function
aQ(b,a){try{var
d=c(r[7],b[1],a);return d}catch(a){a=I(a);if(a===b1)return i[3];throw a}}function
x(a,b,d){var
e=aQ(a,b),f=c(i[7],e,d);H(r[10],a[1],b,f);return a}function
y(a,b){var
d=c(i[7],a[2],b);return[0,a[1],d]}function
aR(b){var
a=i[4];return y(u(0),a)}function
aS(d,b){var
a=u(0),e=d[1];function
f(c,b){x(a,c,b);return 0}c(r[12],f,e);var
g=b[1];function
h(c,b){x(a,c,b);return 0}c(r[12],h,g);var
i=b[2];return y(y(a,d[2]),i)}function
D(e,d){var
b=u(0),f=e[1];function
g(c,a){x(b,c,a);return 0}c(r[12],g,f);var
h=d[1];function
j(d,c){x(b,d,a(i[6],c));return 0}c(r[12],j,h);var
k=a(i[6],d[2]);return y(y(b,e[2]),k)}function
aT(b,a){var
d=u(0),e=a[1];function
f(e,a){x(d,e,c(i[9],b,a));return 0}c(r[12],f,e);return y(d,c(i[9],b,a[2]))}function
P(d){var
b=a(Q[17][7],d),c=b[1];if(0===c[0]){var
e=b[3],f=c[1];if(p.caml_equal(b[2],Q[5][6]))if(p.caml_string_equal(a(Q[5][8],f),b2))return a(Q[6][5],e)}return b3}function
ae(d){var
c=d;for(;;){var
b=a(s[O],c);switch(b[0]){case
5:var
c=b[1];continue;case
10:return P(b[1][1]);default:return b4}}}var
E=[aa,b5,Z(0)];function
q(k){var
f=k;for(;;){var
e=a(s[O],f);switch(e[0]){case
5:var
f=e[1];continue;case
9:var
b=e[2],d=ae(e[1]);if(h(d,b6)){if(h(d,b7)){if(h(d,b8)){if(h(d,b9)){if(h(d,b_)){if(h(d,b$))throw E;var
l=q(j(b,1)[2]),m=q(j(b,0)[1]);return c(i[7],m,l)}var
n=q(j(b,0)[1]);return a(i[6],n)}var
o=q(j(b,1)[2]),p=q(j(b,0)[1]);return c(i[9],p,o)}var
r=q(j(b,1)[2]),t=q(j(b,0)[1]);return c(i[8],t,r)}var
u=q(j(b,0)[1]);return a(i[10],u)}var
v=q(j(b,1)[2]),w=q(j(b,0)[1]);return c(i[11],w,v);case
10:var
g=P(e[1][1]);if(h(g,ca)){if(h(g,cb))throw E;return i[4]}return i[3];default:throw E}}}var
V=[aa,cc,Z(0)];function
o(k){try{var
f=a(s[O],k);switch(f[0]){case
5:var
g=o(f[1]);break;case
9:var
b=f[2],e=ae(f[1]);if(h(e,cd))if(h(e,ce))if(h(e,cf))if(h(e,cg))if(h(e,ch)){if(h(e,ci))throw V;var
w=o(j(b,1)[2]),d=aS(o(j(b,0)[1]),w)}else
var
z=o(j(b,0)[1]),d=aT(a(i[6],i[4]),z);else
try{var
l=q(j(b,0)[1]);try{var
F=q(j(b,1)[2]),G=c(i[9],l,F),H=y(u(0),G),m=H}catch(a){a=I(a);if(a!==E)throw a;var
C=j(b,1)[2],m=x(u(0),C,l)}var
d=m}catch(a){a=I(a);if(a!==E)throw a;var
A=q(j(b,1)[2]),B=j(b,0)[1],d=x(u(0),B,A)}else
var
J=o(j(b,1)[2]),d=D(o(j(b,0)[1]),J);else
var
K=q(j(b,0)[1]),L=a(i[10],K),d=y(u(0),L);else{var
n=q(j(b,1)[2]);try{var
Q=q(j(b,0)[1]),R=c(i[11],Q,n),S=y(u(0),R),p=S}catch(c){c=I(c);if(c!==E)throw c;var
M=a(i[10],n),N=j(b,0)[1],p=x(u(0),N,M)}var
d=p}var
g=d;break;case
10:var
r=P(f[1][1]);if(h(r,cj)){if(h(r,ck))throw V;var
t=aR(0)}else
var
t=u(0);var
g=t;break;default:throw V}return g}catch(a){a=I(a);if(a!==E)if(a!==V)throw a;var
v=i[4];return x(u(0),k,v)}}function
cl(b){var
a=[0,0];function
d(c,b){a[1]=[0,[0,b,c],a[1]];return 0}c(r[12],d,b);return a[1]}var
F=[aa,cm,Z(0)];function
aU(n){var
r=n[2],e=a(w[aF][1],n[1]),t=a(w[aF][1],r),l=a(s[O],t);if(9===l[0]){var
f=l[2],m=a(s[O],l[1]);switch(m[0]){case
10:var
u=m[1][1];if(2===f.length-1){var
b=j(f,0)[1],d=j(f,1)[2],g=P(u);if(h(g,cn)){if(h(g,co)){if(h(g,cp)){if(h(g,cq))throw F;var
v=o(d);return[0,[0,e,cr,b,d,D(o(b),v),1],0]}var
x=o(d);return[0,[0,e,cs,b,d,D(o(b),x),0],0]}var
y=o(b);return[0,[0,e,ct,d,b,D(o(d),y),1],0]}var
z=o(b);return[0,[0,e,cu,d,b,D(o(d),z),0],0]}break;case
11:var
p=m[1][1];if(1-c(cv[5],[2,[0,p[1],p[2]]],L[28]))throw F;var
A=j(f,0)[1],i=j(f,1)[2],k=j(f,2)[3],q=a(s[O],A);if(10===q[0]){if(h(P(q[1][1]),cw))throw F;var
B=o(i),C=[0,[0,e,cx,k,i,D(o(k),B),0],0],E=o(k);return[0,[0,e,cy,i,k,D(o(i),E),0],C]}throw F}throw F}throw F}function
aV(e){var
b=[0,-1],d=a(r[1],50);function
f(a){var
e=a[5][1];function
f(a,f){var
e=1-c(r[11],d,a);return e?(b[1]=b[1]+1|0,H(r[5],d,a,b[1])):e}return c(r[12],f,e)}c(l[15],f,e);function
g(e){var
f=p.caml_make_vect(b[1]+1|0,i[3]),g=e[5][1];function
h(e,b){var
a=c(r[7],d,e);return j(f,a)[a+1]=b}c(r[12],h,g);var
k=e[6],l=[0,a(i[6],e[5][2]),0],m=a(at[11],f);return[0,c(J[25],m,l),k]}var
h=c(l[17],g,e);return a(i[25],h)}function
n(b){var
c=bE(b);return bP===c?b[1]:d===c?a(aW[2],b):b}function
g(b){var
c=bE(b),e=bP===c?b[1]:d===c?a(aW[2],b):b;return a(w[8],e)}function
af(c,b){var
d=H(L[2],cz,c,b);return a(ag[45],d)}var
aX=[d,function(b){return a(L[41],0)}],aY=[d,function(c){var
b=a(L[48],0);return a(ag[45],b)}],aZ=[d,function(c){var
b=a(L[51],0);return a(ag[45],b)}],cA=[d,function(c){var
b=a(L[39],0);return a(ag[45],b)}];function
v(a){return af(cB,a)}var
a0=[d,function(a){return v(cC)}],cE=[d,function(a){return v(cD)}],a1=[d,function(a){return v(cF)}],cH=[d,function(a){return v(cG)}],cJ=[d,function(a){return v(cI)}],a2=[d,function(a){return v(cK)}],R=[d,function(a){return v(cL)}],ah=[d,function(a){return v(cM)}],a3=[d,function(a){return v(cN)}],au=[d,function(a){return v(cO)}],a4=[d,function(a){return v(cP)}],W=[d,function(a){return v(cQ)}],a5=[d,function(a){return af(cS,cR)}];function
k(a){return af(cT,a)}var
ai=[d,function(a){return k(cU)}],aj=[d,function(a){return k(cV)}],a6=[d,function(a){return k(cW)}],a7=[d,function(a){return k(cX)}],a8=[d,function(a){return k(cY)}],a9=[d,function(a){return k(cZ)}],a_=[d,function(a){return k(c0)}],a$=[d,function(a){return k(c1)}],ba=[d,function(a){return k(c2)}],bb=[d,function(a){return k(c3)}],bc=[d,function(a){return k(c4)}],bd=[d,function(a){return k(c5)}],be=[d,function(a){return k(c6)}],bf=[d,function(a){return k(c7)}],bg=[d,function(a){return k(c8)}],bh=[d,function(a){return k(c9)}],bi=[d,function(a){return k(c_)}],bj=[d,function(a){return k(c$)}],bk=[d,function(a){return k(da)}],bl=[d,function(a){return k(db)}],bm=[d,function(a){return k(dc)}],bn=[d,function(a){return k(dd)}],bo=[d,function(a){return k(de)}],bp=[d,function(a){return k(df)}],bq=[d,function(a){return k(dg)}],br=[d,function(a){return k(dh)}];function
di(a){return 1===a[2]?1:0}function
z(a){return[0,a[1],a[2]]}function
av(d){var
e=a(J[6],d);if(0===e)return n(a4);var
b=[0,n(W)],f=e-1|0,g=1;if(!(f<1)){var
c=g;for(;;){var
j=b[1],k=[0,n(W),j],l=[0,n(ah),k];b[1]=a(s[T],l);var
m=c+1|0;if(f!==c){var
c=m;continue}break}}if(0<=d)return b[1];var
h=[0,b[1]],i=[0,n(a3),h];return a(s[T],i)}function
S(c){var
b=z(c),d=b[1],e=[0,av(b[2])],f=[0,n(au),e],g=a(s[T],f),h=[0,av(d),g],i=[0,n(R),h];return a(s[T],i)}function
G(D,j){var
m=j[2],n=j[1],o=g(ai),b=[0,a(e[85],o)],p=g(ai),d=[0,a(e[85],p)],k=n-1|0,q=1;if(!(k<1)){var
i=q;for(;;){var
z=b[1],A=g(aj),B=a(e[85],A);b[1]=c(f[66][3],B,z);var
C=i+1|0;if(k!==i){var
i=C;continue}break}}var
l=m-1|0,r=1;if(!(l<1)){var
h=r;for(;;){var
v=d[1],w=g(aj),x=a(e[85],w);d[1]=c(f[66][3],x,v);var
y=h+1|0;if(l!==h){var
h=y;continue}break}}var
s=[0,b[1],[0,d[1],0]],t=g(a7),u=a(e[85],t);return c(f[66][19],u,s)}function
bs(F,j){var
k=j[1],o=j[2];if(0===k)var
p=g(a8),l=a(e[85],p);else
var
E=g(a9),l=a(e[85],E);var
b=[0,l],q=g(ai),d=[0,a(e[85],q)],m=k-1|0,r=1;if(!(m<1)){var
i=r;for(;;){var
A=b[1],B=g(a6),C=a(e[85],B);b[1]=c(f[66][3],C,A);var
D=i+1|0;if(m!==i){var
i=D;continue}break}}var
n=o-1|0,s=1;if(!(n<1)){var
h=s;for(;;){var
w=d[1],x=g(aj),y=a(e[85],x);d[1]=c(f[66][3],y,w);var
z=h+1|0;if(n!==h){var
h=z;continue}break}}var
t=[0,b[1],[0,d[1],0]],u=g(a_),v=a(e[85],u);return c(f[66][19],v,t)}function
bt(h,b){var
d=b[1],i=b[2];if(0===d){var
j=g(a$);return a(e[85],j)}var
k=bs(h,[0,-d|0,i]),l=g(ba),m=a(e[85],l);return c(f[66][3],m,k)}function
bu(d,b){var
h=G(d,[0,-b[1]|0,b[2]]),i=g(br),j=a(e[85],i);return c(f[66][3],j,h)}var
bv=e[45];function
M(i){var
b=a(bv,a(w[8],i[1])),d=i[2];if(h(d,dj)){if(h(d,dk)){if(h(d,dl)){if(h(d,dm)){if(h(d,dn)){if(h(d,dp))throw[0,U,dq];var
j=g(be),k=a(e[85],j);return c(f[66][3],k,b)}var
l=g(bd),m=a(e[85],l);return c(f[66][3],m,b)}return b}return b}var
n=g(bb),o=a(e[85],n);return c(f[66][3],o,b)}var
p=g(bc),q=a(e[85],p);return c(f[66][3],q,b)}function
bw(a){function
b(a){return 0===a[0]?[0,a[1],a[2]]:[0,a[1],a[3]]}return c(l[17],b,a)}function
A(c){var
b=a(at[11],c),d=a(l[6],b),e=a(at[12],d),f=[0,a(l[5],b),e];return a(s[T],f)}var
ak=[aa,dr,Z(0)];function
X(d){function
b(b){var
aa=a(N[63][3],b),ab=a(ds[48][4],b);a(L[3],dt);var
ac=c(du[61],ab,aa),ad=a(w[aF][1],ac),o=a(Q[1][6],dv);try{var
$=a(s[O],ad);if(9===$[0]){var
x=ae($[1]);if(h(x,dB))if(h(x,dC))if(h(x,dD)){if(h(x,dE))throw ak;var
bL=X(0),bN=a(e[22],o),bO=g(bf),bP=a(e[85],bO),bQ=c(f[66][3],bP,bN),y=c(f[66][3],bQ,bL)}else
var
bR=X(0),bS=a(e[22],o),bT=g(bg),bU=a(e[85],bT),bV=c(f[66][3],bU,bS),y=c(f[66][3],bV,bR);else
var
bW=X(0),bX=a(e[22],o),bY=g(bh),bZ=a(e[85],bY),b0=c(f[66][3],bZ,bX),y=c(f[66][3],b0,bW);else
var
b1=X(0),b2=a(e[22],o),b3=g(bi),b4=a(e[85],b3),b5=c(f[66][3],b4,b2),y=c(f[66][3],b5,b1);return y}throw ak}catch(h){h=I(h);if(h===ak){var
af=bw(a(N[63][4],b)),ag=function(b){var
c=b[2];return[0,a(w[10],b[1]),c]},m=[0,0],ai=c(l[17],ag,af),aj=function(a){try{var
b=m[1],d=aU(a);m[1]=c(J[25],d,b);var
e=0;return e}catch(a){a=I(a);if(a===F)return 0;throw a}};c(l[15],aj,ai);if(0===m[1]){var
al=a(bx[3],dw);H(by[6],0,0,al)}var
q=aV(m[1]),r=[0,a(N[13],0)];if(0===q){var
am=a(bx[3],dx);H(by[6],0,0,am)}else{if(q)if(q[2])var
K=0;else{var
B=q[1],P=B[2],C=B[1],D=[0,0],an=c(l[47],m[1],B[3]),ao=function(a){var
b=a[2],c=p.caml_notequal(b,i[3]),d=a[1],e=c?(D[1]=[0,[0,d,b],D[1]],0):c;return e};c(l[15],ao,an);var
E=D[1];if(!E)throw[0,U,dA];var
V=E[2],Y=E[1],t=Y[2],j=Y[1],k=[0,j[6]],ap=j[3],aq=S(t),u=[0,A([0,n(R),aq,ap])],ar=j[4],as=S(t),v=[0,A([0,n(R),as,ar])],at=function(b){var
c=b[2],a=b[1],d=k[1],e=d||a[6];k[1]=e;var
f=a[3],g=S(c),h=A([0,n(R),g,f]),i=u[1];u[1]=A([0,n(ah),i,h]);var
j=a[4],l=S(c),m=A([0,n(R),l,j]),o=v[1];v[1]=A([0,n(ah),o,m]);return 0};c(l[15],at,V);var
av=v[1],aw=u[1],ax=k[1]?n(a0):n(a1),Z=A([0,ax,aw,av]),ay=S(C);if(j[6])var
az=[0,G(b,z(t)),0],aA=[0,M(j),az],aB=g(bj),aC=a(e[85],aB),_=c(f[66][19],aC,aA);else
var
bH=[0,G(b,z(t)),0],bI=[0,M(j),bH],bJ=g(bk),bK=a(e[85],bJ),_=c(f[66][19],bK,bI);var
d=[0,_];k[1]=j[6];var
aD=function(j){var
i=j[2],h=j[1];if(k[1])if(h[6]){var
l=[0,G(b,z(i)),0],m=[0,M(h),l],n=[0,d[1],m],o=g(bl),p=a(e[85],o);d[1]=c(f[66][19],p,n)}else{var
s=[0,G(b,z(i)),0],t=[0,M(h),s],u=[0,d[1],t],v=g(bm),w=a(e[85],v);d[1]=c(f[66][19],w,u)}else
if(h[6]){var
x=[0,G(b,z(i)),0],y=[0,M(h),x],A=[0,d[1],y],B=g(bn),C=a(e[85],B);d[1]=c(f[66][19],C,A)}else{var
D=[0,G(b,z(i)),0],E=[0,M(h),D],F=[0,d[1],E],H=g(bo),I=a(e[85],H);d[1]=c(f[66][19],I,F)}var
q=k[1],r=q||h[6];k[1]=r;return 0};c(l[15],aD,V);var
aE=P?bt(b,z(C)):bu(b,z(C)),aG=[0,d[1],0],aH=0,aI=0,aJ=function(b){var
d=g(a5),h=a(e[85],d),i=a(e[85],b);return c(f[66][3],i,h)},aK=n(aX),aL=a(f[66][58],aK),aM=[0,c(N[68][1],aL,aJ),aI],aN=a(N[13],0),aO=a(N[13],0),aP=[0,c(f[66][12],aO,aN),aM],aQ=g(W),aR=[0,n(W)],aS=[0,n(au),aR],aT=a(s[T],aS),aW=a(w[8],aT),a3=c(bz[12],aW,aQ),a4=[0,aE,[0,c(f[66][19],a3,aP),aH]],a6=a(w[8],ay),a7=u[1],a8=v[1],a9=A([0,n(a2),a8,a7]),a_=a(w[8],a9),a$=c(bz[12],a_,a6),ba=c(f[66][19],a$,a4),bb=P?g(bp):g(bq),bc=a(e[85],bb),bd=c(f[66][3],bc,ba),be=A([0,n(aZ),Z]),br=a(w[8],be),bs=a(e[52],br),bv=[0,c(f[66][3],bs,bd),aG],bA=a(w[8],Z),bB=a(e[bM],bA);r[1]=c(f[66][19],bB,bv);var
bC=[0,r[1],0],bD=a(dz[2],0),bE=[0,c(f[66][3],e[16],bD),bC],bF=g(aY),bG=a(e[bM],bF);r[1]=c(f[66][19],bG,bE);var
K=1}else
var
K=0;if(!K)throw[0,U,dy]}return r[1]}throw h}}return a(N[63][8],b)}var
aw=[0,r,u,aQ,x,y,aR,aS,D,aT,P,ae,E,q,V,o,cl,F,aU,aV,n,n,g,af,aX,aY,aZ,cA,v,a0,cE,a1,cH,cJ,a2,R,ah,a3,au,a4,W,a5,k,ai,aj,a6,a7,a8,a9,a_,a$,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,di,z,av,S,G,bs,bt,bu,bv,M,bw,A,ak,X];al(129,aw,"Fourier_plugin.FourierR");a(bA[12],Y);function
dF(e){var
b=[28,[0,0,[31,c(dH[10],0,[0,[0,[0,Y,dG],0],0])]]],d=a(Q[1][6],dI);return dL(bB[6][4],1,0,d,b)}var
dJ=[0,function(c,b){return a(aw[82],0)}];H(bB[6][9],0,[0,Y,dK],dJ);c(bA[19],dF,Y);var
bC=[0,Y];al(133,bC,"Fourier_plugin.G_fourier");al(134,[0,i,aw,bC],"Fourier_plugin");return});
