function(dH){"use strict";var
bE="eqTLR",az="Rmult",aC="R0",aD="Ropp",Y="Rle",aB="Rminus",bK=250,bD="eqTRL",ay=127,X="Rlt",d=246,bJ="Fourier",ax="fourier",$="Rge",bC="plugins/fourier/fourier.ml",bI=629,aw="plugins/fourier/fourierR.ml",_="Rgt",av="Rplus",bH=143,Z=248,au="Rinv",bB="R",bG="Reals",aA="R1",bF="Rdiv",p=dH.jsoo_runtime,j=p.caml_check_bound,bz=p.caml_div,W=p.caml_fresh_oo_id,t=p.caml_mul,b=p.caml_new_string,bA=p.caml_obj_tag,ai=p.caml_register_global,h=p.caml_string_notequal,H=p.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):p.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):p.caml_call_gen(a,[b,c])}function
N(a,b,c,d){return a.length==3?a(b,c,d):p.caml_call_gen(a,[b,c,d])}function
dG(a,b,c,d,e){return a.length==4?a(b,c,d,e):p.caml_call_gen(a,[b,c,d,e])}var
m=p.caml_get_global_data(),B=[0,0,1],aF=[0,1,1],at=b("fourier_plugin"),R=m.Assert_failure,l=m.List,I=m.Pervasives,s=m.Constr,e=m.Tactics,f=m.Tacticals,w=m.EConstr,M=m.Proofview,K=m.Coqlib,S=m.Names,bv=m.Pp,bw=m.CErrors,bx=m.Equality,ap=m.Array,ad=m.Universes,aU=m.CamlinternalLazy,bU=[0,b(bC),178,9],bS=[0,b(bC),149,9],bQ=b("empty ineq"),bM=b("/"),bT=b("Fourier.Contradiction"),bZ=b("not_of_constant"),b1=b(bF),b2=b(au),b3=b(aB),b4=b(az),b5=b(aD),b6=b(av),b7=b(aC),b8=b(aA),b_=b(bF),b$=b(au),ca=b(aB),cb=b(az),cc=b(aD),cd=b(av),ce=b(aC),cf=b(aA),dn=[0,b("Coq"),[0,b(ax),[0,b(bJ),0]]],dq=b("new_hyp_for_fourier"),dw=b($),dx=b(_),dy=b(Y),dz=b(X),dr=b("No inequalities"),ds=b("fourier failed"),dv=[0,b(aw),bI,11],dt=[0,b(aw),bI,30],de=b($),df=b(_),dg=b(Y),dh=b(X),di=b(bE),dj=b(bD),dk=[0,b(aw),434,6],dc=b("Rlt_not_le_frac_opp"),db=b("Rnot_le_le"),da=b("Rnot_lt_lt"),c$=b("Rfourier_le_le"),c_=b("Rfourier_le_lt"),c9=b("Rfourier_lt_le"),c8=b("Rfourier_lt_lt"),c7=b("Rfourier_le"),c6=b("Rfourier_lt"),c5=b("Rfourier_not_lt_ge"),c4=b("Rfourier_not_le_gt"),c3=b("Rfourier_not_gt_le"),c2=b("Rfourier_not_ge_lt"),c1=b("Rfourier_eqRL_to_le"),c0=b("Rfourier_eqLR_to_le"),cZ=b("Rfourier_ge_to_le"),cY=b("Rfourier_gt_to_lt"),cX=b("Rle_not_lt"),cW=b("Rnot_lt0"),cV=b("Rle_mult_inv_pos"),cU=b("Rle_zero_1"),cT=b("Rle_zero_zero"),cS=b("Rlt_mult_inv_pos"),cR=b("Rle_zero_pos_plus1"),cQ=b("Rlt_zero_pos_plus1"),cP=b("Rlt_zero_1"),cM=b("Rinv_1"),cN=[0,b(bG),[0,b("RIneq"),0]],cL=b(aA),cK=b(aC),cJ=b(au),cI=b(aD),cH=b(av),cG=b(az),cF=b(aB),cD=b(bB),cB=b($),cA=b(Y),cy=b(_),cx=b(X),cu=b(bJ),ci=b($),cj=b(_),ck=b(Y),cl=b(X),cm=b(X),cn=b(Y),co=b(_),cp=b($),cr=b(bB),cs=b(bD),ct=b(bE),bX=b("Coq.Reals.Rdefinitions"),bY=b("constant_not_of_R"),b0=b("FourierR.NoRational"),b9=b("FourierR.NoLinear"),ch=b("FourierR.NoIneq"),cw=[0,b(bG),[0,b("Rdefinitions"),0]],cO=[0,b(ax),[0,b("Fourier_util"),0]],dl=b("FourierR.GoalDone"),dC=[0,b("fourierz"),0],dE=b(ax),bR=m.Failure,dm=m.Tacmach,dp=m.Termops,du=m.Contradiction,cq=m.Globnames,bW=m.Not_found,bV=m.Hashtbl,dA=m.Mltop,dF=m.Ltac_plugin;function
bL(b){a(I[32],b[1]);a(I[30],bM);return a(I[32],b[2])}function
aE(d,c){var
b=d,a=c;for(;;){if(0===a)return b;var
e=p.caml_mod(b,a),b=a,a=e;continue}}function
J(b){var
a=0<=b[2]?b:[0,-b[1]|0,-b[2]|0];if(0===a[1])return B;var
c=aE(a[1],a[2]),d=0<=c?c:-c|0,e=bz(a[2],d);return[0,bz(a[1],d),e]}function
aG(a){return J([0,-a[1]|0,a[2]])}function
aj(b,a){return J([0,t(b[1],a[2])+t(a[1],b[2])|0,t(b[2],a[2])])}function
bN(b,a){return J([0,t(b[1],a[2])-t(a[1],b[2])|0,t(b[2],a[2])])}function
ak(b,a){return J([0,t(b[1],a[1]),t(b[2],a[2])])}function
bO(a){return J([0,a[2],a[1]])}function
bP(b,a){return J([0,t(b[1],a[2]),t(b[2],a[1])])}function
aa(b,a){return t(b[1],a[2])<t(a[1],b[2])?1:0}function
aH(b,a){return t(b[1],a[2])<=t(a[1],b[2])?1:0}function
C(b,a){a[1]=[0,b,a[1]];return 0}function
aI(a){var
b=[0,0],d=[0,0],e=[0,0];function
f(a){var
c=a[1];if(c){var
f=c[1];return aa(f,B)?C(a,d):aa(B,f)?C(a,b):C(a,e)}throw[0,bR,bQ]}c(l[15],f,a);return[0,d[1],[0,e[1],[0,b[1],0]]]}function
aJ(d){var
b=[0,0],h=a(l[1],d);function
e(e){var
a=[0,0],f=(h-b[1]|0)-1|0,i=e[2],j=e[1],k=1;if(!(f<1)){var
d=k;for(;;){C(B,a);var
n=d+1|0;if(f!==d){var
d=n;continue}break}}C(aF,a);var
g=b[1],l=1;if(!(g<1)){var
c=l;for(;;){C(B,a);var
m=c+1|0;if(g!==c){var
c=m;continue}break}}b[1]=b[1]+1|0;return[0,j,a[1],i]}return c(l[17],e,d)}function
aK(b,a){var
c=b[3],d=c||a[3],e=N(l[23],aj,b[2],a[2]);return[0,N(l[23],aj,b[1],a[1]),e,d]}function
al(b,a){var
d=a[3],e=a[2];function
f(a){return ak(b,a)}var
g=c(l[17],f,e),h=a[1];function
i(a){return ak(b,a)}return[0,c(l[17],i,h),g,d]}function
am(b){var
c=b[3],d=b[2];return[0,a(l[6],b[1]),d,c]}function
an(b){return a(l[5],b[1])}function
aL(b,d){var
a=[0,0];function
e(b){function
e(c){var
d=aG(an(b)),e=an(c),f=al(d,c);return C(am(aK(al(e,b),f)),a)}return c(l[15],e,d)}c(l[15],e,b);return a[1]}function
aM(e){var
a=aI(e);if(a){var
b=a[2];if(b){var
d=b[2];if(d)if(!d[2]){var
f=b[1],g=aL(a[1],d[1]),h=c(l[17],am,f);return c(I[25],h,g)}}}throw[0,R,bS]}function
aN(d){var
f=a(l[5],d)[1],g=a(l[1],f),b=[0,aJ(d)],e=g-1|0,h=1;if(!(e<1)){var
c=h;for(;;){b[1]=aM(b[1]);var
i=c+1|0;if(e!==c){var
c=i;continue}break}}return b[1]}var
ao=[Z,bT,W(0)],i=[0,bL,aE,B,aF,J,aG,aj,bN,ak,bO,bP,aa,aH,C,aI,aJ,aK,al,am,an,aL,aM,aN,ao,function(a){var
b=aN(a);function
d(a){var
b=a[1];if(b)if(!b[2]){var
c=a[3],d=b[1],i=a[2],f=aa(d,B),g=f?1-c:f;if(g)var
e=g;else
var
h=aH(d,B),e=h?c:h;if(e)throw[0,ao,[0,[0,d,c,i],0]];return e}throw[0,R,bU]}try{c(l[15],d,b);var
e=0;return e}catch(a){a=H(a);if(a[1]===ao)return a[2];throw a}}];ai(108,i,"Fourier_plugin.Fourier");var
r=a(bV[19],[0,s[74],s[92]]);function
u(c){var
b=i[3];return[0,a(r[1],50),b]}function
aO(b,a){try{var
d=c(r[7],b[1],a);return d}catch(a){a=H(a);if(a===bW)return i[3];throw a}}function
x(a,b,d){var
e=aO(a,b),f=c(i[7],e,d);N(r[10],a[1],b,f);return a}function
y(a,b){var
d=c(i[7],a[2],b);return[0,a[1],d]}function
aP(b){var
a=i[4];return y(u(0),a)}function
aQ(d,b){var
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
aR(b,a){var
d=u(0),e=a[1];function
f(e,a){x(d,e,c(i[9],b,a));return 0}c(r[12],f,e);return y(d,c(i[9],b,a[2]))}function
O(d){var
b=a(S[17][7],d),c=b[1];if(0===c[0]){var
e=b[3],f=c[1];if(p.caml_equal(b[2],S[5][6]))if(p.caml_string_equal(a(S[5][8],f),bX))return a(S[6][5],e)}return bY}function
ab(d){var
c=d;for(;;){var
b=a(s[26],c);switch(b[0]){case
5:var
c=b[1];continue;case
10:return O(b[1][1]);default:return bZ}}}var
E=[Z,b0,W(0)];function
q(k){var
f=k;for(;;){var
e=a(s[26],f);switch(e[0]){case
5:var
f=e[1];continue;case
9:var
b=e[2],d=ab(e[1]);if(h(d,b1)){if(h(d,b2)){if(h(d,b3)){if(h(d,b4)){if(h(d,b5)){if(h(d,b6))throw E;var
l=q(j(b,1)[2]),m=q(j(b,0)[1]);return c(i[7],m,l)}var
n=q(j(b,0)[1]);return a(i[6],n)}var
o=q(j(b,1)[2]),p=q(j(b,0)[1]);return c(i[9],p,o)}var
r=q(j(b,1)[2]),t=q(j(b,0)[1]);return c(i[8],t,r)}var
u=q(j(b,0)[1]);return a(i[10],u)}var
v=q(j(b,1)[2]),w=q(j(b,0)[1]);return c(i[11],w,v);case
10:var
g=O(e[1][1]);if(h(g,b7)){if(h(g,b8))throw E;return i[4]}return i[3];default:throw E}}}var
T=[Z,b9,W(0)];function
o(k){try{var
f=a(s[26],k);switch(f[0]){case
5:var
g=o(f[1]);break;case
9:var
b=f[2],e=ab(f[1]);if(h(e,b_))if(h(e,b$))if(h(e,ca))if(h(e,cb))if(h(e,cc)){if(h(e,cd))throw T;var
w=o(j(b,1)[2]),d=aQ(o(j(b,0)[1]),w)}else
var
z=o(j(b,0)[1]),d=aR(a(i[6],i[4]),z);else
try{var
l=q(j(b,0)[1]);try{var
F=q(j(b,1)[2]),G=c(i[9],l,F),I=y(u(0),G),m=I}catch(a){a=H(a);if(a!==E)throw a;var
C=j(b,1)[2],m=x(u(0),C,l)}var
d=m}catch(a){a=H(a);if(a!==E)throw a;var
A=q(j(b,1)[2]),B=j(b,0)[1],d=x(u(0),B,A)}else
var
J=o(j(b,1)[2]),d=D(o(j(b,0)[1]),J);else
var
K=q(j(b,0)[1]),L=a(i[10],K),d=y(u(0),L);else{var
n=q(j(b,1)[2]);try{var
P=q(j(b,0)[1]),Q=c(i[11],P,n),R=y(u(0),Q),p=R}catch(c){c=H(c);if(c!==E)throw c;var
M=a(i[10],n),N=j(b,0)[1],p=x(u(0),N,M)}var
d=p}var
g=d;break;case
10:var
r=O(f[1][1]);if(h(r,ce)){if(h(r,cf))throw T;var
t=aP(0)}else
var
t=u(0);var
g=t;break;default:throw T}return g}catch(a){a=H(a);if(a!==E)if(a!==T)throw a;var
v=i[4];return x(u(0),k,v)}}function
cg(b){var
a=[0,0];function
d(c,b){a[1]=[0,[0,b,c],a[1]];return 0}c(r[12],d,b);return a[1]}var
F=[Z,ch,W(0)];function
aS(n){var
r=n[2],e=a(w[ay][1],n[1]),t=a(w[ay][1],r),l=a(s[26],t);if(9===l[0]){var
f=l[2],m=a(s[26],l[1]);switch(m[0]){case
10:var
u=m[1][1];if(2===f.length-1){var
b=j(f,0)[1],d=j(f,1)[2],g=O(u);if(h(g,ci)){if(h(g,cj)){if(h(g,ck)){if(h(g,cl))throw F;var
v=o(d);return[0,[0,e,cm,b,d,D(o(b),v),1],0]}var
x=o(d);return[0,[0,e,cn,b,d,D(o(b),x),0],0]}var
y=o(b);return[0,[0,e,co,d,b,D(o(d),y),1],0]}var
z=o(b);return[0,[0,e,cp,d,b,D(o(d),z),0],0]}break;case
11:var
p=m[1][1];if(1-c(cq[5],[2,[0,p[1],p[2]]],K[28]))throw F;var
A=j(f,0)[1],i=j(f,1)[2],k=j(f,2)[3],q=a(s[26],A);if(10===q[0]){if(h(O(q[1][1]),cr))throw F;var
B=o(i),C=[0,[0,e,cs,k,i,D(o(k),B),0],0],E=o(k);return[0,[0,e,ct,i,k,D(o(i),E),0],C]}throw F}throw F}throw F}function
aT(e){var
b=[0,-1],d=a(r[1],50);function
f(a){var
e=a[5][1];function
f(a,f){var
e=1-c(r[11],d,a);return e?(b[1]=b[1]+1|0,N(r[5],d,a,b[1])):e}return c(r[12],f,e)}c(l[15],f,e);function
g(e){var
f=p.caml_make_vect(b[1]+1|0,i[3]),g=e[5][1];function
h(e,b){var
a=c(r[7],d,e);return j(f,a)[a+1]=b}c(r[12],h,g);var
k=e[6],l=[0,a(i[6],e[5][2]),0],m=a(ap[11],f);return[0,c(I[25],m,l),k]}var
h=c(l[17],g,e);return a(i[25],h)}function
n(b){var
c=bA(b);return bK===c?b[1]:d===c?a(aU[2],b):b}function
g(b){var
c=bA(b),e=bK===c?b[1]:d===c?a(aU[2],b):b;return a(w[8],e)}function
ac(c,b){var
d=N(K[2],cu,c,b);return a(ad[50],d)}var
aV=[d,function(b){return a(K[41],0)}],aW=[d,function(c){var
b=a(K[48],0);return a(ad[50],b)}],aX=[d,function(c){var
b=a(K[51],0);return a(ad[50],b)}],cv=[d,function(c){var
b=a(K[39],0);return a(ad[50],b)}];function
v(a){return ac(cw,a)}var
aY=[d,function(a){return v(cx)}],cz=[d,function(a){return v(cy)}],aZ=[d,function(a){return v(cA)}],cC=[d,function(a){return v(cB)}],cE=[d,function(a){return v(cD)}],a0=[d,function(a){return v(cF)}],P=[d,function(a){return v(cG)}],ae=[d,function(a){return v(cH)}],a1=[d,function(a){return v(cI)}],aq=[d,function(a){return v(cJ)}],a2=[d,function(a){return v(cK)}],U=[d,function(a){return v(cL)}],a3=[d,function(a){return ac(cN,cM)}];function
k(a){return ac(cO,a)}var
af=[d,function(a){return k(cP)}],ag=[d,function(a){return k(cQ)}],a4=[d,function(a){return k(cR)}],a5=[d,function(a){return k(cS)}],a6=[d,function(a){return k(cT)}],a7=[d,function(a){return k(cU)}],a8=[d,function(a){return k(cV)}],a9=[d,function(a){return k(cW)}],a_=[d,function(a){return k(cX)}],a$=[d,function(a){return k(cY)}],ba=[d,function(a){return k(cZ)}],bb=[d,function(a){return k(c0)}],bc=[d,function(a){return k(c1)}],bd=[d,function(a){return k(c2)}],be=[d,function(a){return k(c3)}],bf=[d,function(a){return k(c4)}],bg=[d,function(a){return k(c5)}],bh=[d,function(a){return k(c6)}],bi=[d,function(a){return k(c7)}],bj=[d,function(a){return k(c8)}],bk=[d,function(a){return k(c9)}],bl=[d,function(a){return k(c_)}],bm=[d,function(a){return k(c$)}],bn=[d,function(a){return k(da)}],bo=[d,function(a){return k(db)}],bp=[d,function(a){return k(dc)}];function
dd(a){return 1===a[2]?1:0}function
z(a){return[0,a[1],a[2]]}function
ar(d){var
e=a(I[6],d);if(0===e)return n(a2);var
b=[0,n(U)],f=e-1|0,g=1;if(!(f<1)){var
c=g;for(;;){var
j=b[1],k=[0,n(U),j],l=[0,n(ae),k];b[1]=a(s[13],l);var
m=c+1|0;if(f!==c){var
c=m;continue}break}}if(0<=d)return b[1];var
h=[0,b[1]],i=[0,n(a1),h];return a(s[13],i)}function
Q(c){var
b=z(c),d=b[1],e=[0,ar(b[2])],f=[0,n(aq),e],g=a(s[13],f),h=[0,ar(d),g],i=[0,n(P),h];return a(s[13],i)}function
G(D,j){var
m=j[2],n=j[1],o=g(af),b=[0,a(e[86],o)],p=g(af),d=[0,a(e[86],p)],k=n-1|0,q=1;if(!(k<1)){var
i=q;for(;;){var
z=b[1],A=g(ag),B=a(e[86],A);b[1]=c(f[66][3],B,z);var
C=i+1|0;if(k!==i){var
i=C;continue}break}}var
l=m-1|0,r=1;if(!(l<1)){var
h=r;for(;;){var
v=d[1],w=g(ag),x=a(e[86],w);d[1]=c(f[66][3],x,v);var
y=h+1|0;if(l!==h){var
h=y;continue}break}}var
s=[0,b[1],[0,d[1],0]],t=g(a5),u=a(e[86],t);return c(f[66][19],u,s)}function
bq(F,j){var
k=j[1],o=j[2];if(0===k)var
p=g(a6),l=a(e[86],p);else
var
E=g(a7),l=a(e[86],E);var
b=[0,l],q=g(af),d=[0,a(e[86],q)],m=k-1|0,r=1;if(!(m<1)){var
i=r;for(;;){var
A=b[1],B=g(a4),C=a(e[86],B);b[1]=c(f[66][3],C,A);var
D=i+1|0;if(m!==i){var
i=D;continue}break}}var
n=o-1|0,s=1;if(!(n<1)){var
h=s;for(;;){var
w=d[1],x=g(ag),y=a(e[86],x);d[1]=c(f[66][3],y,w);var
z=h+1|0;if(n!==h){var
h=z;continue}break}}var
t=[0,b[1],[0,d[1],0]],u=g(a8),v=a(e[86],u);return c(f[66][19],v,t)}function
br(h,b){var
d=b[1],i=b[2];if(0===d){var
j=g(a9);return a(e[86],j)}var
k=bq(h,[0,-d|0,i]),l=g(a_),m=a(e[86],l);return c(f[66][3],m,k)}function
bs(d,b){var
h=G(d,[0,-b[1]|0,b[2]]),i=g(bp),j=a(e[86],i);return c(f[66][3],j,h)}var
bt=e[45];function
L(i){var
b=a(bt,a(w[8],i[1])),d=i[2];if(h(d,de)){if(h(d,df)){if(h(d,dg)){if(h(d,dh)){if(h(d,di)){if(h(d,dj))throw[0,R,dk];var
j=g(bc),k=a(e[86],j);return c(f[66][3],k,b)}var
l=g(bb),m=a(e[86],l);return c(f[66][3],m,b)}return b}return b}var
n=g(a$),o=a(e[86],n);return c(f[66][3],o,b)}var
p=g(ba),q=a(e[86],p);return c(f[66][3],q,b)}function
bu(a){function
b(a){return 0===a[0]?[0,a[1],a[2]]:[0,a[1],a[3]]}return c(l[17],b,a)}function
A(c){var
b=a(ap[11],c),d=a(l[6],b),e=a(ap[12],d),f=[0,a(l[5],b),e];return a(s[13],f)}var
ah=[Z,dl,W(0)];function
V(d){function
b(b){var
_=a(M[66][3],b),$=a(dm[42][4],b);a(K[3],dn);var
aa=c(dp[60],$,_),ac=a(w[ay][1],aa),o=a(S[1][6],dq);try{var
Z=a(s[26],ac);if(9===Z[0]){var
x=ab(Z[1]);if(h(x,dw))if(h(x,dx))if(h(x,dy)){if(h(x,dz))throw ah;var
bK=V(0),bL=a(e[22],o),bM=g(bd),bN=a(e[86],bM),bO=c(f[66][3],bN,bL),y=c(f[66][3],bO,bK)}else
var
bP=V(0),bQ=a(e[22],o),bR=g(be),bS=a(e[86],bR),bT=c(f[66][3],bS,bQ),y=c(f[66][3],bT,bP);else
var
bU=V(0),bV=a(e[22],o),bW=g(bf),bX=a(e[86],bW),bY=c(f[66][3],bX,bV),y=c(f[66][3],bY,bU);else
var
bZ=V(0),b0=a(e[22],o),b1=g(bg),b2=a(e[86],b1),b3=c(f[66][3],b2,b0),y=c(f[66][3],b3,bZ);return y}throw ah}catch(h){h=H(h);if(h===ah){var
ad=bu(a(M[66][4],b)),af=function(b){var
c=b[2];return[0,a(w[10],b[1]),c]},m=[0,0],ag=c(l[17],af,ad),ai=function(a){try{var
b=m[1],d=aS(a);m[1]=c(I[25],d,b);var
e=0;return e}catch(a){a=H(a);if(a===F)return 0;throw a}};c(l[15],ai,ag);if(0===m[1]){var
aj=a(bv[3],dr);N(bw[6],0,0,aj)}var
q=aT(m[1]),r=[0,a(M[16],0)];if(0===q){var
ak=a(bv[3],ds);N(bw[6],0,0,ak)}else{if(q)if(q[2])var
J=0;else{var
B=q[1],O=B[2],C=B[1],D=[0,0],al=c(l[47],m[1],B[3]),am=function(a){var
b=a[2],c=p.caml_notequal(b,i[3]),d=a[1],e=c?(D[1]=[0,[0,d,b],D[1]],0):c;return e};c(l[15],am,al);var
E=D[1];if(!E)throw[0,R,dv];var
T=E[2],W=E[1],t=W[2],j=W[1],k=[0,j[6]],an=j[3],ao=Q(t),u=[0,A([0,n(P),ao,an])],ap=j[4],ar=Q(t),v=[0,A([0,n(P),ar,ap])],as=function(b){var
c=b[2],a=b[1],d=k[1],e=d||a[6];k[1]=e;var
f=a[3],g=Q(c),h=A([0,n(P),g,f]),i=u[1];u[1]=A([0,n(ae),i,h]);var
j=a[4],l=Q(c),m=A([0,n(P),l,j]),o=v[1];v[1]=A([0,n(ae),o,m]);return 0};c(l[15],as,T);var
at=v[1],au=u[1],av=k[1]?n(aY):n(aZ),X=A([0,av,au,at]),aw=Q(C);if(j[6])var
ax=[0,G(b,z(t)),0],az=[0,L(j),ax],aA=g(bh),aB=a(e[86],aA),Y=c(f[66][19],aB,az);else
var
bF=[0,G(b,z(t)),0],bG=[0,L(j),bF],bI=g(bi),bJ=a(e[86],bI),Y=c(f[66][19],bJ,bG);var
d=[0,Y];k[1]=j[6];var
aC=function(j){var
i=j[2],h=j[1];if(k[1])if(h[6]){var
l=[0,G(b,z(i)),0],m=[0,L(h),l],n=[0,d[1],m],o=g(bj),p=a(e[86],o);d[1]=c(f[66][19],p,n)}else{var
s=[0,G(b,z(i)),0],t=[0,L(h),s],u=[0,d[1],t],v=g(bk),w=a(e[86],v);d[1]=c(f[66][19],w,u)}else
if(h[6]){var
x=[0,G(b,z(i)),0],y=[0,L(h),x],A=[0,d[1],y],B=g(bl),C=a(e[86],B);d[1]=c(f[66][19],C,A)}else{var
D=[0,G(b,z(i)),0],E=[0,L(h),D],F=[0,d[1],E],H=g(bm),I=a(e[86],H);d[1]=c(f[66][19],I,F)}var
q=k[1],r=q||h[6];k[1]=r;return 0};c(l[15],aC,T);var
aD=O?br(b,z(C)):bs(b,z(C)),aE=[0,d[1],0],aF=0,aG=0,aH=function(b){var
d=g(a3),h=a(e[86],d),i=a(e[86],b);return c(f[66][3],i,h)},aI=n(aV),aJ=a(f[66][59],aI),aK=[0,c(M[71][1],aJ,aH),aG],aL=a(M[16],0),aM=a(M[16],0),aN=[0,c(f[66][12],aM,aL),aK],aO=g(U),aP=[0,n(U)],aQ=[0,n(aq),aP],aR=a(s[13],aQ),aU=a(w[8],aR),a1=c(bx[12],aU,aO),a2=[0,aD,[0,c(f[66][19],a1,aN),aF]],a4=a(w[8],aw),a5=u[1],a6=v[1],a7=A([0,n(a0),a6,a5]),a8=a(w[8],a7),a9=c(bx[12],a8,a4),a_=c(f[66][19],a9,a2),a$=O?g(bn):g(bo),ba=a(e[86],a$),bb=c(f[66][3],ba,a_),bc=A([0,n(aX),X]),bp=a(w[8],bc),bq=a(e[53],bp),bt=[0,c(f[66][3],bq,bb),aE],by=a(w[8],X),bz=a(e[bH],by);r[1]=c(f[66][19],bz,bt);var
bA=[0,r[1],0],bB=a(du[2],0),bC=[0,c(f[66][3],e[16],bB),bA],bD=g(aW),bE=a(e[bH],bD);r[1]=c(f[66][19],bE,bC);var
J=1}else
var
J=0;if(!J)throw[0,R,dt]}return r[1]}throw h}}return a(M[66][9],b)}var
as=[0,r,u,aO,x,y,aP,aQ,D,aR,O,ab,E,q,T,o,cg,F,aS,aT,n,n,g,ac,aV,aW,aX,cv,v,aY,cz,aZ,cC,cE,a0,P,ae,a1,aq,a2,U,a3,k,af,ag,a4,a5,a6,a7,a8,a9,a_,a$,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,dd,z,ar,Q,G,bq,br,bs,bt,L,bu,A,ah,V];ai(128,as,"Fourier_plugin.FourierR");a(dA[10],at);var
dB=0,dD=[0,[0,dC,function(b){return a(as[82],0)}],dB];dG(dF[10][8],at,dE,0,dD);var
by=[0,at];ai(131,by,"Fourier_plugin.G_fourier");ai(132,[0,i,as,by],"Fourier_plugin");return}
