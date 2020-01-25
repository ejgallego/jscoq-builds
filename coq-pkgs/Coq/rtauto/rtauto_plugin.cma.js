function(c$){"use strict";var
Z="Rtauto",ah="{ ",A=246,aj=" pruned",aD=" => ",ai=" }",K="rtauto",ag=" created",Y="",aG="Proof steps : ",aE="can't happen.",af=" created / ",aC="Hypotheses : ",aF="Proof branches : ",X=250,r=c$.jsoo_runtime,W=r.caml_check_bound,d=r.caml_new_string,U=r.caml_obj_tag,T=r.caml_register_global,y=r.caml_wrap_exception;function
b(a,b){return a.length==1?a(b):r.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):r.caml_call_gen(a,[b,c])}function
l(a,b,c,d){return a.length==3?a(b,c,d):r.caml_call_gen(a,[b,c,d])}function
aB(a,b,c,d,e){return a.length==4?a(b,c,d,e):r.caml_call_gen(a,[b,c,d,e])}function
V(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):r.caml_call_gen(a,[b,c,d,e,f])}var
i=r.caml_get_global_data(),ae=d("rtauto_plugin"),g=i.Int,e=i.Util,a=i.Pp,D=i.Feedback,B=i.CErrors,z=i.Not_found,_=i.Goptions,I=i.EConstr,ab=i.Retyping,N=i.CamlinternalLazy,ac=i.Names,h=i.Constr,J=i.Proofview,G=i.Coqlib,x=i.System,az=i.Tactics,ax=i.CClosure,ay=i.Reductionops,O=i.UnivGen,aZ=i.Control,cB=i.Evd,cC=i.Termops,cQ=i.Ltac_plugin__Tacinterp,cW=i.Term,bO=i.Explore,c5=i.Mltop,c_=i.Ltac_plugin__Tacentries;T(110,[0,0,0,0],"Rtauto_plugin");var
f=[0,0,0,0,0,0,0,0,0,0],a0=d(" -> "),a1=d(" \\/ "),a2=d(" /\\ "),a4=d(")"),a5=d("("),a3=d("#"),bs=d(aj),bt=d(af),bu=d(aC),bv=d(aj),bw=d(af),bx=d(aF),by=d(aj),bz=d(af),bA=d(aG),bH=d(ag),bI=d(aC),bJ=d(ag),bK=d(aF),bL=d(ag),bM=d(aG),bN=d("Pruning is off"),bB=d(" branches"),bC=d("Non-deterministic choices : "),bD=d(" failures"),bE=d(" successes / "),bF=d("Branch ends: "),bG=d("Proof-search statistics :"),br=d("<complete>"),bi=d(ai),bj=d("goal  ="),bk=d("cnx   ="),bl=d("arrows="),bm=d("norev ="),bn=d("rev   ="),bo=d("ABSURD"),bq=d(Y),bp=d(ah),bh=d(aD),bd=d(aD),bc=d(Y),be=d(ai),bf=d(ah),a$=d(", "),a_=d(Y),ba=d("]"),bb=d("[ "),a7=d(Y),a8=d(ai),a9=d(ah),aY=d("already succeeded."),aT=d(aE),aU=[0,d("search_in_rev_hyps")],aQ=d(aE),aR=[0,d("search_no_rev")],aN=d("not a successful state."),aL=d("wrong arity."),aM=[0,d("add_step")],aI=[0,d(Z),[0,d("Pruning"),0]],aJ=d("Rtauto Pruning"),aO=d("Rtauto_plugin.Proof_search.Here_is"),cN=[0,d("Coq"),[0,d(K),[0,d(Z),0]]],cO=d("goal should be in Prop"),cP=[0,d(K)],cR=d("Starting proof-search ..."),cS=d("rtauto couldn't find any proof"),cT=[0,d(K)],cU=d("Proof tree found in "),cV=d("Building proof term ... "),cX=d("Giving proof term to Coq ... "),cY=d(" nodes (constants)"),cZ=d("Proof term size : "),c0=d(" steps"),c1=d("Proof size : "),c2=d("Proof term built in "),c3=d("Proof term type-checking is on"),c4=d("Internal tactic executed in "),bR=d("core.or.type"),bQ=d("core.and.type"),bP=d("core.False.type"),bS=d("num.pos.xI"),bU=d("num.pos.xO"),bW=d("num.pos.xH"),bY=d("plugins.rtauto.empty"),b0=d("plugins.rtauto.push"),b2=d("plugins.rtauto.Reflect"),b4=d("plugins.rtauto.Atom"),b6=d("plugins.rtauto.Arrow"),b8=d("plugins.rtauto.Bot"),b_=d("plugins.rtauto.Conjunct"),ca=d("plugins.rtauto.Disjunct"),cc=d("plugins.rtauto.Ax"),ce=d("plugins.rtauto.I_Arrow"),cg=d("plugins.rtauto.E_Arrow"),ci=d("plugins.rtauto.D_Arrow"),ck=d("plugins.rtauto.E_False"),cm=d("plugins.rtauto.I_And"),co=d("plugins.rtauto.E_And"),cq=d("plugins.rtauto.D_And"),cs=d("plugins.rtauto.I_Or_l"),cu=d("plugins.rtauto.I_Or_r"),cw=d("plugins.rtauto.E_Or"),cy=d("plugins.rtauto.D_Or"),cF=[0,d(Z),[0,d("Verbose"),0]],cG=d("Rtauto Verbose"),cJ=[0,d(Z),[0,d("Check"),0]],cK=d("Rtauto Check"),c7=[0,d(K),0],c9=d(K);function
ak(a){f[1]=0;f[2]=0;f[3]=0;f[4]=0;f[5]=0;f[6]=0;f[7]=0;f[8]=0;f[9]=0;return 0}var
L=[0,1];function
aH(a){L[1]=a;return 0}var
aK=[0,0,aJ,aI,function(a){return b(e[3],L)},aH];c(_[4],0,aK);function
M(g,f){var
b=g,a=f;for(;;)if(typeof
b==="number")return typeof
a==="number"?0:-1;else
switch(b[0]){case
0:var
h=b[1];return typeof
a==="number"?1:0===a[0]?r.caml_int_compare(h,a[1]):-1;case
1:var
i=b[2],j=b[1];if(typeof
a!=="number")switch(a[0]){case
1:var
k=a[2],c=M(j,a[1]);if(0===c){var
b=i,a=k;continue}return c;case
0:break;default:return-1}return 1;case
2:var
l=b[2],m=b[1];if(typeof
a!=="number")switch(a[0]){case
2:var
n=a[2],d=M(m,a[1]);if(0===d){var
b=l,a=n;continue}return d;case
3:return-1}return 1;default:var
o=b[2],p=b[1];if(typeof
a!=="number"&&3===a[0]){var
q=a[2],e=M(p,a[1]);if(0===e){var
b=o,a=q;continue}return e}return 1}}var
t=b(e[26][1],[0,M]);function
al(d,c){if(typeof
d==="number")switch(d){case
0:if(c)if(!c[2])return[1,c[1]];break;case
1:if(c){var
e=c[2];if(e)if(!e[2])return[5,c[1],e[1]]}break;case
2:if(c)if(!c[2])return[8,c[1]];break;default:if(c)if(!c[2])return[9,c[1]]}else
switch(d[0]){case
0:if(!c)return[0,d[1]];break;case
1:if(c)if(!c[2])return[2,d[1],d[2],c[1]];break;case
2:if(c){var
f=c[2];if(f)if(!f[2])return[3,d[1],c[1],f[1]]}break;case
3:if(!c)return[4,d[1]];break;case
4:if(c)if(!c[2])return[6,d[1],c[1]];break;case
5:if(c)if(!c[2])return[7,d[1],c[1]];break;case
6:if(c){var
g=c[2];if(g)if(!g[2])return[10,d[1],c[1],g[1]]}break;default:if(c)if(!c[2])return[11,d[1],c[1]]}var
h=b(a[3],aL);return l(B[2],0,aM,h)}function
am(c){if(0===c[0])return c[1];var
d=b(a[3],aN);return l(B[2],0,0,d)}function
n(a){return[0,a,0,g[2][1]]}function
C(b,c){var
a=b[1];return[0,[0,a[1],a[2],a[3],a[4],a[5],a[6],a[7],c],1,b[3]]}function
p(q,d){f[5]=c(e[4],f[5],1);var
a=q[1],b=c(e[4],a[3],1),j=l(t[4],d,b,a[4]);try{var
K=c(t[23],d,a[5]),L=c(t[6],d,a[5]),M=a[6],N=function(a,c){return[0,[0,b,a[1],d,a[2]],c]},O=l(e[22][16],N,K,M),k=O,h=L}catch(b){b=y(b);if(b!==z)throw b;var
k=a[6],h=a[5]}if(typeof
d==="number")var
m=[0,a[1],a[2],b,j,h,k,[0,b],a[8]];else
switch(d[0]){case
0:var
m=[0,a[1],a[2],b,j,h,k,a[7],a[8]];break;case
1:var
r=d[2],i=d[1];try{var
G=[0,[0,c(t[23],i,a[4]),b,i,r],k],o=G,n=h}catch(a){a=y(a);if(a!==z)throw a;try{var
v=[0,[0,b,r],c(t[23],i,h)],w=l(t[4],i,v,h),u=w}catch(a){a=y(a);if(a!==z)throw a;var
u=l(t[4],i,[0,[0,b,r],0],h)}var
o=k,n=u}if(typeof
i==="number")var
p=0;else
switch(i[0]){case
1:var
A=a[8],B=a[7],C=l(g[3][4],b,d,a[2]),s=[0,a[1],C,b,j,n,o,B,A],p=1;break;case
0:var
p=0;break;default:var
D=a[8],E=a[7],F=a[2],s=[0,l(g[3][4],b,d,a[1]),F,b,j,n,o,E,D],p=1}if(!p)var
s=[0,a[1],a[2],b,j,n,o,a[7],a[8]];var
m=s;break;default:var
H=a[8],I=a[7],J=a[2],m=[0,l(g[3][4],b,d,a[1]),J,b,j,h,k,I,H]}var
x=c(g[2][4],b,q[3]);return[0,m,q[2],x]}var
an=[248,aO,r.caml_fresh_oo_id(0)];function
aP(d){var
f=d[8];if(typeof
f==="number")var
h=0;else
if(3===f[0])var
k=f[2],m=f[1],o=[0,C(n(d),k),0],q=[0,[0,[0,3,1,g[2][1]],o],0],r=[0,C(n(d),m),0],j=[0,[0,[0,2,1,g[2][1]],r],q],h=1;else
var
h=0;if(!h)var
j=0;var
i=[0,j];function
s(j,f){if(typeof
f!=="number"&&1===f[0]){var
h=f[1];if(typeof
h!=="number"&&1===h[0]){var
k=f[2],m=h[2],r=h[1],s=d[8],t=d[7],u=d[6],v=d[5],w=d[4],x=d[3],y=c(g[3][6],j,d[2]),o=[0,d[1],y,x,w,v,u,t,s],z=b(e[3],i),A=[0,p(n(o),k),0],D=[0,p(p(C(n(o),m),[1,m,k]),r),A];i[1]=[0,[0,[0,[2,j],0,b(g[2][5],j)],D],z];return 0}}var
q=b(a[3],aQ);return l(B[2],0,aR,q)}c(g[3][11],s,d[2]);var
t=b(e[3],i);return b(e[22][9],t)}function
aS(d){try{var
u=d[1];try{var
j=function(b,a){throw[0,an,[0,b,a]]};c(g[3][11],j,u);throw z}catch(j){j=y(j);if(j[1]===an){var
r=j[2],e=r[2],f=r[1],m=function(a){return[0,a,0,b(g[2][5],f)]},v=d[8],w=d[7],x=d[6],A=d[5],C=d[4],D=d[3],E=d[2],k=[0,c(g[3][6],f,d[1]),E,D,C,A,x,w,v];if(typeof
e==="number")var
i=0;else
switch(e[0]){case
1:var
h=e[1];if(typeof
h==="number")var
q=0;else
switch(h[0]){case
2:var
G=[1,h[1],[1,h[2],e[2]]],H=[0,p(n(k),G),0],s=[0,[0,m([5,f]),H],0],q=1;break;case
3:var
t=e[2],I=[1,h[2],t],J=[1,h[1],t],K=[0,p(p(n(k),J),I),0],s=[0,[0,m([7,f]),K],0],q=1;break;default:var
q=0}if(q)var
o=s,i=1;else
var
i=0;break;case
2:var
L=e[2],M=e[1],N=[0,p(p(n(k),M),L),0],o=[0,[0,m([4,f]),N],0],i=1;break;case
3:var
O=e[2],P=e[1],Q=[0,p(n(k),O),0],R=[0,p(n(k),P),Q],o=[0,[0,m([6,f]),R],0],i=1;break;default:var
i=0}if(!i)var
F=b(a[3],aT),o=l(B[2],0,aU,F);return o}throw j}}catch(a){a=y(a);if(a===z)return aP(d);throw a}}function
aV(a){var
i=a[6];if(i){var
j=i[2],e=i[1],f=e[2],m=e[1],o=e[4],l=e[3];if(typeof
l==="number")var
h=0;else
switch(l[0]){case
1:var
s=a[8],t=a[7],u=a[5],v=a[4],w=a[3],x=c(g[3][6],f,a[2]),k=[0,a[1],x,w,v,u,j,t,s],h=1;break;case
0:var
h=0;break;default:var
y=a[8],z=a[7],A=a[5],B=a[4],D=a[3],E=a[2],k=[0,c(g[3][6],f,a[1]),E,D,B,A,j,z,y],h=1}if(!h)var
k=[0,a[1],a[2],a[3],a[4],a[5],j,a[7],a[8]];var
q=[0,p(n(k),o),0],r=b(g[2][5],f);return[0,[0,[0,[1,m,f],0,c(g[2][4],m,r)],q],0]}var
d=a[8];if(typeof
d!=="number")switch(d[0]){case
1:var
F=d[2],G=d[1],H=[0,p(C(n(a),F),G),0];return[0,[0,[0,0,1,g[2][1]],H],0];case
2:var
I=d[2],J=d[1],K=[0,C(n(a),I),0],L=[0,C(n(a),J),K];return[0,[0,[0,1,1,g[2][1]],L],0]}return aS(a)}function
aW(a){var
d=a[7];if(d){var
e=d[1];return[0,[0,[0,[3,e],0,b(g[2][5],e)],0],0]}try{var
f=c(t[23],a[8],a[4]),h=[0,[0,[0,[0,f],1,b(g[2][5],f)],0],0];return h}catch(b){b=y(b);if(b===z)return aV(a);throw b}}var
aX=n([0,g[3][1],g[3][1],0,t[1],t[1],0,0,0]);function
ao(b,a){var
c=C(aX,a);function
d(b,a){return p(a,b[2])}return[1,l(e[22][16],d,b,c)[1],0]}function
ap(a){return 0===a[0]?1:0}function
aq(d){if(0===d[0]){var
i=b(a[3],aY);return l(B[2],0,0,i)}var
y=d[2],j=d[1];b(aZ[3],0);var
h=aW(j);if(h){var
k=b(e[22][1],h[2]);f[9]=c(e[4],f[9],k)}else
f[7]=c(e[4],f[7],1);function
m(z){var
p=z[2],h=z[1];f[1]=c(e[4],f[1],1);if(p){var
x=p[2],q=p[1],K=b(e[22][1],x);f[3]=c(e[4],f[3],K);return[1,q[1],[0,[0,0,x,h[1],h[2],h[3],q[2],q[3]],y]]}f[8]=c(e[4],f[8],1);var
M=h[3],N=h[2],i=y,d=[0,al(h[1],0),N,M];for(;;){if(i){var
k=i[2],a=i[1];if(b(e[3],L))if(b(e[22][48],a[1])){var
O=a[6]?d[2]?1:0:0;if(!O){var
D=a[7],E=function(b){return function(a){return c(g[2][3],a,b[3])}}(d);if(!c(g[2][17],E,D)){f[2]=c(e[4],f[2],1);var
F=b(e[22][1],a[2]);f[4]=c(e[4],f[4],F);var
G=b(g[2][20],a[7]),H=a[2],I=function(d,a){var
f=b(g[2][20],a[3]);return c(e[4],d,f)},J=l(e[22][15],I,G,H);f[6]=c(e[4],f[6],J);var
w=b(g[2][20],a[7]),j=d[1];if(12===j[0])var
A=j[2],r=[12,c(e[4],j[1],w),A];else
var
r=[12,w,j];var
i=k,d=[0,r,d[2],d[3]];continue}}}var
B=c(g[2][9],d[3],a[7]),s=c(g[2][7],a[5],B),t=a[4];if(t)var
m=t;else
var
v=1-a[6],m=v?d[2]:v;var
u=[0,d[1],a[1]],n=a[2];if(n){var
o=n[1];return[1,o[1],[0,[0,u,n[2],a[3],m,s,o[2],o[3]],k]]}var
C=b(e[22][9],u),i=k,d=[0,al(a[3],C),m,s];continue}return[0,d[1]]}}return c(e[22][68],m,h)}function
ar(d){if(typeof
d==="number")return b(a[3],a3);else{if(0===d[0])return b(a[16],d[1]);var
e=b(a[3],a4),f=u(d),g=c(a[25],2,f),h=b(a[3],a5),i=c(a[12],h,g);return c(a[12],i,e)}}function
aa(d){if(typeof
d!=="number"&&2===d[0]){var
e=d[1],f=ar(d[2]),g=b(a[3],a2),h=aa(e),i=c(a[12],h,g);return c(a[12],i,f)}return ar(d)}function
$(d){if(typeof
d!=="number"&&3===d[0]){var
e=d[1],f=aa(d[2]),g=b(a[3],a1),h=$(e),i=c(a[12],h,g);return c(a[12],i,f)}return aa(d)}function
u(d){if(typeof
d!=="number"&&1===d[0]){var
e=d[1],f=u(d[2]),g=b(a[3],a0),h=$(e),i=c(a[12],h,g);return c(a[12],i,f)}return $(d)}function
a6(a){return u(a)}function
as(f){var
d=[0,b(a[3],a7)];function
h(k,f){var
g=b(a[14],0),h=u(f),i=b(e[3],d),j=c(a[12],i,h);d[1]=c(a[12],j,g);return 0}c(g[3][11],h,f);var
i=b(a[3],a8),j=b(e[3],d),k=c(a[24],0,j),l=b(a[3],a9),m=c(a[12],l,k);return c(a[12],m,i)}function
at(g,f){var
d=[0,b(a[3],a_)];function
h(f){var
h=b(a[3],a$),i=b(g,f),j=b(e[3],d),k=c(a[12],j,i);d[1]=c(a[12],k,h);return 0}c(e[22][11],h,f);var
i=b(a[3],ba),j=b(e[3],d),k=b(a[3],bb),l=c(a[12],k,j);return c(a[12],l,i)}function
bg(d){var
e=d[3],f=u(d[4]),g=b(a[3],bh),h=u(e),i=c(a[12],h,g);return c(a[12],i,f)}function
au(h){if(0===h[0])return b(a[3],br);var
d=h[1],aa=b(a[5],0),p=b(a[3],bi),q=u(d[8]),r=b(a[3],bj),s=b(a[14],0),v=at(bg,d[6]),w=b(a[3],bk),x=b(a[14],0),y=d[5],f=[0,b(a[3],bc)];function
i(g,d){var
h=b(a[14],0),i=at(function(a){return u(a[2])},d),j=b(a[3],bd),k=u(g),l=b(e[3],f),m=c(a[12],l,k),n=c(a[12],m,j),o=c(a[12],n,i);f[1]=c(a[12],o,h);return 0}c(t[11],i,y);var
j=b(a[3],be),k=b(e[3],f),l=c(a[12],k,j),m=c(a[25],0,l),n=b(a[3],bf),o=c(a[12],n,m),z=b(a[3],bl),A=b(a[14],0),B=as(d[2]),C=b(a[3],bm),D=b(a[14],0),E=as(d[1]),F=b(a[3],bn);if(d[7])var
G=b(a[14],0),H=b(a[3],bo),g=c(a[12],H,G);else
var
g=b(a[3],bq);var
I=c(a[12],g,F),J=c(a[12],I,E),K=c(a[12],J,D),L=c(a[12],K,C),M=c(a[12],L,B),N=c(a[12],M,A),O=c(a[12],N,z),P=c(a[12],O,o),Q=c(a[12],P,x),R=c(a[12],Q,w),S=c(a[12],R,v),T=c(a[12],S,s),U=c(a[12],T,r),V=c(a[12],U,q),W=c(a[12],V,p),X=c(a[25],0,W),Y=b(a[3],bp),Z=b(a[14],0),_=c(a[12],Z,Y),$=c(a[12],_,X);return c(a[12],$,aa)}function
av(aK){if(b(e[3],L))var
g=b(a[5],0),h=b(a[3],bs),i=b(a[16],f[6]),j=b(a[3],bt),k=b(a[16],f[5]),l=b(a[3],bu),m=b(a[5],0),n=b(a[3],bv),o=b(a[16],f[4]),p=b(a[3],bw),q=b(a[16],f[3]),r=b(a[3],bx),s=b(a[5],0),t=b(a[3],by),u=b(a[16],f[2]),v=b(a[3],bz),w=b(a[16],f[1]),x=b(a[3],bA),y=c(a[12],x,w),z=c(a[12],y,v),A=c(a[12],z,u),B=c(a[12],A,t),C=c(a[12],B,s),E=c(a[12],C,r),F=c(a[12],E,q),G=c(a[12],F,p),H=c(a[12],G,o),I=c(a[12],H,n),J=c(a[12],I,m),K=c(a[12],J,l),M=c(a[12],K,k),N=c(a[12],M,j),O=c(a[12],N,i),P=c(a[12],O,h),d=c(a[12],P,g);else
var
ak=b(a[5],0),al=b(a[3],bH),am=b(a[16],f[5]),an=b(a[3],bI),ao=b(a[5],0),ap=b(a[3],bJ),aq=b(a[16],f[3]),ar=b(a[3],bK),as=b(a[5],0),at=b(a[3],bL),au=b(a[16],f[1]),av=b(a[3],bM),aw=b(a[5],0),ax=b(a[3],bN),ay=c(a[12],ax,aw),az=c(a[12],ay,av),aA=c(a[12],az,au),aB=c(a[12],aA,at),aC=c(a[12],aB,as),aD=c(a[12],aC,ar),aE=c(a[12],aD,aq),aF=c(a[12],aE,ap),aG=c(a[12],aF,ao),aH=c(a[12],aG,an),aI=c(a[12],aH,am),aJ=c(a[12],aI,al),d=c(a[12],aJ,ak);var
Q=b(a[3],bB),R=b(a[16],f[9]),S=b(a[3],bC),T=b(a[5],0),U=b(a[3],bD),V=b(a[16],f[7]),W=b(a[3],bE),X=b(a[16],f[8]),Y=b(a[3],bF),Z=b(a[5],0),_=b(a[3],bG),$=c(a[12],_,Z),aa=c(a[12],$,d),ab=c(a[12],aa,Y),ac=c(a[12],ab,X),ad=c(a[12],ac,W),ae=c(a[12],ad,V),af=c(a[12],ae,U),ag=c(a[12],af,T),ah=c(a[12],ag,S),ai=c(a[12],ah,R),aj=c(a[12],ai,Q);return c(D[6],0,aj)}T(119,[0,am,ao,aq,ap,au,a6,ak,av],"Rtauto_plugin__Proof_search");var
aw=b(bO[1],[0,aq,ap,au]);function
j(d,a){d[1]++;var
c=U(a);return X===c?a[1]:A===c?b(N[2],a):a}var
m=[0,0],q=[0,0],P=[A,function(d){var
a=b(G[2],bP),c=b(O[15],a);return b(h[74],c)}],Q=[A,function(d){var
a=b(G[2],bQ),c=b(O[15],a);return b(h[74],c)}],R=[A,function(d){var
a=b(G[2],bR),c=b(O[15],a);return b(h[74],c)}];function
k(a){return[A,function(d){var
c=b(G[2],a);return b(O[15],c)}]}var
bT=k(bS),bV=k(bU),bX=k(bW),bZ=k(bY),b1=k(b0),b3=k(b2),b5=k(b4),b7=k(b6),b9=k(b8),b$=k(b_),cb=k(ca),cd=k(cc),cf=k(ce),ch=k(cg),cj=k(ci),cl=k(ck),cn=k(cm),cp=k(co),cr=k(cq),ct=k(cs),cv=k(cu),cx=k(cw),cz=k(cy);function
cA(c,b,a){return aB(ay[17],ax[4],c,b,a)}function
H(a,g){var
f=b(I[142][1],g);try{var
i=a[2],j=function(a){return c(h[81],f,a[1])},k=[0,c(e[22][27],j,i)[2]];return k}catch(b){b=y(b);if(b===z){var
d=a[1];a[2]=[0,[0,f,d],a[2]];a[1]=c(e[4],d,1);return[0,d]}throw b}}function
v(e,a,d,s){var
f=s;for(;;){var
i=function(b){return aB(ay[16],ax[9],e,a,b)},t=cA(e,a,f),g=c(I[3],a,t);switch(g[0]){case
5:var
f=g[1];continue;case
6:var
m=g[3],n=g[2];if(l(I[121][13],a,1,m))if(1===V(ab[4],0,0,e,a,n)){var
u=v(e,a,d,n);return[1,u,v(e,a,d,m)]}return H(d,i(f));case
9:var
j=g[2],w=g[1];if(2===j.length-1)try{var
o=c(I[85],a,w)[1],p=U(Q),x=X===p?Q[1]:A===p?b(N[2],Q):Q;if(c(ac[43],o,x[1]))var
z=v(e,a,d,W(j,0)[1]),k=[2,z,v(e,a,d,W(j,1)[2])];else{var
q=U(R),B=X===q?R[1]:A===q?b(N[2],R):R;if(c(ac[43],o,B[1]))var
C=v(e,a,d,W(j,0)[1]),k=[3,C,v(e,a,d,W(j,1)[2])];else
var
k=H(d,i(f))}return k}catch(a){a=y(a);if(a===h[60])return H(d,i(f));throw a}break;case
11:var
D=g[1][1],r=U(P),E=X===r?P[1]:A===r?b(N[2],P):P;return c(ac[43],D,E[1])?0:H(d,i(f))}return H(d,i(f))}}function
ad(g,f,i,n,m){var
d=n,a=m;for(;;){if(a){var
b=a[1];if(0===b[0]){var
h=b[2],j=b[1],k=ad(g,f,i,[0,h,d],a[2]),o=function(a){return l(cC[33],cB[16],j[1],a)};if(!c(e[22][22],o,d))if(1===V(ab[4],0,0,g,f,h))return[0,[0,j,v(g,f,i,h)],k];return k}var
d=[0,b[3],[0,b[2],d]],a=a[2];continue}return 0}}function
s(a){if(1<a){if(0===(a&1)){var
c=[0,s(a>>1)],d=[0,j(q,bV),c];return b(h[16],d)}var
e=[0,s(a>>1)],f=[0,j(q,bT),e];return b(h[16],f)}return j(q,bX)}function
E(a){if(typeof
a==="number")return j(q,b9);else
switch(a[0]){case
0:var
c=[0,s(a[1])],d=[0,j(q,b5),c];return b(h[16],d);case
1:var
e=a[1],f=E(a[2]),g=[0,E(e),f],i=[0,j(q,b7),g];return b(h[16],i);case
2:var
k=a[1],l=E(a[2]),m=[0,E(k),l],n=[0,j(q,b$),m];return b(h[16],n);default:var
o=a[1],p=E(a[2]),r=[0,E(o),p],t=[0,j(q,cb),r];return b(h[16],t)}}function
w(b,f){var
a=f;for(;;){if(a){var
d=a[1],g=a[2],h=d[2];if(d[1]<b)return c(e[5],b,h);var
a=g;continue}return b}}function
o(p,f,n){var
d=p,a=n;for(;;)switch(a[0]){case
0:var
q=[0,s(w(a[1],d))],r=[0,j(m,cd),q];return b(h[16],r);case
1:var
t=a[1],u=[0,o(d,c(e[4],f,1),t)],v=[0,j(m,cf),u];return b(h[16],v);case
2:var
x=a[3],y=a[2],z=a[1],A=o(d,c(e[4],f,1),x),B=s(w(y,d)),C=[0,s(w(z,d)),B,A],D=[0,j(m,ch),C];return b(h[16],D);case
3:var
E=a[3],F=a[2],G=a[1],H=o(d,c(e[4],f,1),E),I=o(d,c(e[4],f,2),F),J=[0,s(w(G,d)),I,H],K=[0,j(m,cj),J];return b(h[16],K);case
4:var
L=[0,s(w(a[1],d))],M=[0,j(m,cl),L];return b(h[16],M);case
5:var
N=a[1],O=o(d,f,a[2]),P=[0,o(d,f,N),O],Q=[0,j(m,cn),P];return b(h[16],Q);case
6:var
R=a[2],S=a[1],T=o(d,c(e[4],f,2),R),U=[0,s(w(S,d)),T],V=[0,j(m,cp),U];return b(h[16],V);case
7:var
W=a[2],X=a[1],Y=o(d,c(e[4],f,1),W),Z=[0,s(w(X,d)),Y],_=[0,j(m,cr),Z];return b(h[16],_);case
8:var
$=[0,o(d,f,a[1])],aa=[0,j(m,ct),$];return b(h[16],aa);case
9:var
ab=[0,o(d,f,a[1])],ac=[0,j(m,cv),ab];return b(h[16],ac);case
10:var
ad=a[3],ae=a[2],af=a[1],ag=o(d,c(e[4],f,1),ad),ah=o(d,c(e[4],f,1),ae),ai=[0,s(w(af,d)),ah,ag],aj=[0,j(m,cx),ai];return b(h[16],aj);case
11:var
ak=a[2],al=a[1],am=o(d,c(e[4],f,2),ak),an=[0,s(w(al,d)),am],ao=[0,j(m,cz),an];return b(h[16],ao);default:var
g=a[1],ap=a[2];if(d)var
i=d[1][2],l=c(e[4],i,g),k=[0,[0,c(e[4],f,i),l],d];else
var
k=[0,[0,c(e[4],f,g),g],0];var
d=k,a=ap;continue}}var
F=[0,0];function
cD(a){var
c=[0,h[9]],d=[0,j(q,bZ),c],f=b(h[16],d),g=a[2];function
i(c,a){var
d=[0,h[9],c[1],a],e=[0,j(q,b1),d];return b(h[16],e)}return l(e[22][16],i,g,f)}function
cE(a){F[1]=a;return 0}var
cH=[0,0,cG,cF,function(a){return b(e[3],F)},cE];c(_[4],0,cH);var
S=[0,0];function
cI(a){S[1]=a;return 0}var
cL=[0,0,cK,cJ,function(a){return b(e[3],S)},cI];c(_[4],0,cL);function
cM(d){var
A=b(J[67][2],d),f=b(J[67][1],d),g=b(J[67][3],d),i=b(J[67][4],d);b(G[12],cN);var
k=[0,1,0];if(1!==V(ab[4],0,0,g,i,f)){var
C=b(a[3],cO);l(B[5],0,cP,C)}var
H=v(g,i,k,f),p=ad(g,i,k,[0,f,0],A);function
K(b,a){return[1,a[2],b]}var
r=l(e[22][15],K,H,p),s=b(cQ[8],0);if(s)if(0===s[1])var
t=aw[2],n=1;else
var
n=0;else
var
n=0;if(!n)var
t=aw[1];ak(0);if(b(e[3],F)){var
L=b(a[3],cR);c(D[6],0,L)}var
M=b(x[26],0);try{var
aT=am(b(t,ao(0,r))),u=aT}catch(c){c=y(c);if(c!==z)throw c;var
N=b(a[3],cS),u=l(B[5],0,cT,N)}var
O=b(x[26],0);if(b(e[3],F)){var
P=c(x[28],M,O),Q=b(a[3],cU),R=c(a[12],Q,P);c(D[6],0,R);av(0);var
T=b(a[3],cV);c(D[6],0,T)}var
U=b(x[26],0);m[1]=0;q[1]=0;var
W=o(0,0,u),X=E(r),Y=[0,cD(k),X,W],Z=[0,j(q,b3),Y],_=b(h[16],Z);function
$(a){return b(h[2],a[1][1])}var
aa=c(e[22][14],$,p),ac=c(cW[13],_,aa),ae=b(x[26],0);if(b(e[3],F)){var
af=b(a[3],cX),ag=b(a[5],0),ah=b(a[3],cY),ai=b(e[3],q),aj=b(e[3],m),al=c(e[4],aj,ai),an=b(a[16],al),ap=b(a[3],cZ),aq=b(a[5],0),ar=b(a[3],c0),as=b(e[3],m),at=b(a[16],as),au=b(a[3],c1),ax=b(a[5],0),ay=c(x[28],U,ae),aA=b(a[3],c2),aB=c(a[12],aA,ay),aC=c(a[12],aB,ax),aD=c(a[12],aC,au),aE=c(a[12],aD,at),aF=c(a[12],aE,ar),aG=c(a[12],aF,aq),aH=c(a[12],aG,ap),aI=c(a[12],aH,an),aJ=c(a[12],aI,ah),aK=c(a[12],aJ,ag),aL=c(a[12],aK,af);c(D[6],0,aL)}var
aM=b(x[26],0),w=b(I[9],ac),aN=b(e[3],S)?b(az[46],w):b(az[43],w),aO=b(x[26],0);if(b(e[3],S)){var
aP=b(a[3],c3);c(D[6],0,aP)}if(b(e[3],F)){var
aQ=c(x[28],aM,aO),aR=b(a[3],c4),aS=c(a[12],aR,aQ);c(D[6],0,aS)}return aN}var
aA=b(J[67][7],cM);T(137,[0,v,ad,aA],"Rtauto_plugin__Refl_tauto");b(c5[9],ae);var
c6=0,c8=[0,[0,c7,function(a){return aA}],c6];V(c_[8],ae,c9,0,0,c8);T(140,[0,ae],"Rtauto_plugin__G_rtauto");return}
